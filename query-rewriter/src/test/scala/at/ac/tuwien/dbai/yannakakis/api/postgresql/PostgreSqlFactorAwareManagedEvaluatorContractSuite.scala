package at.ac.tuwien.dbai.yannakakis.api.postgresql

import java.io.{PrintWriter, StringReader}
import java.lang.reflect.{InvocationHandler, Method, Proxy}
import java.sql.{
  Connection,
  PreparedStatement,
  ResultSet,
  ResultSetMetaData,
  SQLException,
  SQLFeatureNotSupportedException,
  Statement
}
import java.util.logging.Logger
import javax.sql.DataSource

class PostgreSqlFactorAwareManagedEvaluatorContractSuite extends munit.FunSuite {
  import PostgreSqlFactorAwareContractTestSupport._

  test("managed factor evaluator uses one read-only transaction and preserves exact stage order") {
    val request = factorRequest(2)
    val plans = Vector(
      planJson("0", "10", "100", "8"),
      planJson("0.5", "2.25", "12.5", "16"),
      planJson("1", "3.5", "25", "32")
    )
    val state = new JdbcState(JdbcScenario(plans.map(Right(_))))
    val dataSource = new TrackingDataSource(state)
    val batch = evaluator(dataSource)
      .evaluateFactorAware(
        request,
        PostgreSqlFactorAwareSelectionLimits.default
      )
      .fold(problem => fail(problem.toString), identity)

    assertEquals(state.connectionRequests, 1)
    assertEquals(state.autoCommitValues, Vector(false, true))
    assertEquals(
      state.transactionStatements,
      Vector(PostgreSqlExplainCostEvaluator.TransactionStatement)
    )
    assertEquals(
      state.searchPathSchemaBindings,
      Vector("MiXeD Schema")
    )
    assertEquals(
      state.events,
      Vector(
        "transaction",
        "search-path",
        "version",
        "settings",
        "explain",
        "explain",
        "explain",
        "version",
        "settings"
      )
    )
    assertEquals(
      state.explainSql,
      (Vector(request.source.sql) ++ request.flattened.map(_._2.sql)).map(
        PostgreSqlExplainCostEvaluator.ExplainPrefix + _
      )
    )
    assertEquals(state.versionReadCount, 2)
    assertEquals(state.settingsReadCount, 2)
    assert(state.queryTimeouts.nonEmpty)
    assert(state.queryTimeouts.forall(timeout => timeout > 0 && timeout <= 30))
    assertEquals(dataSource.closeAttempts, 0)
    assertCleanedUp(state)

    assertEquals(batch.source.sourceStatementSha256, request.source.sourceStatementSha256)
    assertEquals(batch.source.estimate.planRows, BigDecimal(100))
    assertEquals(
      batch.observations.map(observation =>
        observation.physicalVariantId -> observation.stageId
      ),
      request.flattened.map { case (candidate, statement) =>
        candidate.physicalVariantId -> statement.stageId
      }
    )
    assertEquals(
      batch.observations.map(_.statementSha256),
      request.flattened.map(_._2.statementSha256)
    )
    assert(batch.observations.forall(_.estimate.provenance == batch.provenance))
  }

  test("managed plan parsing preserves float8-scale and precision-64 values exactly") {
    val request = factorRequest(1)
    val precision64 =
      "1234567890123456789012345678901234567890123456789012345678901234"
    val sourcePlan = planJson(
      "1e-1024",
      "1.7976931348623157e308",
      precision64,
      Long.MaxValue.toString
    )
    val stagePlan = planJson(
      "1e-1024",
      "1e308",
      "1e308",
      Long.MaxValue.toString
    )
    val state = new JdbcState(
      JdbcScenario(Vector(Right(sourcePlan), Right(stagePlan)))
    )
    val batch = evaluator(new TrackingDataSource(state))
      .evaluateFactorAware(
        request,
        PostgreSqlFactorAwareSelectionLimits.default
      )
      .fold(problem => fail(problem.toString), identity)

    assertEquals(batch.source.estimate.startupCost, BigDecimal("1e-1024"))
    assertEquals(
      batch.source.estimate.planTotalCost,
      BigDecimal("1.7976931348623157e308")
    )
    assertEquals(batch.source.estimate.planRows, BigDecimal(precision64))
    assertEquals(batch.source.estimate.planWidth, Long.MaxValue)
    assertEquals(batch.observations.head.estimate.planTotalCost, BigDecimal("1e308"))
    assertEquals(batch.observations.head.estimate.planRows, BigDecimal("1e308"))
    assertEquals(batch.observations.head.estimate.planWidth, Long.MaxValue)
    assertCleanedUp(state)
  }

  test("managed plan parsing rejects hostile factor numerics at the exact stage prefix") {
    val request = factorRequest(1)
    val sourcePlan = planJson("0", "1", "100", "8")
    val hostileRows = Vector(
      "12345678901234567890123456789012345678901234567890123456789012345",
      "1e-1025",
      "1e1025"
    )

    hostileRows.foreach { rows =>
      val state = new JdbcState(
        JdbcScenario(
          Vector(
            Right(sourcePlan),
            Right(planJson("0", "1", rows, "8"))
          )
        )
      )
      val result = evaluator(new TrackingDataSource(state))
        .evaluateFactorAware(
          request,
          PostgreSqlFactorAwareSelectionLimits.default
        )
      assertFactorFailure(
        result,
        PostgreSqlExplainCostFailureCode.PlanJsonMalformed,
        PostgreSqlExplainEvaluationStage.StageExplain,
        failedIndex = Some(0),
        completedCount = 0,
        sourcePresent = true,
        clue = rows
      )
      assertCleanedUp(state)
    }
  }

  test("request resource envelopes fail before acquiring a connection") {
    val request = factorRequest(2)
    val stageLengths = request.flattened.map(_._2.sql.length.toLong)
    val cases = Vector(
      "statement-count" -> limits(maxExplainStatements = 2L),
      "characters-per-probe" -> limits(
        maxCharactersPerProbe = stageLengths.max - 1L,
        maxDiagnosticCharactersPerCandidate = stageLengths.max,
        maxDiagnosticCharactersPerSession = stageLengths.sum
      ),
      "diagnostic-characters-per-session" -> limits(
        maxCharactersPerProbe = stageLengths.max,
        maxDiagnosticCharactersPerCandidate = stageLengths.max,
        maxDiagnosticCharactersPerSession = stageLengths.sum - 1L
      )
    )

    cases.foreach { case (label, activeLimits) =>
      val state = new JdbcState(JdbcScenario(Vector.empty))
      val result = evaluator(new TrackingDataSource(state))
        .evaluateFactorAware(request, activeLimits)
      assertFactorFailure(
        result,
        PostgreSqlExplainCostFailureCode.SessionResourceLimitExceeded,
        PostgreSqlExplainEvaluationStage.Session,
        failedIndex = None,
        completedCount = 0,
        sourcePresent = false,
        clue = label
      )
      assertEquals(state.connectionRequests, 0, label)
      assertEquals(state.events, Vector.empty, label)
    }
  }

  test("per-document and cumulative plan JSON bounds retain the exact stage prefix") {
    val request = factorRequest(1)
    val source = planJson("0", "1", "100", "8", padding = "s" * 7)
    val stage = planJson("0", "2", "20", "16", padding = "x" * 43)

    val perDocumentState = new JdbcState(
      JdbcScenario(Vector(Right(source), Right(stage)))
    )
    val perDocument = evaluator(new TrackingDataSource(perDocumentState))
      .evaluateFactorAware(
        request,
        limits(
          maxPlanJsonCharacters = stage.length.toLong - 1L,
          maxCumulativePlanJsonCharacters =
            source.length.toLong + stage.length.toLong
        )
      )
    assertFactorFailure(
      perDocument,
      PostgreSqlExplainCostFailureCode.PlanJsonOversized,
      PostgreSqlExplainEvaluationStage.StageExplain,
      failedIndex = Some(0),
      completedCount = 0,
      sourcePresent = true
    )
    assertCleanedUp(perDocumentState)

    val cumulativeState = new JdbcState(
      JdbcScenario(Vector(Right(source), Right(stage)))
    )
    val cumulative = evaluator(new TrackingDataSource(cumulativeState))
      .evaluateFactorAware(
        request,
        limits(
          maxPlanJsonCharacters = stage.length.toLong,
          maxCumulativePlanJsonCharacters =
            source.length.toLong + stage.length.toLong - 1L
        )
      )
    assertFactorFailure(
      cumulative,
      PostgreSqlExplainCostFailureCode.PlanJsonOversized,
      PostgreSqlExplainEvaluationStage.StageExplain,
      failedIndex = Some(0),
      completedCount = 0,
      sourcePresent = true
    )
    assertCleanedUp(cumulativeState)
  }

  test("unsafe later stage fails with the exact completed observation prefix") {
    val baseline = factorRequest(2)
    val unsafeSql = "DELETE FROM secret_table RETURNING *"
    val unsafeStage = PostgreSqlFactorAwareExplainStageStatement(
      at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.PostgreSqlStageId.FinalReconstruction,
      PostgreSqlCandidateEvaluationEvidence.renderedStatementSha256(unsafeSql),
      unsafeSql
    )
    val unsafeCandidate = baseline.candidates(1).copy(stages = Vector(unsafeStage))
    val request = baseline.copy(
      candidates = baseline.candidates.updated(1, unsafeCandidate)
    )
    val sourcePlan = planJson("0", "1", "100", "8")
    val firstStagePlan = planJson("0", "2", "100", "16")
    val state = new JdbcState(
      JdbcScenario(Vector(Right(sourcePlan), Right(firstStagePlan)))
    )
    val result = evaluator(new TrackingDataSource(state))
      .evaluateFactorAware(
        request,
        PostgreSqlFactorAwareSelectionLimits.default
      )

    assertFactorFailure(
      result,
      PostgreSqlExplainCostFailureCode.UnsafeRenderedStatement,
      PostgreSqlExplainEvaluationStage.StageExplain,
      failedIndex = Some(1),
      completedCount = 1,
      sourcePresent = true
    )
    val problem = result.left.toOption.get
    assertEquals(
      problem.completed.head.physicalVariantId,
      request.candidates.head.physicalVariantId
    )
    assertEquals(state.explainSql.size, 2)
    assert(!state.explainSql.exists(_.contains("secret_table")))
    assertCleanedUp(state)
  }

  test("statement-scheduling budget caps the next JDBC statement and still cleans up") {
    val state = new JdbcState(
      JdbcScenario(
        Vector.empty,
        autoCommitReadDelayMillis = 1100L
      )
    )
    val result = evaluator(new TrackingDataSource(state))
      .evaluateFactorAware(
        factorRequest(1),
        limits(
          statementTimeoutSeconds = 1,
          explainStatementBudgetSeconds = 1
        )
      )

    assertFactorFailure(
      result,
      PostgreSqlExplainCostFailureCode.StatementTimedOut,
      PostgreSqlExplainEvaluationStage.Session,
      failedIndex = None,
      completedCount = 0,
      sourcePresent = false
    )
    assertEquals(state.transactionStatements, Vector.empty)
    assertCleanedUp(state)
  }

  test("provenance drift and cleanup failure retain complete sanitized evidence") {
    val request = factorRequest(2)
    val plans = Vector(
      planJson("0", "1", "100", "8"),
      planJson("0", "2", "50", "16"),
      planJson("0", "3", "25", "32")
    )
    val changedState = new JdbcState(
      JdbcScenario(plans.map(Right(_)), versionValues = Vector(180004, 180005))
    )
    val changed = evaluator(new TrackingDataSource(changedState))
      .evaluateFactorAware(
        request,
        PostgreSqlFactorAwareSelectionLimits.default
      )
    assertFactorFailure(
      changed,
      PostgreSqlExplainCostFailureCode.ProvenanceChanged,
      PostgreSqlExplainEvaluationStage.Provenance,
      failedIndex = None,
      completedCount = 2,
      sourcePresent = true
    )
    assertCleanedUp(changedState)

    val cleanupState = new JdbcState(
      JdbcScenario(
        plans.map(Right(_)),
        failRollback = true,
        failRestoreAutoCommit = true,
        failConnectionClose = true
      )
    )
    val cleanup = evaluator(new TrackingDataSource(cleanupState))
      .evaluateFactorAware(
        request,
        PostgreSqlFactorAwareSelectionLimits.default
      )
    assertFactorFailure(
      cleanup,
      PostgreSqlExplainCostFailureCode.CleanupFailed,
      PostgreSqlExplainEvaluationStage.Session,
      failedIndex = None,
      completedCount = 2,
      sourcePresent = true
    )
    assertEquals(cleanupState.rollbackAttempts, 1)
    assertEquals(cleanupState.restoreAutoCommitAttempts, 1)
    assertEquals(cleanupState.connectionCloseAttempts, 1)
    assert(!cleanup.toString.contains("secret"))
  }

  private final case class JdbcScenario(
      planOutcomes: Vector[Either[Throwable, String]],
      versionValues: Vector[Int] = Vector(180004, 180004),
      failRollback: Boolean = false,
      failRestoreAutoCommit: Boolean = false,
      failConnectionClose: Boolean = false,
      autoCommitReadDelayMillis: Long = 0L
  )

  private final class JdbcState(val scenario: JdbcScenario) {
    var connectionRequests: Int = 0
    var connectionClosed: Boolean = false
    var autoCommit: Boolean = true
    var autoCommitValues: Vector[Boolean] = Vector.empty
    var restoreAutoCommitAttempts: Int = 0
    var rollbackAttempts: Int = 0
    var connectionCloseAttempts: Int = 0
    var transactionStatements: Vector[String] = Vector.empty
    var searchPathSchemaBindings: Vector[String] = Vector.empty
    var explainSql: Vector[String] = Vector.empty
    var queryTimeouts: Vector[Int] = Vector.empty
    var versionReadCount: Int = 0
    var settingsReadCount: Int = 0
    var planIndex: Int = 0
    var events: Vector[String] = Vector.empty

    lazy val connection: Connection = proxy(classOf[Connection]) {
      case ("isClosed", _) => java.lang.Boolean.valueOf(connectionClosed)
      case ("getAutoCommit", _) =>
        if (scenario.autoCommitReadDelayMillis > 0L)
          Thread.sleep(scenario.autoCommitReadDelayMillis)
        java.lang.Boolean.valueOf(autoCommit)
      case ("setAutoCommit", arguments) =>
        val value = arguments.head.asInstanceOf[java.lang.Boolean].booleanValue()
        autoCommitValues :+= value
        if (value) restoreAutoCommitAttempts += 1
        if (value && scenario.failRestoreAutoCommit)
          throw new SQLException("secret restore failure")
        autoCommit = value
        null
      case ("rollback", _) =>
        rollbackAttempts += 1
        if (scenario.failRollback)
          throw new SQLException("secret rollback failure")
        null
      case ("close", _) =>
        connectionCloseAttempts += 1
        connectionClosed = true
        if (scenario.failConnectionClose)
          throw new SQLException("secret close failure")
        null
      case ("createStatement", _) => statement
      case ("prepareStatement", arguments) =>
        preparedStatement(arguments.head.asInstanceOf[String])
    }

    private def statement: Statement = proxy(classOf[Statement]) {
      case ("setQueryTimeout", arguments) =>
        queryTimeouts :+= arguments.head.asInstanceOf[java.lang.Integer].intValue()
        null
      case ("setMaxRows", _) => null
      case ("execute", arguments) =>
        transactionStatements :+= arguments.head.asInstanceOf[String]
        events :+= "transaction"
        java.lang.Boolean.FALSE
      case ("executeQuery", arguments) =>
        val sql = arguments.head.asInstanceOf[String]
        if (sql == "SELECT current_setting('server_version_num')") {
          val index = math.min(versionReadCount, scenario.versionValues.size - 1)
          versionReadCount += 1
          events :+= "version"
          resultSet(Vector(Vector(scenario.versionValues(index).toString)))
        } else if (sql.startsWith(PostgreSqlExplainCostEvaluator.ExplainPrefix)) {
          explainSql :+= sql
          events :+= "explain"
          val outcome = scenario.planOutcomes.lift(planIndex).getOrElse(
            throw new SQLException("unexpected EXPLAIN")
          )
          planIndex += 1
          outcome match {
            case Left(problem) => throw problem
            case Right(json)   => resultSet(Vector(Vector(json)))
          }
        } else throw new SQLException("unexpected statement query")
      case ("close", _) => null
    }

    private def preparedStatement(sql: String): PreparedStatement = {
      var strings = Map.empty[Int, String]
      proxy(classOf[PreparedStatement]) {
        case ("setQueryTimeout", arguments) =>
          queryTimeouts :+= arguments.head.asInstanceOf[java.lang.Integer].intValue()
          null
        case ("setMaxRows", _) => null
        case ("setString", arguments) =>
          val index = arguments.head.asInstanceOf[java.lang.Integer].intValue()
          strings += index -> arguments(1).asInstanceOf[String]
          null
        case ("executeQuery", _) =>
          if (sql == PostgreSqlExplainCostEvaluator.SearchPathStatement) {
            searchPathSchemaBindings :+= strings.getOrElse(
              1,
              throw new SQLException("missing schema binding")
            )
            events :+= "search-path"
            resultSet(Vector(Vector("pg_catalog, configured, pg_temp")))
          } else {
            settingsReadCount += 1
            events :+= "settings"
            resultSet(
              PostgreSqlExplainCostProvenance.PlannerSettingNames.map(name =>
                Vector(name, "stable-value")
              )
            )
          }
        case ("close", _) => null
      }
    }

    private def resultSet(rows: Vector[Vector[String]]): ResultSet = {
      var position = -1
      proxy(classOf[ResultSet]) {
        case ("next", _) =>
          position += 1
          java.lang.Boolean.valueOf(position < rows.size)
        case ("getString", arguments) =>
          rows(position)(arguments.head.asInstanceOf[java.lang.Integer].intValue() - 1)
        case ("getCharacterStream", arguments) =>
          new StringReader(
            rows(position)(arguments.head.asInstanceOf[java.lang.Integer].intValue() - 1)
          )
        case ("getMetaData", _) =>
          proxy(classOf[ResultSetMetaData]) {
            case ("getColumnCount", _) =>
              java.lang.Integer.valueOf(rows.headOption.fold(0)(_.size))
          }
        case ("close", _) => null
      }
    }
  }

  private final class TrackingDataSource(state: JdbcState)
      extends DataSource
      with AutoCloseable {
    var closeAttempts: Int = 0

    override def getConnection: Connection = {
      state.connectionRequests += 1
      state.connection
    }
    override def getConnection(username: String, password: String): Connection =
      getConnection
    override def close(): Unit = closeAttempts += 1
    override def getLogWriter: PrintWriter = null
    override def setLogWriter(out: PrintWriter): Unit = ()
    override def setLoginTimeout(seconds: Int): Unit = ()
    override def getLoginTimeout: Int = 0
    override def getParentLogger: Logger =
      throw new SQLFeatureNotSupportedException
    override def unwrap[T](iface: Class[T]): T =
      throw new SQLException("not a wrapper")
    override def isWrapperFor(iface: Class[_]): Boolean = false
  }

  private def proxy[T](interface: Class[T])(
      partial: PartialFunction[(String, Vector[AnyRef]), AnyRef]
  ): T = {
    val handler = new InvocationHandler {
      override def invoke(
          proxyValue: Any,
          method: Method,
          rawArguments: Array[AnyRef]
      ): AnyRef = {
        val arguments = Option(rawArguments).fold(Vector.empty[AnyRef])(_.toVector)
        val key = method.getName -> arguments
        if (partial.isDefinedAt(key)) partial(key)
        else
          method.getName match {
            case "toString" => s"test-${interface.getSimpleName}"
            case "hashCode" =>
              java.lang.Integer.valueOf(System.identityHashCode(proxyValue))
            case "equals" =>
              java.lang.Boolean.valueOf(
                proxyValue.asInstanceOf[AnyRef] eq arguments.head
              )
            case _ => defaultValue(method.getReturnType)
          }
      }
    }
    Proxy
      .newProxyInstance(interface.getClassLoader, Array(interface), handler)
      .asInstanceOf[T]
  }

  private def defaultValue(returnType: Class[_]): AnyRef =
    if (!returnType.isPrimitive) null
    else if (returnType == java.lang.Boolean.TYPE) java.lang.Boolean.FALSE
    else if (returnType == java.lang.Integer.TYPE) java.lang.Integer.valueOf(0)
    else if (returnType == java.lang.Long.TYPE) java.lang.Long.valueOf(0L)
    else if (returnType == java.lang.Double.TYPE) java.lang.Double.valueOf(0.0d)
    else if (returnType == java.lang.Float.TYPE) java.lang.Float.valueOf(0.0f)
    else if (returnType == java.lang.Short.TYPE)
      java.lang.Short.valueOf(0.toShort)
    else if (returnType == java.lang.Byte.TYPE)
      java.lang.Byte.valueOf(0.toByte)
    else if (returnType == java.lang.Character.TYPE)
      java.lang.Character.valueOf(0.toChar)
    else null

  private def evaluator(dataSource: DataSource): PostgreSqlExplainCostEvaluator =
    PostgreSqlExplainCostEvaluator
      .forDataSource(dataSource, "MiXeD Schema")
      .fold(problem => fail(problem.message), identity)

  private def limits(
      maxCharactersPerProbe: Long =
        PostgreSqlFactorAwareSelectionLimits.default.maxCharactersPerProbe,
      maxDiagnosticCharactersPerCandidate: Long =
        PostgreSqlFactorAwareSelectionLimits.default.maxDiagnosticCharactersPerCandidate,
      maxExplainStatements: Long =
        PostgreSqlFactorAwareSelectionLimits.default.maxExplainStatementsPerSession,
      maxDiagnosticCharactersPerSession: Long =
        PostgreSqlFactorAwareSelectionLimits.default.maxDiagnosticCharactersPerSession,
      maxPlanJsonCharacters: Long =
        PostgreSqlFactorAwareSelectionLimits.default.maxPlanJsonCharacters,
      maxCumulativePlanJsonCharacters: Long =
        PostgreSqlFactorAwareSelectionLimits.default.maxCumulativePlanJsonCharacters,
      statementTimeoutSeconds: Int =
        PostgreSqlFactorAwareSelectionLimits.default.statementTimeoutSeconds,
      explainStatementBudgetSeconds: Int =
        PostgreSqlFactorAwareSelectionLimits.default.explainStatementBudgetSeconds
  ): PostgreSqlFactorAwareSelectionLimits =
    PostgreSqlFactorAwareSelectionLimits
      .create(
        maxCharactersPerProbe = maxCharactersPerProbe,
        maxDiagnosticCharactersPerCandidate =
          maxDiagnosticCharactersPerCandidate,
        maxExplainStatementsPerSession = maxExplainStatements,
        maxDiagnosticCharactersPerSession =
          maxDiagnosticCharactersPerSession,
        maxPlanJsonCharacters = maxPlanJsonCharacters,
        maxCumulativePlanJsonCharacters = maxCumulativePlanJsonCharacters,
        statementTimeoutSeconds = statementTimeoutSeconds,
        explainStatementBudgetSeconds = explainStatementBudgetSeconds
      )
      .fold(problems => fail(problems.map(_.message).mkString("; ")), identity)

  private def planJson(
      startup: String,
      total: String,
      rows: String,
      width: String,
      padding: String = ""
  ): String = {
    val paddingField =
      if (padding.isEmpty) "" else s",\"Padding\":\"$padding\""
    s"[{\"Plan\":{\"Node Type\":\"Result\",\"Startup Cost\":$startup," +
      s"\"Total Cost\":$total,\"Plan Rows\":$rows,\"Plan Width\":$width" +
      s"$paddingField},\"Settings\":{}}]"
  }

  private def assertFactorFailure(
      result: Either[
        PostgreSqlFactorAwareExplainFailure,
        PostgreSqlFactorAwareExplainBatch
      ],
      code: PostgreSqlExplainCostFailureCode,
      stage: PostgreSqlExplainEvaluationStage,
      failedIndex: Option[Int],
      completedCount: Int,
      sourcePresent: Boolean,
      clue: String = ""
  ): Unit = result match {
    case Left(problem) =>
      assertEquals(problem.code, code, clue)
      assertEquals(problem.stage, stage, clue)
      assertEquals(problem.failedStatementIndex, failedIndex, clue)
      assertEquals(problem.completed.size, completedCount, clue)
      assertEquals(problem.source.nonEmpty, sourcePresent, clue)
    case Right(batch) =>
      fail(s"expected $code failure, received $batch ($clue)")
  }

  private def assertCleanedUp(state: JdbcState): Unit = {
    assertEquals(state.rollbackAttempts, 1)
    assertEquals(state.restoreAutoCommitAttempts, 1)
    assertEquals(state.connectionCloseAttempts, 1)
    assert(state.connectionClosed)
  }
}

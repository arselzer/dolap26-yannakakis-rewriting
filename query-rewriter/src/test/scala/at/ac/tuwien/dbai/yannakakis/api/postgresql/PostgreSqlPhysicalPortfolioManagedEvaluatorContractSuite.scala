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

class PostgreSqlPhysicalPortfolioManagedEvaluatorContractSuite
    extends munit.FunSuite {
  import PostgreSqlPhysicalPortfolioContractTestSupport._

  test("managed portfolio evaluation uses one transaction and exact flattened workload order") {
    val activeRequest = request(1)
    val plans = Vector(
      planJson("0", "10", "100", "8")
    ) ++ activeRequest.flattened.indices.map(index =>
      planJson("0.5", (index + 1).toString, (index + 2).toString, "16")
    )
    val state = new JdbcState(JdbcScenario(plans.map(Right(_))))
    val dataSource = new TrackingDataSource(state)
    val batch = evaluator(dataSource)
      .evaluatePhysicalPortfolio(
        activeRequest,
        PostgreSqlPhysicalPortfolioEvaluationLimits.default
      )
      .fold(problem => fail(problem.toString), identity)

    assertEquals(state.connectionRequests, 1)
    assertEquals(state.autoCommitValues, Vector(false, true))
    assertEquals(
      state.transactionStatements,
      Vector(PostgreSqlExplainCostEvaluator.TransactionStatement)
    )
    assertEquals(state.searchPathSchemaBindings, Vector("MiXeD Schema"))
    assertEquals(
      state.explainSql,
      (Vector(activeRequest.source.sql) ++ activeRequest.flattened.map(_._2.sql))
        .map(PostgreSqlExplainCostEvaluator.ExplainPrefix + _)
    )
    assertEquals(state.versionReadCount, 2)
    assertEquals(state.settingsReadCount, 2)
    assertEquals(
      batch.observations.map(observation =>
        (observation.workloadId, observation.representativePhysicalVariantId, observation.stageId)
      ),
      activeRequest.flattened.map { case (workload, stage) =>
        (
          workload.group.descriptor.workloadId,
          workload.group.representative.physicalVariantId,
          stage.stageId
        )
      }
    )
    assert(batch.observations.forall(_.estimate.provenance == batch.provenance))
    assertEquals(dataSource.closeAttempts, 0)
    assertCleanedUp(state)
  }

  test("PostgreSQL 11 fails at version preflight before settings, source, or stage EXPLAIN") {
    val activeRequest = request(1)
    val state = new JdbcState(
      JdbcScenario(Vector.empty, versionValues = Vector(119999))
    )
    val result = evaluator(new TrackingDataSource(state))
      .evaluatePhysicalPortfolio(
        activeRequest,
        PostgreSqlPhysicalPortfolioEvaluationLimits.default
      )
    assertPortfolioFailure(
      result,
      PostgreSqlPhysicalPortfolioExplainFailureCode.UnsupportedServerVersion,
      PostgreSqlPhysicalPortfolioExplainStage.VersionPreflight,
      None,
      completedCount = 0,
      sourcePresent = false
    )
    val failure = result.left.toOption.get
    assertEquals(failure.observedServerVersionNum, Some(119999))
    assertEquals(failure.requiredServerVersionNum, Some(120000))
    assertEquals(state.events, Vector("transaction", "search-path", "version"))
    assertEquals(state.settingsReadCount, 0)
    assertEquals(state.explainSql, Vector.empty)
    assertCleanedUp(state)
  }

  test("request envelope failures occur before acquiring a connection") {
    val activeRequest = request(1)
    val activeLimits = limits(
      maxExplainStatementsPerSession = activeRequest.flattened.size.toLong
    )
    val state = new JdbcState(JdbcScenario(Vector.empty))
    val result = evaluator(new TrackingDataSource(state))
      .evaluatePhysicalPortfolio(activeRequest, activeLimits)
    assertPortfolioFailure(
      result,
      PostgreSqlPhysicalPortfolioExplainFailureCode.SessionResourceLimitExceeded,
      PostgreSqlPhysicalPortfolioExplainStage.Session,
      None,
      completedCount = 0,
      sourcePresent = false
    )
    assertEquals(state.connectionRequests, 0)
    assertEquals(state.events, Vector.empty)
  }

  test("hostile numeric estimates fail at the exact flattened stage prefix") {
    val activeRequest = request(1)
    val precision65 =
      "12345678901234567890123456789012345678901234567890123456789012345"
    val plans = Vector(
      planJson("0", "10", "100", "8"),
      planJson("0", "1", "10", "2"),
      planJson("0", "2", "20", "4"),
      planJson("0", "3", precision65, "8")
    )
    val state = new JdbcState(JdbcScenario(plans.map(Right(_))))
    val result = evaluator(new TrackingDataSource(state))
      .evaluatePhysicalPortfolio(
        activeRequest,
        PostgreSqlPhysicalPortfolioEvaluationLimits.default
      )
    assertPortfolioFailure(
      result,
      PostgreSqlPhysicalPortfolioExplainFailureCode.PlanJsonMalformed,
      PostgreSqlPhysicalPortfolioExplainStage.StageExplain,
      Some(2),
      completedCount = 2,
      sourcePresent = true
    )
    assertEquals(state.explainSql.size, 4)
    assertCleanedUp(state)
  }

  test("per-document and cumulative JSON caps retain the exact stage prefix") {
    val activeRequest = request(1)
    val source = planJson("0", "1", "100", "8", padding = "s" * 7)
    val stage = planJson("0", "2", "20", "16", padding = "x" * 43)

    val perDocumentState = new JdbcState(
      JdbcScenario(Vector(Right(source), Right(stage)))
    )
    val perDocument = evaluator(new TrackingDataSource(perDocumentState))
      .evaluatePhysicalPortfolio(
        activeRequest,
        limits(
          maxPlanJsonCharacters = stage.length.toLong - 1L,
          maxCumulativePlanJsonCharacters =
            source.length.toLong + stage.length.toLong
        )
      )
    assertPortfolioFailure(
      perDocument,
      PostgreSqlPhysicalPortfolioExplainFailureCode.PlanJsonOversized,
      PostgreSqlPhysicalPortfolioExplainStage.StageExplain,
      Some(0),
      completedCount = 0,
      sourcePresent = true
    )
    assertCleanedUp(perDocumentState)

    val cumulativeState = new JdbcState(
      JdbcScenario(Vector(Right(source), Right(stage)))
    )
    val cumulative = evaluator(new TrackingDataSource(cumulativeState))
      .evaluatePhysicalPortfolio(
        activeRequest,
        limits(
          maxPlanJsonCharacters = stage.length.toLong,
          maxCumulativePlanJsonCharacters =
            source.length.toLong + stage.length.toLong - 1L
        )
      )
    assertPortfolioFailure(
      cumulative,
      PostgreSqlPhysicalPortfolioExplainFailureCode.PlanJsonOversized,
      PostgreSqlPhysicalPortfolioExplainStage.StageExplain,
      Some(0),
      completedCount = 0,
      sourcePresent = true
    )
    assertCleanedUp(cumulativeState)
  }

  test("final provenance drift and cleanup failure preserve the complete sanitized prefix") {
    val activeRequest = request(1)
    val plans = Vector(
      planJson("0", "10", "100", "8")
    ) ++ activeRequest.flattened.indices.map(index =>
      planJson("0", (index + 1).toString, (index + 2).toString, "16")
    )
    val changedState = new JdbcState(
      JdbcScenario(plans.map(Right(_)), versionValues = Vector(180004, 180005))
    )
    val changed = evaluator(new TrackingDataSource(changedState))
      .evaluatePhysicalPortfolio(
        activeRequest,
        PostgreSqlPhysicalPortfolioEvaluationLimits.default
      )
    assertPortfolioFailure(
      changed,
      PostgreSqlPhysicalPortfolioExplainFailureCode.ProvenanceChanged,
      PostgreSqlPhysicalPortfolioExplainStage.FinalProvenance,
      None,
      completedCount = activeRequest.flattened.size,
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
      .evaluatePhysicalPortfolio(
        activeRequest,
        PostgreSqlPhysicalPortfolioEvaluationLimits.default
      )
    assertPortfolioFailure(
      cleanup,
      PostgreSqlPhysicalPortfolioExplainFailureCode.CleanupFailed,
      PostgreSqlPhysicalPortfolioExplainStage.Session,
      None,
      completedCount = activeRequest.flattened.size,
      sourcePresent = true
    )
    assertEquals(cleanupState.rollbackAttempts, 1)
    assertEquals(cleanupState.restoreAutoCommitAttempts, 1)
    assertEquals(cleanupState.connectionCloseAttempts, 1)
  }

  private final case class JdbcScenario(
      planOutcomes: Vector[Either[Throwable, String]],
      versionValues: Vector[Int] = Vector(180004, 180004),
      failRollback: Boolean = false,
      failRestoreAutoCommit: Boolean = false,
      failConnectionClose: Boolean = false
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
      case ("getAutoCommit", _) => java.lang.Boolean.valueOf(autoCommit)
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
      maxExplainStatementsPerSession: Long =
        PostgreSqlPhysicalPortfolioEvaluationLimits.default.maxExplainStatementsPerSession,
      maxPlanJsonCharacters: Long =
        PostgreSqlPhysicalPortfolioEvaluationLimits.default.maxPlanJsonCharacters,
      maxCumulativePlanJsonCharacters: Long =
        PostgreSqlPhysicalPortfolioEvaluationLimits.default.maxCumulativePlanJsonCharacters
  ): PostgreSqlPhysicalPortfolioEvaluationLimits =
    PostgreSqlPhysicalPortfolioEvaluationLimits
      .create(
        maxExplainStatementsPerSession = maxExplainStatementsPerSession,
        maxPlanJsonCharacters = maxPlanJsonCharacters,
        maxCumulativePlanJsonCharacters = maxCumulativePlanJsonCharacters
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

  private def assertPortfolioFailure(
      result: Either[
        PostgreSqlPhysicalPortfolioExplainFailure,
        PostgreSqlPhysicalPortfolioExplainBatch
      ],
      code: PostgreSqlPhysicalPortfolioExplainFailureCode,
      stage: PostgreSqlPhysicalPortfolioExplainStage,
      failedIndex: Option[Int],
      completedCount: Int,
      sourcePresent: Boolean
  ): Unit = result match {
    case Left(problem) =>
      assertEquals(problem.code, code)
      assertEquals(problem.stage, stage)
      assertEquals(problem.failedStatementIndex, failedIndex)
      assertEquals(problem.completed.size, completedCount)
      assertEquals(problem.source.nonEmpty, sourcePresent)
    case Right(batch) => fail(s"expected $code failure, received $batch")
  }

  private def assertCleanedUp(state: JdbcState): Unit = {
    assertEquals(state.rollbackAttempts, 1)
    assertEquals(state.restoreAutoCommitAttempts, 1)
    assertEquals(state.connectionCloseAttempts, 1)
    assert(state.connectionClosed)
  }
}

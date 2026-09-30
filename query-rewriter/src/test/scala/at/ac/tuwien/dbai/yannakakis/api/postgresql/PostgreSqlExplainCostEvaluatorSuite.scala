package at.ac.tuwien.dbai.yannakakis.api.postgresql

import java.io.{PrintWriter, StringReader}
import java.lang.reflect.{InvocationHandler, Method, Proxy}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.sql.{
  Connection,
  PreparedStatement,
  ResultSet,
  ResultSetMetaData,
  SQLException,
  SQLFeatureNotSupportedException,
  SQLTimeoutException,
  Statement
}
import java.util.logging.Logger
import javax.sql.DataSource

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidatePlanId
import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode

class PostgreSqlExplainCostEvaluatorSuite extends munit.FunSuite {
  test("managed evaluator uses one read-only transaction, one connection, and preserves root evidence") {
    val plans = Vector(
      planJson("0.5", "3.25", "77", "16"),
      planJson("1.25", "9.75", "120.5", "44"),
      planJson("0", "4.5", "2", "8")
    )
    val state = new JdbcState(JdbcScenario(plans.map(Right(_))))
    val dataSource = new TrackingDataSource(state)
    val result = evaluator(dataSource).evaluate(
      request(2),
      PostgreSqlExplainSelectionLimits.default
    )
    val batch = result.fold(problem => fail(problem.toString), identity)

    assertEquals(state.connectionRequests, 1)
    assertEquals(state.autoCommitValues, Vector(false, true))
    assertEquals(
      state.transactionStatements,
      Vector(PostgreSqlExplainCostEvaluator.TransactionStatement)
    )
    assertEquals(
      state.preparedSql.headOption,
      Some(PostgreSqlExplainCostEvaluator.SearchPathStatement)
    )
    assertEquals(
      PostgreSqlExplainCostEvaluator.SearchPathStatement,
      "SELECT pg_catalog.set_config(" +
        "'search_path', 'pg_catalog, ' || pg_catalog.quote_ident(?) || ', pg_temp', true)"
    )
    assertEquals(state.searchPathSchemaBindings, Vector("MiXeD Schema"))
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
    assertEquals(state.rollbackAttempts, 1)
    assertEquals(state.connectionCloseAttempts, 1)
    assert(state.connectionClosed)
    assertEquals(dataSource.closeAttempts, 0)
    assertEquals(state.explainSql.size, 3)
    assertEquals(
      state.explainSql.head,
      PostgreSqlExplainCostEvaluator.ExplainPrefix + sourceSql
    )
    assert(state.explainSql.tail.forall(
      _.startsWith(PostgreSqlExplainCostEvaluator.ExplainPrefix + "WITH\n")
    ))
    assertEquals(
      state.queryTimeouts.distinct,
      Vector(PostgreSqlExplainSelectionLimits.default.statementTimeoutSeconds)
    )
    assertEquals(state.versionReadCount, 2)
    assertEquals(state.settingsReadCount, 2)

    assertEquals(batch.source.sourceStatementSha256, sourceStatement.sourceStatementSha256)
    assertEquals(batch.source.estimate.planRows, BigDecimal("77"))
    assertEquals(batch.observations.map(_.structuralRank), Vector(1, 2))
    assertEquals(batch.observations.map(_.estimate.provenance).distinct, Vector(batch.provenance))
    val first = batch.observations.head.estimate
    assertEquals(first.startupCost, BigDecimal("1.25"))
    assertEquals(first.planTotalCost, BigDecimal("9.75"))
    assertEquals(first.planRows, BigDecimal("120.5"))
    assertEquals(first.planWidth, 44L)
    assertEquals(first.canonicalStartupCost, "1.25")
    assertEquals(first.canonicalPlanTotalCost, "9.75")
    assertEquals(first.canonicalPlanRows, "120.5")
  }

  test("plan JSON per-document and cumulative bounds are exact") {
    val plans = Vector(
      planJson("0", "0.5", "3", "1", padding = "s" * 17),
      planJson("0", "1", "1", "1", padding = "x" * 31),
      planJson("0", "2", "2", "2", padding = "y" * 47)
    )
    val perPlan = plans.map(_.length.toLong).max
    val cumulative = plans.map(_.length.toLong).sum

    val exactLimits = explainLimits(perPlan, cumulative)
    val exactState = new JdbcState(JdbcScenario(plans.map(Right(_))))
    val exact = evaluator(new TrackingDataSource(exactState))
      .evaluate(request(2), exactLimits)
    assert(exact.isRight, clues(exact))
    assertCleanedUp(exactState)

    val perPlanState = new JdbcState(
      JdbcScenario(Vector(Right(plans.head), Right(plans(1))))
    )
    val perPlanFailure = evaluator(new TrackingDataSource(perPlanState))
      .evaluate(
        request(1),
        explainLimits(plans(1).length - 1L, plans.head.length + plans(1).length - 1L)
      )
    assertFailure(
      perPlanFailure,
      PostgreSqlExplainCostFailureCode.PlanJsonOversized,
      PostgreSqlExplainEvaluationStage.CandidateExplain,
      failedCandidateIndex = Some(0),
      completedRanks = Vector.empty,
      sourcePresent = true
    )
    assertCleanedUp(perPlanState)

    val cumulativeState = new JdbcState(JdbcScenario(plans.map(Right(_))))
    val cumulativeFailure = evaluator(new TrackingDataSource(cumulativeState))
      .evaluate(request(2), explainLimits(perPlan, cumulative - 1L))
    assertFailure(
      cumulativeFailure,
      PostgreSqlExplainCostFailureCode.PlanJsonOversized,
      PostgreSqlExplainEvaluationStage.CandidateExplain,
      failedCandidateIndex = Some(1),
      completedRanks = Vector(1),
      sourcePresent = true
    )
    assertCleanedUp(cumulativeState)
  }

  test("timeouts, malformed roots, missing fields, non-finite values, and negatives fail closed and clean up") {
    val malformedCode = PostgreSqlExplainCostFailureCode.PlanJsonMalformed
    val cases = Vector[(String, Either[Throwable, String], PostgreSqlExplainCostFailureCode)](
      (
        "timeout",
        Left(new SQLTimeoutException("secret timeout SQL")),
        PostgreSqlExplainCostFailureCode.StatementTimedOut
      ),
      (
        "cancelled",
        Left(new SQLException("secret cancelled SQL", "57014")),
        PostgreSqlExplainCostFailureCode.StatementTimedOut
      ),
      ("malformed", Right("not-json"), malformedCode),
      (
        "duplicate-root-field",
        Right(
          """[{"Plan":{"Startup Cost":0,"Total Cost":1,"Total Cost":2,"Plan Rows":2,"Plan Width":4}}]"""
        ),
        malformedCode
      ),
      (
        "trailing-json",
        Right(planJson("0", "1", "2", "4") + "{}"),
        malformedCode
      ),
      (
        "missing-root-field",
        Right("""[{"Plan":{"Startup Cost":0,"Total Cost":1,"Plan Rows":2}}]"""),
        malformedCode
      ),
      ("non-finite", Right(planJson("0", "NaN", "2", "4")), malformedCode),
      ("negative-startup", Right(planJson("-1", "1", "2", "4")), malformedCode),
      ("negative-total", Right(planJson("0", "-1", "2", "4")), malformedCode),
      ("negative-rows", Right(planJson("0", "1", "-2", "4")), malformedCode),
      ("negative-width", Right(planJson("0", "1", "2", "-4")), malformedCode),
      (
        "oversized-width",
        Right(planJson("0", "1", "2", "9223372036854775808")),
        malformedCode
      ),
      (
        "execution-evidence",
        Right(
          """[{"Plan":{"Startup Cost":0,"Total Cost":1,"Plan Rows":2,"Plan Width":4,"Actual Rows":0}}]"""
        ),
        malformedCode
      )
    )

    cases.foreach { case (label, outcome, expectedCode) =>
      val state = new JdbcState(JdbcScenario(Vector(outcome)))
      val dataSource = new TrackingDataSource(state)
      val result = evaluator(dataSource).evaluate(
        request(1),
        PostgreSqlExplainSelectionLimits.default
      )
      assertFailure(
        result,
        expectedCode,
        PostgreSqlExplainEvaluationStage.SourceExplain,
        None,
        Vector.empty,
        sourcePresent = false,
        clue = label
      )
      assertCleanedUp(state, label)
      assertEquals(dataSource.closeAttempts, 0, label)
      assert(!result.toString.contains("secret"), label)
      assert(!result.toString.contains("SELECT"), label)
      assert(!result.toString.contains(classOf[SQLTimeoutException].getName), label)
    }
  }

  test("root numeric evidence is lossless above 2^53 and across decimal exponents") {
    val source = planJson(
      "0.000000000000000000000000000123",
      "1e400",
      "9007199254740993.125",
      Long.MaxValue.toString
    )
    val candidate = planJson(
      "1.2300",
      "100e2147483647",
      "9007199254740995.875",
      "64"
    )
    val state = new JdbcState(
      JdbcScenario(Vector(Right(source), Right(candidate)))
    )
    val batch = evaluator(new TrackingDataSource(state))
      .evaluate(request(1), PostgreSqlExplainSelectionLimits.default)
      .fold(problem => fail(problem.toString), identity)

    assertEquals(
      batch.source.estimate.startupCost,
      BigDecimal("0.000000000000000000000000000123")
    )
    assertEquals(batch.source.estimate.planTotalCost, BigDecimal("1e400"))
    assertEquals(
      batch.source.estimate.planRows,
      BigDecimal("9007199254740993.125")
    )
    assertEquals(batch.source.estimate.planWidth, Long.MaxValue)
    assertEquals(
      batch.observations.head.estimate.planTotalCost,
      BigDecimal("100e2147483647")
    )
    assertEquals(
      batch.observations.head.estimate.canonicalPlanTotalCost,
      "1.00E+2147483649"
    )
    assertCleanedUp(state)
  }

  test("unsafe rendered SQL is rejected without planning and still cleans up") {
    val state = new JdbcState(
      JdbcScenario(Vector(Right(planJson("0", "1", "1", "1"))))
    )
    val dataSource = new TrackingDataSource(state)
    val unsafe = requestWithSql("WITH\n\"x\" AS (DELETE FROM t RETURNING *) SELECT * FROM \"x\";")
    val result = evaluator(dataSource).evaluate(
      PostgreSqlExplainCostBatchRequest(sourceStatement, Vector(unsafe)),
      PostgreSqlExplainSelectionLimits.default
    )
    assertFailure(
      result,
      PostgreSqlExplainCostFailureCode.UnsafeRenderedStatement,
      PostgreSqlExplainEvaluationStage.CandidateExplain,
      Some(0),
      Vector.empty,
      sourcePresent = true
    )
    assertEquals(
      state.explainSql,
      Vector(PostgreSqlExplainCostEvaluator.ExplainPrefix + sourceSql)
    )
    assertCleanedUp(state)
  }

  test("lexical gate handles comments and PostgreSQL strings while rejecting writes and statement stacking") {
    val benchmarkPath = Paths.get(
      "..",
      "benchmarks",
      "cyclic-postgresql",
      "queries",
      "tpch-q5-derived-enum-core.sql"
    )
    assert(Files.isRegularFile(benchmarkPath), benchmarkPath.toAbsolutePath.toString)
    val benchmarkSource =
      new String(Files.readAllBytes(benchmarkPath), StandardCharsets.UTF_8)
    assert(benchmarkSource.startsWith("/*"))
    assert(
      PostgreSqlExplainSafeStatement.isReadOnlySelect(benchmarkSource),
      benchmarkPath.toAbsolutePath.toString
    )

    val safe = Vector(
      "/* leading */ SELECT 1",
      "/* outer /* nested delete */ still */ WITH x AS (SELECT 1) SELECT * FROM x;",
      "-- leading update\nSELECT E'delete\\nvalue'",
      "SELECT $$update; drop table x$$",
      "SELECT $tag$insert into hidden values (1)$tag$",
      "SELECT 'delete''update' AS text",
      "SELECT \"into\" FROM \"delete\""
    )
    val unsafe = Vector(
      "WITH x AS (DELETE FROM t RETURNING *) SELECT * FROM x",
      "WITH x AS (UPDATE t SET a = 1 RETURNING *) SELECT * FROM x",
      "SELECT * INTO temporary t FROM source",
      "SELECT 1; SELECT 2",
      "SELECT 1; /* comment */ UPDATE t SET a = 2",
      "SELECT 'unterminated",
      "SELECT $$unterminated",
      "VACUUM t"
    )
    safe.foreach(sql => assert(PostgreSqlExplainSafeStatement.isReadOnlySelect(sql), sql))
    unsafe.foreach(sql => assert(!PostgreSqlExplainSafeStatement.isReadOnlySelect(sql), sql))
  }

  test("provenance must remain identical across the whole batch") {
    val state = new JdbcState(
      JdbcScenario(
        planOutcomes = Vector(
          Right(planJson("0", "1", "2", "4")),
          Right(planJson("0", "2", "3", "4"))
        ),
        versionValues = Vector(180004, 180005)
      )
    )
    val result = evaluator(new TrackingDataSource(state)).evaluate(
      request(1),
      PostgreSqlExplainSelectionLimits.default
    )
    assertFailure(
      result,
      PostgreSqlExplainCostFailureCode.ProvenanceChanged,
      PostgreSqlExplainEvaluationStage.Provenance,
      failedCandidateIndex = None,
      completedRanks = Vector(1),
      sourcePresent = true
    )
    assertCleanedUp(state)
  }

  test("cleanup failure overrides success without skipping rollback, restore, or close") {
    val state = new JdbcState(
      JdbcScenario(
        Vector(
          Right(planJson("0", "1", "2", "4")),
          Right(planJson("0", "2", "3", "4"))
        ),
        failRollback = true,
        failRestoreAutoCommit = true,
        failConnectionClose = true
      )
    )
    val result = evaluator(new TrackingDataSource(state)).evaluate(
      request(1),
      PostgreSqlExplainSelectionLimits.default
    )
    assertFailure(
      result,
      PostgreSqlExplainCostFailureCode.CleanupFailed,
      PostgreSqlExplainEvaluationStage.Session,
      failedCandidateIndex = None,
      completedRanks = Vector(1),
      sourcePresent = true
    )
    assertEquals(state.rollbackAttempts, 1)
    assertEquals(state.restoreAutoCommitAttempts, 1)
    assertEquals(state.connectionCloseAttempts, 1)
  }

  test("limit construction rejects non-positive and inconsistent envelopes") {
    val invalid = PostgreSqlExplainSelectionLimits.create(
      statementTimeoutSeconds = 0,
      maxPlanJsonCharacters = -1,
      maxCumulativePlanJsonCharacters = 0
    )
    val invalidCodes = invalid.left.toOption
      .getOrElse(fail("invalid limits were accepted"))
      .map(_.code)
    assertEquals(
      invalidCodes,
      Vector(
        "explain-limit.invalid.statement-timeout-seconds",
        "explain-limit.invalid.plan-json-characters",
        "explain-limit.invalid.cumulative-plan-json-characters"
      )
    )

    val inconsistent = PostgreSqlExplainSelectionLimits.create(
      maxPlanJsonCharacters = 100,
      maxCumulativePlanJsonCharacters = 99
    )
    assertEquals(
      inconsistent.left.toOption
        .getOrElse(fail("inconsistent limits were accepted"))
        .map(_.code),
      Vector("explain-limit.inconsistent.plan-json-characters")
    )
  }

  test("missing data source is a typed configuration failure") {
    val failure = PostgreSqlExplainCostEvaluator
      .forDataSource(null, "public")
      .left
      .toOption
      .getOrElse(fail("null data source was accepted"))
    assertEquals(failure.code, "explain-evaluator.configuration.data-source")
    assert(!failure.toString.contains("null"))

    val missingSchema = PostgreSqlExplainCostEvaluator
      .forDataSource(new TrackingDataSource(new JdbcState(JdbcScenario(Vector.empty))), " ")
      .left
      .toOption
      .getOrElse(fail("blank physical schema was accepted"))
    assertEquals(missingSchema.code, "explain-evaluator.configuration.schema")
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
    var preparedSql: Vector[String] = Vector.empty
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
        if (scenario.failRollback) throw new SQLException("secret rollback failure")
        null
      case ("close", _) =>
        connectionCloseAttempts += 1
        connectionClosed = true
        if (scenario.failConnectionClose)
          throw new SQLException("secret close failure")
        null
      case ("createStatement", _) => statement
      case ("prepareStatement", arguments) =>
        val sql = arguments.head.asInstanceOf[String]
        preparedSql :+= sql
        preparedStatement(sql)
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
            case "hashCode" => java.lang.Integer.valueOf(System.identityHashCode(proxyValue))
            case "equals" =>
              java.lang.Boolean.valueOf(
                proxyValue.asInstanceOf[AnyRef] eq arguments.head
              )
            case _           => defaultValue(method.getReturnType)
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
    else if (returnType == java.lang.Short.TYPE) java.lang.Short.valueOf(0.toShort)
    else if (returnType == java.lang.Byte.TYPE) java.lang.Byte.valueOf(0.toByte)
    else if (returnType == java.lang.Character.TYPE) java.lang.Character.valueOf(0.toChar)
    else null

  private def evaluator(
      dataSource: DataSource
  ): PostgreSqlExplainCostEvaluator =
    PostgreSqlExplainCostEvaluator
      .forDataSource(dataSource, "MiXeD Schema")
      .fold(problem => fail(problem.message), identity)

  private val sourceSql = "SELECT 42 AS source_value"

  private def sourceStatement: PostgreSqlExplainSourceStatement =
    PostgreSqlExplainSourceStatement(
      PostgreSqlCandidateEvaluationEvidence.renderedStatementSha256(sourceSql),
      sourceSql
    )

  private def request(count: Int): PostgreSqlExplainCostBatchRequest =
    PostgreSqlExplainCostBatchRequest(
      sourceStatement,
      (1 to count).toVector.map(index =>
        requestWithSql(
          s"WITH\n\"q_$index\" AS (SELECT $index) SELECT * FROM \"q_$index\";",
          index
        )
      )
    )

  private def requestWithSql(
      sql: String,
      rank: Int = 1
  ): PostgreSqlExplainCandidateStatement = {
    val id = candidateId(rank)
    val mode = YannakakisReductionMode.NoReduction
    PostgreSqlExplainCandidateStatement(
      id,
      rank,
      mode,
      PostgreSqlPhysicalVariantId.from(id, mode),
      PostgreSqlCandidateEvaluationEvidence.renderedStatementSha256(sql),
      sql
    )
  }

  private def candidateId(rank: Int): CandidatePlanId =
    CandidatePlanId
      .parse("sha256:" + f"$rank%064x")
      .fold(problem => fail(problem.message), identity)

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

  private def explainLimits(
      perPlan: Long,
      cumulative: Long
  ): PostgreSqlExplainSelectionLimits =
    PostgreSqlExplainSelectionLimits
      .create(
        statementTimeoutSeconds = 3,
        maxPlanJsonCharacters = perPlan,
        maxCumulativePlanJsonCharacters = cumulative
      )
      .fold(problems => fail(problems.map(_.message).mkString("; ")), identity)

  private def assertFailure(
      result: Either[PostgreSqlExplainCostEvaluationFailure, PostgreSqlExplainCostBatch],
      code: PostgreSqlExplainCostFailureCode,
      stage: PostgreSqlExplainEvaluationStage,
      failedCandidateIndex: Option[Int],
      completedRanks: Vector[Int],
      sourcePresent: Boolean,
      clue: String = ""
  ): Unit = result match {
    case Left(problem) =>
      assertEquals(problem.code, code, clue)
      assertEquals(problem.stage, stage, clue)
      assertEquals(problem.failedCandidateIndex, failedCandidateIndex, clue)
      assertEquals(problem.source.nonEmpty, sourcePresent, clue)
      assertEquals(problem.completed.map(_.structuralRank), completedRanks, clue)
    case Right(batch) =>
      fail(s"expected $code failure, received $batch (clue: $clue)")
  }

  private def assertCleanedUp(state: JdbcState, clue: String = ""): Unit = {
    assertEquals(state.rollbackAttempts, 1, clue)
    assertEquals(state.restoreAutoCommitAttempts, 1, clue)
    assertEquals(state.connectionCloseAttempts, 1, clue)
    assert(state.connectionClosed, clue)
  }
}

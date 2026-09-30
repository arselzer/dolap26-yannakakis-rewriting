package at.ac.tuwien.dbai.yannakakis.integration

import java.io.PrintWriter
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.sql.{Connection, DriverManager, SQLException}
import java.util.UUID
import java.util.concurrent.atomic.AtomicInteger
import java.util.logging.Logger
import javax.sql.DataSource

import scala.util.Using

import at.ac.tuwien.dbai.yannakakis.api.postgresql.{
  PostgreSqlEnumerationCampaignV2PlanAction,
  PostgreSqlEnumerationCampaignV2PlanActionKind,
  PostgreSqlEnumerationCampaignV2WholePlanCapture,
  PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits,
  PostgreSqlEnumerationCampaignV2WholePlanRequest
}
import org.testcontainers.postgresql.PostgreSQLContainer

private object PostgreSqlEnumerationCampaignV2WholePlanCaptureLiveSuite {
  final case class Database(
      jdbcUrl: String,
      username: String,
      password: String
  )

  /** Counts the connections requested by the capture boundary itself. */
  final class CountingDataSource(database: Database) extends DataSource {
    private val requestedConnections = new AtomicInteger(0)
    @volatile private var logWriter: PrintWriter = _
    @volatile private var loginTimeoutSeconds = 0

    def connectionRequestCount: Int = requestedConnections.get()

    override def getConnection(): Connection = {
      requestedConnections.incrementAndGet()
      DriverManager.getConnection(
        database.jdbcUrl,
        database.username,
        database.password
      )
    }

    override def getConnection(username: String, password: String): Connection = {
      requestedConnections.incrementAndGet()
      DriverManager.getConnection(database.jdbcUrl, username, password)
    }

    override def getLogWriter(): PrintWriter = logWriter
    override def setLogWriter(value: PrintWriter): Unit = logWriter = value
    override def getLoginTimeout(): Int = loginTimeoutSeconds
    override def setLoginTimeout(seconds: Int): Unit =
      loginTimeoutSeconds = seconds
    override def getParentLogger(): Logger =
      Logger.getLogger(classOf[CountingDataSource].getName)

    override def isWrapperFor(interface: Class[_]): Boolean =
      interface != null && interface.isInstance(this)

    override def unwrap[T](interface: Class[T]): T =
      if (isWrapperFor(interface)) interface.cast(this)
      else
        throw new SQLException(
          s"${getClass.getName} does not wrap ${Option(interface).map(_.getName).orNull}"
        )
  }
}

/** Live correctness gate for the exact campaign-v2 whole-plan capture.
  *
  * The gate owns a fresh schema, captures one native action followed by one
  * retained action on one capture-managed JDBC session, and retains no live
  * output or timing evidence.
  */
class PostgreSqlEnumerationCampaignV2WholePlanCaptureLiveSuite
    extends munit.FunSuite {
  import PostgreSqlEnumerationCampaignV2WholePlanCaptureLiveSuite.{
    CountingDataSource,
    Database
  }

  private val NativeSql =
    "SELECT \"k\", \"payload\" FROM \"capture_input\" " +
      "WHERE \"payload\" <> 'Δ'"
  private val CandidateSql = NativeSql + " ORDER BY \"k\", \"payload\""

  private val ExpectedPlanGrammarId =
    "postgresql-18-core-nonverbose-analyze-false-select-json-v1"
  private val ExpectedPlanGrammarSourceCommit =
    "739f5a947d57845ec60d998e8b0e357e8b906049"
  private val ExpectedPlanGrammarSourcePath =
    "src/backend/commands/explain.c"
  private val ExpectedPlanGrammarSourceSha256 =
    "34c86d6070224a0e981efef51f79101d6d505e5874f1684ace183034bab14bb4"

  private val RuntimeFieldNames = Set(
    "Execution Time",
    "Heap Fetches",
    "Original Hash Batches",
    "Original Hash Buckets",
    "Peak Memory Usage",
    "Planning Time",
    "Sort Method",
    "Sort Space Type",
    "Sort Space Used",
    "Triggers",
    "Workers",
    "Workers Launched"
  )

  private var postgres: PostgreSQLContainer = _
  private var setupConnection: Connection = _
  private var database: Database = _
  private var schemaName: String = _
  private var schemaCreated = false

  override def beforeAll(): Unit = {
    super.beforeAll()
    database = externalDatabase.getOrElse {
      postgres = new PostgreSQLContainer("postgres:18.4-alpine")
        .withDatabaseName("whole_plan_capture_live_gate_v1")
        .withUsername("whole_plan_capture_gate")
        .withPassword("whole_plan_capture_gate")
      postgres.start()
      Database(postgres.getJdbcUrl, postgres.getUsername, postgres.getPassword)
    }
    setupConnection = DriverManager.getConnection(
      database.jdbcUrl,
      database.username,
      database.password
    )
    schemaName =
      "whole_plan_capture_live_" + UUID.randomUUID().toString.replace("-", "")
    prepareFreshSchema()
  }

  override def afterAll(): Unit =
    try cleanupFreshSchema()
    finally {
      try if (setupConnection != null) setupConnection.close()
      finally {
        try if (postgres != null) postgres.stop()
        finally super.afterAll()
      }
    }

  test("PostgreSQL 18 captures two exact ANALYZE-FALSE plans on one session") {
    val native = rightOrFail(
      PostgreSqlEnumerationCampaignV2PlanAction.create(
        "native",
        PostgreSqlEnumerationCampaignV2PlanActionKind.Native,
        NativeSql
      )
    )
    val candidate = rightOrFail(
      PostgreSqlEnumerationCampaignV2PlanAction.create(
        "rewrite-0001",
        PostgreSqlEnumerationCampaignV2PlanActionKind.RetainedCandidate,
        CandidateSql
      )
    )
    val request = rightOrFail(
      PostgreSqlEnumerationCampaignV2WholePlanRequest.create(
        Vector(native, candidate)
      )
    )
    val dataSource = new CountingDataSource(database)
    val capture = rightOrFail(
      PostgreSqlEnumerationCampaignV2WholePlanCapture.forDataSource(
        dataSource,
        schemaName
      )
    )
    val batch = rightOrFail(
      capture.capture(
        request,
        PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits.default
      )
    )

    assertEquals(dataSource.connectionRequestCount, 1)
    assertEquals(batch.postgresqlServerMajor, 18)
    assertEquals(batch.provenance.postgresqlServerMajor, 18)
    assert(
      batch.provenance.serverVersionNum >= 180000 &&
        batch.provenance.serverVersionNum < 190000,
      clues(batch.provenance.serverVersionNum)
    )
    assertEquals(batch.planGrammarId, ExpectedPlanGrammarId)
    assertEquals(
      batch.planGrammarSourceCommit,
      ExpectedPlanGrammarSourceCommit
    )
    assertEquals(batch.planGrammarSourcePath, ExpectedPlanGrammarSourcePath)
    assertEquals(
      batch.planGrammarSourceSha256,
      ExpectedPlanGrammarSourceSha256
    )
    assertEquals(batch.provenance.databaseEncoding, "UTF8")
    assertEquals(batch.provenance.clientEncoding, "UTF8")
    assertEquals(
      batch.limits.statementTimeoutSeconds,
      PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits.StatementTimeoutSeconds
    )

    val provenanceSettings = batch.provenance.settings.iterator.map { setting =>
      setting.name -> setting.value
    }.toMap
    val expectedSearchPath = s"pg_catalog, $schemaName, pg_temp"
    assertEquals(batch.provenance.settingCount, 63)
    assertEquals(provenanceSettings.size, 63)
    assert(provenanceSettings.values.forall(_.exists(_.nonEmpty)))
    assertEquals(provenanceSettings("client_encoding"), Some("UTF8"))
    assertEquals(provenanceSettings("server_encoding"), Some("UTF8"))
    assertEquals(provenanceSettings("search_path"), Some(expectedSearchPath))
    assertEquals(provenanceSettings("statement_timeout"), Some("10s"))
    assertEquals(
      provenanceSettings("transaction_isolation"),
      Some("repeatable read")
    )
    assertEquals(provenanceSettings("transaction_read_only"), Some("on"))

    assertEquals(
      batch.observations.map(observation =>
        observation.actionId -> observation.actionKind.id
      ),
      Vector("native" -> "native", "rewrite-0001" -> "retained_candidate")
    )
    assertEquals(
      batch.observations.map(_.renderedStatementSha256),
      Vector(sha256(NativeSql.getBytes(StandardCharsets.UTF_8)),
        sha256(CandidateSql.getBytes(StandardCharsets.UTF_8)))
    )
    assertEquals(
      BigInt(batch.totalPlanUtf8Bytes),
      batch.observations
        .map(observation => BigInt(observation.document.utf8ByteLength))
        .sum
    )

    batch.observations.foreach { observation =>
      val document = observation.document
      val bytes = document.rawUtf8Bytes
      assert(bytes.nonEmpty, clues(observation.actionId))
      assertEquals(document.utf8ByteLength, bytes.length.toLong)
      assertEquals(document.rawUtf8Sha256, sha256(bytes))
      assertEquals(document.rawUtf8, new String(bytes, StandardCharsets.UTF_8))
      assert(document.walkerReceipt.nodeCount > 0, clues(observation.actionId))
      assert(document.walkerReceipt.nodes.nonEmpty, clues(observation.actionId))
      assertEquals(
        document.walkerReceipt.nodeCount,
        document.walkerReceipt.nodes.size
      )
      assertEquals(
        document.settings.map(setting => setting.name -> setting.value).toMap
          .get("search_path"),
        Some(expectedSearchPath)
      )
      assertNoRuntimeFields(ujson.read(bytes), observation.actionId)
    }
  }

  private def prepareFreshSchema(): Unit = {
    Using.resource(
      setupConnection.prepareStatement(
        "SELECT EXISTS (" +
          "SELECT 1 FROM pg_catalog.pg_namespace WHERE nspname = ?" +
          ")"
      )
    ) { check =>
      check.setString(1, schemaName)
      Using.resource(check.executeQuery()) { rows =>
        assert(rows.next())
        assert(!rows.getBoolean(1), clues(schemaName))
        assert(!rows.next())
      }
    }
    val schema = quotedIdentifier(schemaName)
    Using.resource(setupConnection.createStatement()) { statement =>
      statement.execute(s"CREATE SCHEMA $schema")
      schemaCreated = true
      statement.execute(
        s"""CREATE TABLE $schema."capture_input" (
           |  "k" INTEGER NOT NULL,
           |  "payload" TEXT
           |)""".stripMargin
      )
      statement.executeUpdate(
        s"""INSERT INTO $schema."capture_input" ("k", "payload") VALUES
           |  (1, 'alpha'),
           |  (2, 'Δ'),
           |  (3, 'omega'),
           |  (4, NULL)""".stripMargin
      )
      statement.execute(s"ANALYZE $schema.\"capture_input\"")
    }
  }

  private def cleanupFreshSchema(): Unit =
    if (setupConnection != null && schemaCreated) {
      Using.resource(setupConnection.createStatement()) { statement =>
        statement.execute(s"DROP SCHEMA ${quotedIdentifier(schemaName)} CASCADE")
      }
      schemaCreated = false
    }

  private def assertNoRuntimeFields(value: ujson.Value, actionId: String): Unit = {
    val keys = objectKeys(value)
    val forbidden = keys.filter { key =>
      RuntimeFieldNames(key) || key.startsWith("Actual ") ||
      key.startsWith("Rows Removed by ")
    }
    assertEquals(forbidden, Vector.empty, clues(actionId, forbidden))
  }

  private def objectKeys(value: ujson.Value): Vector[String] =
    value match {
      case objectValue: ujson.Obj =>
        objectValue.value.iterator.flatMap { case (key, child) =>
          Iterator.single(key) ++ objectKeys(child).iterator
        }.toVector
      case arrayValue: ujson.Arr =>
        arrayValue.value.iterator.flatMap(objectKeys).toVector
      case _ => Vector.empty
    }

  private def rightOrFail[Error, Value](
      result: Either[Error, Value]
  ): Value =
    result.fold(error => fail(error.toString), identity)

  private def quotedIdentifier(value: String): String = {
    require(
      value != null && value.matches("[a-z][a-z0-9_]{0,62}"),
      "integration schema identifier is unsafe"
    )
    s"\"$value\""
  }

  private def sha256(bytes: Array[Byte]): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(bytes)
      .iterator
      .map(byte => f"${byte & 0xff}%02x")
      .mkString

  private def externalDatabase: Option[Database] = {
    val urlName = "YANNAKAKIS_TEST_JDBC_URL"
    val userName = "YANNAKAKIS_TEST_DB_USER"
    val passwordName = "YANNAKAKIS_TEST_DB_PASSWORD"
    val names = Vector(urlName, userName, passwordName)
    val present = names.filter(sys.env.contains)

    if (present.isEmpty) None
    else if (present.size != names.size)
      throw new IllegalArgumentException(
        "External PostgreSQL test configuration is incomplete; missing: " +
          names.filterNot(sys.env.contains).mkString(", ")
      )
    else {
      if (sys.env(urlName).trim.isEmpty || sys.env(userName).trim.isEmpty)
        throw new IllegalArgumentException(
          "External PostgreSQL URL and user must not be blank"
        )
      Some(Database(sys.env(urlName), sys.env(userName), sys.env(passwordName)))
    }
  }
}

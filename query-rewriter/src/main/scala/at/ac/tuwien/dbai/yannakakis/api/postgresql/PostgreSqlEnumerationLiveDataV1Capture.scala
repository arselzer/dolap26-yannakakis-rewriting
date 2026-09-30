package at.ac.tuwien.dbai.yannakakis.api.postgresql

import java.io.{FilterInputStream, FilterOutputStream, InputStream, OutputStream}
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets
import java.nio.file.attribute.{BasicFileAttributes, PosixFilePermissions}
import java.nio.file.{Files, LinkOption, Path, StandardOpenOption}
import java.security.MessageDigest
import java.sql.{Connection, PreparedStatement, ResultSet, ResultSetMetaData, Statement}

import javax.sql.DataSource

import scala.collection.mutable.ArrayBuffer
import scala.util.control.NonFatal

import org.postgresql.PGConnection

/** Exact expected PostgreSQL COPY-text stream identity. */
private[yannakakis] final case class PostgreSqlEnumerationLiveDataV1Content(
    byteLength: Long,
    rowCount: Long,
    sha256: String,
    encoding: String
) {
  require(byteLength >= 0L, "COPY byte length must be nonnegative")
  require(rowCount >= 0L, "COPY row count must be nonnegative")
  require(
    sha256 != null && sha256.matches("[0-9a-f]{64}"),
    "COPY SHA-256 must be lowercase hexadecimal"
  )
  require(
    encoding == PostgreSqlEnumerationLiveDataV1Capture.CopyEncoding,
    "COPY encoding differs from the frozen contract"
  )
}

/** One request-authorized physical table and its exact private input file. */
private[yannakakis] final case class PostgreSqlEnumerationLiveDataV1TableRequest(
    tableName: String,
    tableDefinitionSha256: String,
    createTableSql: String,
    copyFromStdinSql: String,
    copyToStdoutSql: String,
    statisticsTargetSql: Vector[String],
    analyzeSql: String,
    copyInputPath: Path,
    content: PostgreSqlEnumerationLiveDataV1Content
) {
  require(
    tableName != null && tableName.matches("[a-z][a-z0-9_]{0,62}"),
    "table name is outside the closed identifier grammar"
  )
  require(
    tableDefinitionSha256 != null &&
      tableDefinitionSha256.matches("[0-9a-f]{64}"),
    "table definition SHA-256 is invalid"
  )
  require(
    Vector(createTableSql, copyFromStdinSql, copyToStdoutSql, analyzeSql)
      .forall(value => value != null && value.nonEmpty),
    "table SQL must be nonempty"
  )
  require(
    statisticsTargetSql != null && statisticsTargetSql.nonEmpty &&
      statisticsTargetSql.forall(value => value != null && value.nonEmpty),
    "statistics-target SQL must be nonempty"
  )
  require(copyInputPath != null && copyInputPath.isAbsolute)
  require(content != null)
}

/** Fixed query shape for one raw, bounded capture projection. */
private[yannakakis] final case class PostgreSqlEnumerationLiveDataV1RawQuery(
    sql: String,
    columns: Vector[String],
    maximumRows: Int
) {
  require(sql != null && sql.nonEmpty)
  require(
    columns != null && columns.nonEmpty && columns.distinct.size == columns.size &&
      columns.forall(value => value != null && value.nonEmpty)
  )
  require(maximumRows > 0)
}

/** Table-specific raw catalog and pg_stats query pair. */
private[yannakakis] final case class PostgreSqlEnumerationLiveDataV1TableQueries(
    tableName: String,
    relation: PostgreSqlEnumerationLiveDataV1RawQuery,
    columns: PostgreSqlEnumerationLiveDataV1RawQuery,
    constraintDefinitions: PostgreSqlEnumerationLiveDataV1RawQuery,
    indexDefinitions: PostgreSqlEnumerationLiveDataV1RawQuery,
    inheritanceDefinitions: PostgreSqlEnumerationLiveDataV1RawQuery,
    policyDefinitions: PostgreSqlEnumerationLiveDataV1RawQuery,
    ruleDefinitions: PostgreSqlEnumerationLiveDataV1RawQuery,
    triggerDefinitions: PostgreSqlEnumerationLiveDataV1RawQuery,
    plannerStatistics: PostgreSqlEnumerationLiveDataV1RawQuery,
    extendedStatisticsDefinitions: PostgreSqlEnumerationLiveDataV1RawQuery
) {
  require(tableName != null && tableName.matches("[a-z][a-z0-9_]{0,62}"))
  require(
    Vector(
      relation,
      columns,
      constraintDefinitions,
      indexDefinitions,
      inheritanceDefinitions,
      policyDefinitions,
      ruleDefinitions,
      triggerDefinitions,
      plannerStatistics,
      extendedStatisticsDefinitions
    ).forall(_ != null)
  )
}

/**
  * Schema-dependent SQL lives in the protocol adapter, not in the JDBC
  * foundation. This keeps changes to raw projection fields explicit.
  */
private[yannakakis] final case class PostgreSqlEnumerationLiveDataV1RawQueries(
    provenance: PostgreSqlEnumerationLiveDataV1RawQuery,
    serverIdentity: PostgreSqlEnumerationLiveDataV1RawQuery,
    databaseIdentity: PostgreSqlEnumerationLiveDataV1RawQuery,
    snapshotIdentity: PostgreSqlEnumerationLiveDataV1RawQuery,
    schemaCatalog: PostgreSqlEnumerationLiveDataV1RawQuery,
    schemaOwner: PostgreSqlEnumerationLiveDataV1RawQuery,
    relationSet: PostgreSqlEnumerationLiveDataV1RawQuery,
    tables: Vector[PostgreSqlEnumerationLiveDataV1TableQueries]
) {
  require(
    Vector(
      provenance,
      serverIdentity,
      databaseIdentity,
      snapshotIdentity,
      schemaCatalog,
      schemaOwner,
      relationSet
    ).forall(_ != null)
  )
  require(tables != null && tables.nonEmpty && tables.forall(_ != null))
  require(tables.map(_.tableName).distinct.size == tables.size)
}

/** One fully admitted workload-local live-data request. */
private[yannakakis] final case class PostgreSqlEnumerationLiveDataV1Request(
    schemaName: String,
    createSchemaSql: String,
    tables: Vector[PostgreSqlEnumerationLiveDataV1TableRequest],
    rawQueries: PostgreSqlEnumerationLiveDataV1RawQueries
) {
  require(
    schemaName != null && schemaName.matches("[a-z][a-z0-9_]{0,62}"),
    "schema name is outside the closed identifier grammar"
  )
  require(createSchemaSql != null && createSchemaSql.nonEmpty)
  require(tables != null && tables.nonEmpty && tables.forall(_ != null))
  require(tables.map(_.tableName).distinct.size == tables.size)
  require(rawQueries != null)
  require(
    rawQueries.tables.map(_.tableName) == tables.map(_.tableName),
    "raw table queries are not in exact request order"
  )
}

/** Raw JDBC text projection; NULL remains distinct from the empty string. */
private[yannakakis] final case class PostgreSqlEnumerationLiveDataV1RawResult(
    columns: Vector[String],
    rows: Vector[Vector[Option[String]]]
) {
  require(columns != null && columns.nonEmpty && columns.distinct.size == columns.size)
  require(rows != null && rows.forall(row => row != null && row.size == columns.size))
}

private[yannakakis] final case class PostgreSqlEnumerationLiveDataV1CopyObservation(
    byteLength: Long,
    rowCount: Long,
    sha256: String,
    relativePath: Option[String]
) {
  require(byteLength >= 0L && rowCount >= 0L)
  require(sha256 != null && sha256.matches("[0-9a-f]{64}"))
  require(relativePath.forall(value => value != null && value.nonEmpty))
}

private[yannakakis] final case class PostgreSqlEnumerationLiveDataV1LoadTable(
    tableName: String,
    copyIn: PostgreSqlEnumerationLiveDataV1CopyObservation,
    statisticsTargetCommandCount: Int,
    analyzeExecuted: Boolean
)

private[yannakakis] final case class PostgreSqlEnumerationLiveDataV1Load(
    existingSchemaObserved: Boolean,
    allTablesOneTransaction: Boolean,
    committed: Boolean,
    initialPostgreSql: PostgreSqlEnumerationLiveDataV1RawResult,
    finalPostgreSql: PostgreSqlEnumerationLiveDataV1RawResult,
    tables: Vector[PostgreSqlEnumerationLiveDataV1LoadTable]
)

private[yannakakis] final case class PostgreSqlEnumerationLiveDataV1CaptureTable(
    tableName: String,
    relation: PostgreSqlEnumerationLiveDataV1RawResult,
    columns: PostgreSqlEnumerationLiveDataV1RawResult,
    constraintDefinitions: PostgreSqlEnumerationLiveDataV1RawResult,
    indexDefinitions: PostgreSqlEnumerationLiveDataV1RawResult,
    inheritanceDefinitions: PostgreSqlEnumerationLiveDataV1RawResult,
    policyDefinitions: PostgreSqlEnumerationLiveDataV1RawResult,
    ruleDefinitions: PostgreSqlEnumerationLiveDataV1RawResult,
    triggerDefinitions: PostgreSqlEnumerationLiveDataV1RawResult,
    plannerStatisticsProjection: PostgreSqlEnumerationLiveDataV1RawResult,
    extendedStatisticsDefinitions: PostgreSqlEnumerationLiveDataV1RawResult,
    copyOut: PostgreSqlEnumerationLiveDataV1CopyObservation
)

private[yannakakis] final case class PostgreSqlEnumerationLiveDataV1ReadCapture(
    initialProvenance: PostgreSqlEnumerationLiveDataV1RawResult,
    finalProvenance: PostgreSqlEnumerationLiveDataV1RawResult,
    serverIdentity: PostgreSqlEnumerationLiveDataV1RawResult,
    databaseIdentity: PostgreSqlEnumerationLiveDataV1RawResult,
    snapshotIdentity: PostgreSqlEnumerationLiveDataV1RawResult,
    schemaCatalog: PostgreSqlEnumerationLiveDataV1RawResult,
    schemaOwner: PostgreSqlEnumerationLiveDataV1RawResult,
    relationSet: PostgreSqlEnumerationLiveDataV1RawResult,
    tables: Vector[PostgreSqlEnumerationLiveDataV1CaptureTable],
    sameSession: Boolean,
    transactionIsolation: String,
    transactionReadOnly: Boolean
)

private[yannakakis] final case class PostgreSqlEnumerationLiveDataV1Batch(
    load: PostgreSqlEnumerationLiveDataV1Load,
    capture: PostgreSqlEnumerationLiveDataV1ReadCapture
)

private[yannakakis] sealed trait PostgreSqlEnumerationLiveDataV1CaptureFailure
    extends Product
    with Serializable {
  def code: String
}

private[yannakakis] object PostgreSqlEnumerationLiveDataV1CaptureFailure {
  case object InvalidRequest extends PostgreSqlEnumerationLiveDataV1CaptureFailure {
    val code = "live-data-v1.capture-invalid-request"
  }
  case object OutputDirectoryInvalid extends PostgreSqlEnumerationLiveDataV1CaptureFailure {
    val code = "live-data-v1.capture-output-directory-invalid"
  }
  case object LoadConnectionUnavailable extends PostgreSqlEnumerationLiveDataV1CaptureFailure {
    val code = "live-data-v1.load-connection-unavailable"
  }
  case object LoadSessionFailed extends PostgreSqlEnumerationLiveDataV1CaptureFailure {
    val code = "live-data-v1.load-session-failed"
  }
  case object PostgreSqlContractMismatch extends PostgreSqlEnumerationLiveDataV1CaptureFailure {
    val code = "live-data-v1.postgresql-contract-mismatch"
  }
  case object SchemaAlreadyExists extends PostgreSqlEnumerationLiveDataV1CaptureFailure {
    val code = "live-data-v1.schema-already-exists"
  }
  case object LoadStatementFailed extends PostgreSqlEnumerationLiveDataV1CaptureFailure {
    val code = "live-data-v1.load-statement-failed"
  }
  case object CopyInputInvalid extends PostgreSqlEnumerationLiveDataV1CaptureFailure {
    val code = "live-data-v1.copy-input-invalid"
  }
  case object CopyInputMismatch extends PostgreSqlEnumerationLiveDataV1CaptureFailure {
    val code = "live-data-v1.copy-input-mismatch"
  }
  case object CopyInputFailed extends PostgreSqlEnumerationLiveDataV1CaptureFailure {
    val code = "live-data-v1.copy-input-failed"
  }
  case object LoadCommitFailed extends PostgreSqlEnumerationLiveDataV1CaptureFailure {
    val code = "live-data-v1.load-commit-failed"
  }
  case object CaptureConnectionUnavailable extends PostgreSqlEnumerationLiveDataV1CaptureFailure {
    val code = "live-data-v1.capture-connection-unavailable"
  }
  case object CaptureSessionFailed extends PostgreSqlEnumerationLiveDataV1CaptureFailure {
    val code = "live-data-v1.capture-session-failed"
  }
  case object RawProjectionFailed extends PostgreSqlEnumerationLiveDataV1CaptureFailure {
    val code = "live-data-v1.raw-projection-failed"
  }
  case object RawProjectionMismatch extends PostgreSqlEnumerationLiveDataV1CaptureFailure {
    val code = "live-data-v1.raw-projection-mismatch"
  }
  case object CopyOutputExists extends PostgreSqlEnumerationLiveDataV1CaptureFailure {
    val code = "live-data-v1.copy-output-exists"
  }
  case object CopyOutputFailed extends PostgreSqlEnumerationLiveDataV1CaptureFailure {
    val code = "live-data-v1.copy-output-failed"
  }
  case object CopyOutputMismatch extends PostgreSqlEnumerationLiveDataV1CaptureFailure {
    val code = "live-data-v1.copy-output-mismatch"
  }
  case object CaptureChanged extends PostgreSqlEnumerationLiveDataV1CaptureFailure {
    val code = "live-data-v1.capture-provenance-changed"
  }
  case object CleanupFailed extends PostgreSqlEnumerationLiveDataV1CaptureFailure {
    val code = "live-data-v1.capture-cleanup-failed"
  }
}

private[yannakakis] trait PostgreSqlEnumerationLiveDataV1Capture {
  def capture(
      request: PostgreSqlEnumerationLiveDataV1Request,
      copyOutDirectory: Path
  ): Either[
    PostgreSqlEnumerationLiveDataV1CaptureFailure,
    PostgreSqlEnumerationLiveDataV1Batch
  ]
}

/** PostgreSQL-18.4 workload-local loader and exact raw COPY capture. */
private[yannakakis] object PostgreSqlEnumerationLiveDataV1Capture {
  import PostgreSqlEnumerationLiveDataV1CaptureFailure._

  val PostgreSqlServerVersionNum: Int = 180004
  val StatisticsTarget: Int = 2000
  val CopyEncoding: String =
    "postgresql-copy-text-tab-null-backslash-N-utf8-lf/v1"
  val LoadTransactionStatement: String =
    "SET TRANSACTION ISOLATION LEVEL READ COMMITTED, READ WRITE"
  val CaptureTransactionStatement: String =
    "SET TRANSACTION ISOLATION LEVEL REPEATABLE READ, READ ONLY"
  val CaptureSearchPathStatement: String =
    "SELECT pg_catalog.set_config('search_path', 'pg_catalog, pg_temp', true)"
  val CaptureTextSettingsStatement: String =
    "SELECT pg_catalog.set_config('DateStyle', 'ISO, MDY', true), " +
      "pg_catalog.set_config('extra_float_digits', '3', true), " +
      "pg_catalog.set_config('standard_conforming_strings', 'on', true), " +
      "pg_catalog.set_config('TimeZone', 'UTC', true)"
  val StatementTimeoutSetting: String = "300000"
  val LockTimeoutSetting: String = "30000"
  val TimeoutStatement: String =
    "SELECT pg_catalog.set_config('statement_timeout', '" +
      StatementTimeoutSetting + "ms', true), " +
      "pg_catalog.set_config('lock_timeout', '" + LockTimeoutSetting + "ms', true)"
  val SchemaExistsStatement: String =
    "SELECT pg_catalog.count(*) FROM pg_catalog.pg_namespace WHERE nspname = ?"
  val PostgreSqlContractStatement: String =
    "SELECT pg_catalog.current_setting('server_version_num') AS server_version_num, " +
      "pg_catalog.current_setting('server_encoding') AS server_encoding, " +
      "pg_catalog.current_setting('client_encoding') AS client_encoding, " +
      "pg_catalog.current_database() AS database_name, " +
      "control.system_identifier::pg_catalog.text AS system_identifier, " +
      "(SELECT setting FROM pg_catalog.pg_settings WHERE name = 'statement_timeout') " +
      "AS statement_timeout_ms, " +
      "(SELECT setting FROM pg_catalog.pg_settings WHERE name = 'lock_timeout') " +
      "AS lock_timeout_ms, " +
      "pg_catalog.current_setting('transaction_isolation') AS transaction_isolation, " +
      "pg_catalog.current_setting('transaction_read_only') AS transaction_read_only, " +
      "pg_catalog.current_setting('search_path') AS search_path " +
      "FROM pg_catalog.pg_control_system() AS control"
  val PostgreSqlContractColumns: Vector[String] =
    Vector(
      "server_version_num",
      "server_encoding",
      "client_encoding",
      "database_name",
      "system_identifier",
      "statement_timeout_ms",
      "lock_timeout_ms",
      "transaction_isolation",
      "transaction_read_only",
      "search_path"
    )
  val StatementTimeoutSeconds: Int = 300
  val MaximumRawFieldUtf8Bytes: Int = 16 * 1024 * 1024
  val MaximumCopyBytesPerTable: Long = 256L * 1024L * 1024L

  private val FilePermissions = PosixFilePermissions.fromString("rw-------")
  private val NoFollow = Array(LinkOption.NOFOLLOW_LINKS)

  private[postgresql] trait CopyApi {
    def copyIn(sql: String, input: InputStream): Long
    def copyOut(sql: String, output: OutputStream): Long
  }

  private[postgresql] trait CopyApiFactory {
    def forConnection(
        connection: Connection
    ): Either[PostgreSqlEnumerationLiveDataV1CaptureFailure, CopyApi]
  }

  private object PgCopyApiFactory extends CopyApiFactory {
    override def forConnection(
        connection: Connection
    ): Either[PostgreSqlEnumerationLiveDataV1CaptureFailure, CopyApi] =
      try {
        val pg = connection.unwrap(classOf[PGConnection])
        if (pg == null) Left(PostgreSqlContractMismatch)
        else {
          val manager = pg.getCopyAPI
          if (manager == null) Left(PostgreSqlContractMismatch)
          else Right(new CopyApi {
            override def copyIn(sql: String, input: InputStream): Long =
              manager.copyIn(sql, input)
            override def copyOut(sql: String, output: OutputStream): Long =
              manager.copyOut(sql, output)
          })
        }
      } catch { case NonFatal(_) => Left(PostgreSqlContractMismatch) }
  }

  def forDataSource(
      dataSource: DataSource
  ): Either[PostgreSqlEnumerationLiveDataV1CaptureFailure, PostgreSqlEnumerationLiveDataV1Capture] =
    if (dataSource == null) Left(InvalidRequest)
    else Right(new ManagedCapture(() => dataSource.getConnection, PgCopyApiFactory))

  private[postgresql] def forTest(
      openConnection: () => Connection,
      copyApiFactory: CopyApiFactory
  ): PostgreSqlEnumerationLiveDataV1Capture =
    new ManagedCapture(openConnection, copyApiFactory)

  private final class ManagedCapture(
      openConnection: () => Connection,
      copyApiFactory: CopyApiFactory
  ) extends PostgreSqlEnumerationLiveDataV1Capture {
    override def capture(
        request: PostgreSqlEnumerationLiveDataV1Request,
        copyOutDirectory: Path
    ): Either[
      PostgreSqlEnumerationLiveDataV1CaptureFailure,
      PostgreSqlEnumerationLiveDataV1Batch
    ] = {
      if (request == null || copyOutDirectory == null) return Left(InvalidRequest)
      if (!validPrivateOutputDirectory(copyOutDirectory))
        return Left(OutputDirectoryInvalid)
      val loaded = withConnection(openConnection, LoadConnectionUnavailable) { connection =>
        load(connection, request, copyApiFactory)
      }
      loaded.flatMap { loadResult =>
        withConnection(openConnection, CaptureConnectionUnavailable) { connection =>
          captureReadOnly(connection, request, copyOutDirectory, copyApiFactory)
        }.map(captureResult => PostgreSqlEnumerationLiveDataV1Batch(loadResult, captureResult))
      }
    }
  }

  private def withConnection[A](
      openConnection: () => Connection,
      unavailable: PostgreSqlEnumerationLiveDataV1CaptureFailure
  )(
      operation: Connection => Either[PostgreSqlEnumerationLiveDataV1CaptureFailure, A]
  ): Either[PostgreSqlEnumerationLiveDataV1CaptureFailure, A] = {
    val connection =
      try openConnection()
      catch { case NonFatal(_) => return Left(unavailable) }
    if (connection == null) return Left(unavailable)
    var result: Either[PostgreSqlEnumerationLiveDataV1CaptureFailure, A] = Left(unavailable)
    var closeFailed = false
    try result = operation(connection)
    catch { case NonFatal(_) => result = Left(unavailable) }
    finally {
      try connection.close()
      catch { case NonFatal(_) => closeFailed = true }
    }
    if (closeFailed) Left(CleanupFailed) else result
  }

  private def load(
      connection: Connection,
      request: PostgreSqlEnumerationLiveDataV1Request,
      copyApiFactory: CopyApiFactory
  ): Either[PostgreSqlEnumerationLiveDataV1CaptureFailure, PostgreSqlEnumerationLiveDataV1Load] = {
    var transactionStarted = false
    var committed = false
    var cleanupFailed = false
    var result: Either[PostgreSqlEnumerationLiveDataV1CaptureFailure, PostgreSqlEnumerationLiveDataV1Load] =
      Left(LoadSessionFailed)
    try {
      if (connection.isClosed || !connection.getAutoCommit) result = Left(LoadSessionFailed)
      else {
        connection.setAutoCommit(false)
        transactionStarted = true
        result = for {
          _ <- execute(connection, LoadTransactionStatement, LoadSessionFailed)
          _ <- execute(connection, TimeoutStatement, LoadSessionFailed)
          _ <- execute(connection, CaptureSearchPathStatement, LoadSessionFailed)
          initial <- readPostgreSqlContract(connection, "read committed", "off")
          exists <- schemaExists(connection, request.schemaName)
          _ <- if (exists) Left(SchemaAlreadyExists) else Right(())
          _ <- execute(connection, request.createSchemaSql, LoadStatementFailed)
          copyApi <- copyApiFactory.forConnection(connection)
          tables <- loadTables(connection, request.tables, copyApi)
          finalValue <- readPostgreSqlContract(connection, "read committed", "off")
          _ <- if (finalValue == initial) Right(()) else Left(PostgreSqlContractMismatch)
          _ <- try {
            connection.commit()
            committed = true
            Right(())
          } catch { case NonFatal(_) => Left(LoadCommitFailed) }
        } yield PostgreSqlEnumerationLiveDataV1Load(
          existingSchemaObserved = false,
          allTablesOneTransaction = true,
          committed = true,
          initialPostgreSql = initial,
          finalPostgreSql = finalValue,
          tables = tables
        )
      }
    } catch { case NonFatal(_) => result = Left(LoadSessionFailed) }
    finally {
      if (transactionStarted && !committed) {
        try connection.rollback()
        catch { case NonFatal(_) => cleanupFailed = true }
      }
      if (transactionStarted) {
        try connection.setAutoCommit(true)
        catch { case NonFatal(_) => cleanupFailed = true }
      }
    }
    if (cleanupFailed) Left(CleanupFailed) else result
  }

  private def loadTables(
      connection: Connection,
      tables: Vector[PostgreSqlEnumerationLiveDataV1TableRequest],
      copyApi: CopyApi
  ): Either[
    PostgreSqlEnumerationLiveDataV1CaptureFailure,
    Vector[PostgreSqlEnumerationLiveDataV1LoadTable]
  ] = {
    val result = Vector.newBuilder[PostgreSqlEnumerationLiveDataV1LoadTable]
    val iterator = tables.iterator
    while (iterator.hasNext) {
      val table = iterator.next()
      execute(connection, table.createTableSql, LoadStatementFailed) match {
        case Left(problem) => return Left(problem)
        case Right(_)      => ()
      }
      val copy = copyIn(copyApi, table) match {
        case Left(problem) => return Left(problem)
        case Right(value)  => value
      }
      table.statisticsTargetSql.foreach { sql =>
        execute(connection, sql, LoadStatementFailed) match {
          case Left(problem) => return Left(problem)
          case Right(_)      => ()
        }
      }
      execute(connection, table.analyzeSql, LoadStatementFailed) match {
        case Left(problem) => return Left(problem)
        case Right(_)      => ()
      }
      result += PostgreSqlEnumerationLiveDataV1LoadTable(
        table.tableName,
        copy,
        table.statisticsTargetSql.size,
        analyzeExecuted = true
      )
    }
    Right(result.result())
  }

  private def captureReadOnly(
      connection: Connection,
      request: PostgreSqlEnumerationLiveDataV1Request,
      copyOutDirectory: Path,
      copyApiFactory: CopyApiFactory
  ): Either[
    PostgreSqlEnumerationLiveDataV1CaptureFailure,
    PostgreSqlEnumerationLiveDataV1ReadCapture
  ] = {
    var transactionStarted = false
    var cleanupFailed = false
    var result: Either[PostgreSqlEnumerationLiveDataV1CaptureFailure, PostgreSqlEnumerationLiveDataV1ReadCapture] =
      Left(CaptureSessionFailed)
    try {
      if (connection.isClosed || !connection.getAutoCommit) result = Left(CaptureSessionFailed)
      else {
        connection.setAutoCommit(false)
        transactionStarted = true
        result = for {
          _ <- execute(connection, CaptureTransactionStatement, CaptureSessionFailed)
          _ <- execute(connection, TimeoutStatement, CaptureSessionFailed)
          _ <- execute(connection, CaptureSearchPathStatement, CaptureSessionFailed)
          _ <- execute(connection, CaptureTextSettingsStatement, CaptureSessionFailed)
          contract <- readPostgreSqlContract(connection, "repeatable read", "on")
          initial <- query(connection, request.rawQueries.provenance)
          server <- query(connection, request.rawQueries.serverIdentity)
          database <- query(connection, request.rawQueries.databaseIdentity)
          snapshot <- query(connection, request.rawQueries.snapshotIdentity)
          schemaCatalog <- query(connection, request.rawQueries.schemaCatalog)
          schemaOwner <- query(connection, request.rawQueries.schemaOwner)
          relationSet <- query(connection, request.rawQueries.relationSet)
          copyApi <- copyApiFactory.forConnection(connection)
          tables <- captureTables(connection, request, copyOutDirectory, copyApi)
          finalValue <- query(connection, request.rawQueries.provenance)
          _ <- if (finalValue == initial) Right(()) else Left(CaptureChanged)
        } yield PostgreSqlEnumerationLiveDataV1ReadCapture(
          initial,
          finalValue,
          server,
          database,
          snapshot,
          schemaCatalog,
          schemaOwner,
          relationSet,
          tables,
          sameSession = true,
          transactionIsolation = "repeatable read",
          transactionReadOnly = true
        )
      }
    } catch { case NonFatal(_) => result = Left(CaptureSessionFailed) }
    finally {
      if (transactionStarted) {
        try connection.rollback()
        catch { case NonFatal(_) => cleanupFailed = true }
        try connection.setAutoCommit(true)
        catch { case NonFatal(_) => cleanupFailed = true }
      }
    }
    if (cleanupFailed) Left(CleanupFailed) else result
  }

  private def captureTables(
      connection: Connection,
      request: PostgreSqlEnumerationLiveDataV1Request,
      copyOutDirectory: Path,
      copyApi: CopyApi
  ): Either[
    PostgreSqlEnumerationLiveDataV1CaptureFailure,
    Vector[PostgreSqlEnumerationLiveDataV1CaptureTable]
  ] = {
    val result = Vector.newBuilder[PostgreSqlEnumerationLiveDataV1CaptureTable]
    request.tables.zip(request.rawQueries.tables).foreach { case (table, queries) =>
      val projections = for {
        relation <- query(connection, queries.relation)
        columns <- query(connection, queries.columns)
        constraints <- query(connection, queries.constraintDefinitions)
        indexes <- query(connection, queries.indexDefinitions)
        inheritance <- query(connection, queries.inheritanceDefinitions)
        policies <- query(connection, queries.policyDefinitions)
        rules <- query(connection, queries.ruleDefinitions)
        triggers <- query(connection, queries.triggerDefinitions)
        statistics <- query(connection, queries.plannerStatistics)
        extended <- query(connection, queries.extendedStatisticsDefinitions)
      } yield (
        relation,
        columns,
        constraints,
        indexes,
        inheritance,
        policies,
        rules,
        triggers,
        statistics,
        extended
      )
      val (
        relation,
        columns,
        constraints,
        indexes,
        inheritance,
        policies,
        rules,
        triggers,
        statistics,
        extendedStatistics
      ) = projections match {
        case Left(problem) => return Left(problem)
        case Right(value)  => value
      }
      val relative = s"copy-out/${table.tableName}.copy"
      val copy = copyOut(
        copyApi,
        table.copyToStdoutSql,
        copyOutDirectory.resolve(s"${table.tableName}.copy"),
        relative,
        table.content
      ) match {
        case Left(problem) => return Left(problem)
        case Right(value)  => value
      }
      result += PostgreSqlEnumerationLiveDataV1CaptureTable(
        table.tableName,
        relation,
        columns,
        constraints,
        indexes,
        inheritance,
        policies,
        rules,
        triggers,
        statistics,
        extendedStatistics,
        copy
      )
    }
    Right(result.result())
  }

  private def copyIn(
      copyApi: CopyApi,
      table: PostgreSqlEnumerationLiveDataV1TableRequest
  ): Either[
    PostgreSqlEnumerationLiveDataV1CaptureFailure,
    PostgreSqlEnumerationLiveDataV1CopyObservation
  ] = {
    val expected = table.content
    if (expected.byteLength > MaximumCopyBytesPerTable) return Left(CopyInputInvalid)
    val before = regularFileIdentity(table.copyInputPath, expected.byteLength)
      .getOrElse(return Left(CopyInputInvalid))
    var input: InputStream = null
    try {
      val raw = Files.newInputStream(
        table.copyInputPath,
        StandardOpenOption.READ,
        LinkOption.NOFOLLOW_LINKS
      )
      val counted = new BoundedDigestInputStream(raw, expected.byteLength)
      input = counted
      val rowCount = copyApi.copyIn(table.copyFromStdinSql, counted)
      val observation = counted.observation(rowCount, None)
      val after = regularFileIdentity(table.copyInputPath, expected.byteLength)
      if (
        after.isEmpty || before != after.get ||
        !matches(observation, expected)
      ) Left(CopyInputMismatch)
      else Right(observation)
    } catch {
      case _: CopyBoundExceeded => Left(CopyInputMismatch)
      case NonFatal(_)          => Left(CopyInputFailed)
    } finally {
      if (input != null) try input.close() catch { case NonFatal(_) => () }
    }
  }

  private def copyOut(
      copyApi: CopyApi,
      sql: String,
      outputPath: Path,
      relativePath: String,
      expected: PostgreSqlEnumerationLiveDataV1Content
  ): Either[
    PostgreSqlEnumerationLiveDataV1CaptureFailure,
    PostgreSqlEnumerationLiveDataV1CopyObservation
  ] = {
    if (expected.byteLength > MaximumCopyBytesPerTable) return Left(CopyOutputMismatch)
    var channel: FileChannel = null
    var output: OutputStream = null
    try {
      channel = openPrivateNewFile(outputPath)
      val counted = new BoundedDigestOutputStream(
        java.nio.channels.Channels.newOutputStream(channel),
        expected.byteLength
      )
      output = counted
      val rowCount = copyApi.copyOut(sql, counted)
      counted.flush()
      channel.force(true)
      val observation = counted.observation(rowCount, Some(relativePath))
      if (
        !matches(observation, expected) ||
        !privateSingleLinkFile(outputPath, expected.byteLength)
      ) Left(CopyOutputMismatch)
      else Right(observation)
    } catch {
      case _: java.nio.file.FileAlreadyExistsException => Left(CopyOutputExists)
      case _: CopyBoundExceeded                        => Left(CopyOutputMismatch)
      case NonFatal(_)                                 => Left(CopyOutputFailed)
    } finally {
      if (output != null) try output.close() catch { case NonFatal(_) => () }
      else if (channel != null) try channel.close() catch { case NonFatal(_) => () }
    }
  }

  private def matches(
      observed: PostgreSqlEnumerationLiveDataV1CopyObservation,
      expected: PostgreSqlEnumerationLiveDataV1Content
  ): Boolean =
    observed.byteLength == expected.byteLength &&
      observed.rowCount == expected.rowCount &&
      observed.sha256 == expected.sha256

  private def schemaExists(
      connection: Connection,
      schemaName: String
  ): Either[PostgreSqlEnumerationLiveDataV1CaptureFailure, Boolean] = {
    var statement: PreparedStatement = null
    var resultSet: ResultSet = null
    try {
      statement = connection.prepareStatement(SchemaExistsStatement)
      statement.setQueryTimeout(StatementTimeoutSeconds)
      statement.setMaxRows(2)
      statement.setString(1, schemaName)
      resultSet = statement.executeQuery()
      if (
        resultSet.getMetaData.getColumnCount != 1 || !resultSet.next()
      ) Left(LoadSessionFailed)
      else {
        val count = resultSet.getLong(1)
        if (resultSet.wasNull() || resultSet.next() || count < 0L || count > 1L)
          Left(LoadSessionFailed)
        else Right(count == 1L)
      }
    } catch { case NonFatal(_) => Left(LoadSessionFailed) }
    finally {
      if (resultSet != null) try resultSet.close() catch { case NonFatal(_) => () }
      if (statement != null) try statement.close() catch { case NonFatal(_) => () }
    }
  }

  private def readPostgreSqlContract(
      connection: Connection,
      expectedIsolation: String,
      expectedReadOnly: String
  ): Either[
    PostgreSqlEnumerationLiveDataV1CaptureFailure,
    PostgreSqlEnumerationLiveDataV1RawResult
  ] =
    query(
      connection,
      PostgreSqlEnumerationLiveDataV1RawQuery(
        PostgreSqlContractStatement,
        PostgreSqlContractColumns,
        1
      )
    ).flatMap { value =>
      if (verifyPostgreSqlContract(value, expectedIsolation, expectedReadOnly)) Right(value)
      else Left(PostgreSqlContractMismatch)
    }

  private def verifyPostgreSqlContract(
      value: PostgreSqlEnumerationLiveDataV1RawResult,
      expectedIsolation: String,
      expectedReadOnly: String
  ): Boolean =
    value.columns == PostgreSqlContractColumns &&
      value.rows.size == 1 && {
        val row = value.rows.head
        row.size == PostgreSqlContractColumns.size &&
        row(0).contains(PostgreSqlServerVersionNum.toString) &&
        row(1).contains("UTF8") && row(2).contains("UTF8") &&
        row(3).exists(value => value.nonEmpty && value.getBytes(StandardCharsets.UTF_8).length <= 63) &&
        row(4).exists(_.matches("0|[1-9][0-9]*")) &&
        row(5).contains(StatementTimeoutSetting) &&
        row(6).contains(LockTimeoutSetting) &&
        row(7).contains(expectedIsolation) && row(8).contains(expectedReadOnly) &&
        row(9).contains("pg_catalog, pg_temp")
      }

  private def execute(
      connection: Connection,
      sql: String,
      failure: PostgreSqlEnumerationLiveDataV1CaptureFailure
  ): Either[PostgreSqlEnumerationLiveDataV1CaptureFailure, Unit] = {
    var statement: Statement = null
    try {
      statement = connection.createStatement()
      statement.setQueryTimeout(StatementTimeoutSeconds)
      statement.execute(sql)
      Right(())
    } catch { case NonFatal(_) => Left(failure) }
    finally if (statement != null) try statement.close() catch { case NonFatal(_) => () }
  }

  private def query(
      connection: Connection,
      query: PostgreSqlEnumerationLiveDataV1RawQuery
  ): Either[
    PostgreSqlEnumerationLiveDataV1CaptureFailure,
    PostgreSqlEnumerationLiveDataV1RawResult
  ] = {
    var statement: Statement = null
    var resultSet: ResultSet = null
    try {
      statement = connection.createStatement()
      statement.setQueryTimeout(StatementTimeoutSeconds)
      statement.setMaxRows(query.maximumRows + 1)
      resultSet = statement.executeQuery(query.sql)
      val metadata = resultSet.getMetaData
      if (!metadataMatches(metadata, query.columns)) return Left(RawProjectionMismatch)
      val rows = Vector.newBuilder[Vector[Option[String]]]
      var count = 0
      while (resultSet.next()) {
        count += 1
        if (count > query.maximumRows) return Left(RawProjectionMismatch)
        val row = Vector.newBuilder[Option[String]]
        var column = 1
        while (column <= query.columns.size) {
          val value = resultSet.getString(column)
          val observed = if (resultSet.wasNull()) None else Option(value)
          if (observed.exists(_.getBytes(StandardCharsets.UTF_8).length > MaximumRawFieldUtf8Bytes))
            return Left(RawProjectionMismatch)
          row += observed
          column += 1
        }
        rows += row.result()
      }
      Right(PostgreSqlEnumerationLiveDataV1RawResult(query.columns, rows.result()))
    } catch { case NonFatal(_) => Left(RawProjectionFailed) }
    finally {
      if (resultSet != null) try resultSet.close() catch { case NonFatal(_) => () }
      if (statement != null) try statement.close() catch { case NonFatal(_) => () }
    }
  }

  private def metadataMatches(
      metadata: ResultSetMetaData,
      expected: Vector[String]
  ): Boolean = {
    if (metadata == null || metadata.getColumnCount != expected.size) return false
    var index = 1
    while (index <= expected.size) {
      if (metadata.getColumnLabel(index) != expected(index - 1)) return false
      index += 1
    }
    true
  }

  private final case class FileIdentity(
      fileKey: Option[AnyRef],
      size: Long,
      modifiedMillis: Long
  )

  private def regularFileIdentity(path: Path, expectedSize: Long): Option[FileIdentity] =
    try {
      val attributes = Files.readAttributes(
        path,
        classOf[BasicFileAttributes],
        LinkOption.NOFOLLOW_LINKS
      )
      if (
        !attributes.isRegularFile || attributes.isSymbolicLink ||
        attributes.size() != expectedSize
      ) None
      else
        Some(
          FileIdentity(
            Option(attributes.fileKey()).map(_.asInstanceOf[AnyRef]),
            attributes.size(),
            attributes.lastModifiedTime().toMillis
          )
        )
    } catch { case NonFatal(_) => None }

  private def validPrivateOutputDirectory(path: Path): Boolean =
    try {
      path.isAbsolute && path == path.normalize() &&
      Files.isDirectory(path, LinkOption.NOFOLLOW_LINKS) &&
      !Files.isSymbolicLink(path) &&
      Files.getPosixFilePermissions(path) ==
        PosixFilePermissions.fromString("rwx------")
    } catch { case NonFatal(_) => false }

  private def openPrivateNewFile(path: Path): FileChannel = {
    val options = new java.util.HashSet[java.nio.file.OpenOption]()
    options.add(StandardOpenOption.CREATE_NEW)
    options.add(StandardOpenOption.WRITE)
    options.add(LinkOption.NOFOLLOW_LINKS)
    FileChannel.open(
      path,
      options,
      PosixFilePermissions.asFileAttribute(FilePermissions)
    )
  }

  private def privateSingleLinkFile(path: Path, expectedSize: Long): Boolean =
    try {
      val attributes = Files.readAttributes(
        path,
        classOf[BasicFileAttributes],
        LinkOption.NOFOLLOW_LINKS
      )
      val links = Files
        .getAttribute(path, "unix:nlink", LinkOption.NOFOLLOW_LINKS)
        .asInstanceOf[java.lang.Number]
        .longValue()
      attributes.isRegularFile && !attributes.isSymbolicLink &&
        attributes.size() == expectedSize && links == 1L &&
        Files.getPosixFilePermissions(path) == FilePermissions
    } catch { case NonFatal(_) => false }

  private final class CopyBoundExceeded extends java.io.IOException

  private abstract class BoundedDigest(
      maximumBytes: Long
  ) {
    require(maximumBytes >= 0L && maximumBytes <= MaximumCopyBytesPerTable)
    protected val digest: MessageDigest = MessageDigest.getInstance("SHA-256")
    protected var bytesSeen: Long = 0L

    final def accept(bytes: Array[Byte], offset: Int, length: Int): Unit = {
      if (length < 0 || length.toLong > maximumBytes - bytesSeen)
        throw new CopyBoundExceeded
      digest.update(bytes, offset, length)
      bytesSeen += length.toLong
    }

    final def acceptByte(value: Int): Unit = {
      if (bytesSeen >= maximumBytes) throw new CopyBoundExceeded
      digest.update(value.toByte)
      bytesSeen += 1L
    }

    final def observation(
        rowCount: Long,
        relativePath: Option[String]
    ): PostgreSqlEnumerationLiveDataV1CopyObservation =
      PostgreSqlEnumerationLiveDataV1CopyObservation(
        bytesSeen,
        rowCount,
        digest.digest().map(byte => f"${byte & 0xff}%02x").mkString,
        relativePath
      )
  }

  private final class BoundedDigestInputStream(
      delegate: InputStream,
      maximumBytes: Long
  ) extends FilterInputStream(delegate) {
    private val state = new BoundedDigest(maximumBytes) {}
    override def read(): Int = {
      val value = super.read()
      if (value >= 0) state.acceptByte(value)
      value
    }
    override def read(bytes: Array[Byte], offset: Int, length: Int): Int = {
      val count = super.read(bytes, offset, length)
      if (count > 0) state.accept(bytes, offset, count)
      count
    }
    def observation(
        rows: Long,
        relativePath: Option[String]
    ): PostgreSqlEnumerationLiveDataV1CopyObservation =
      state.observation(rows, relativePath)
  }

  private final class BoundedDigestOutputStream(
      delegate: OutputStream,
      maximumBytes: Long
  ) extends FilterOutputStream(delegate) {
    private val state = new BoundedDigest(maximumBytes) {}
    override def write(value: Int): Unit = {
      state.acceptByte(value)
      out.write(value)
    }
    override def write(bytes: Array[Byte], offset: Int, length: Int): Unit = {
      state.accept(bytes, offset, length)
      out.write(bytes, offset, length)
    }
    def observation(
        rows: Long,
        relativePath: Option[String]
    ): PostgreSqlEnumerationLiveDataV1CopyObservation =
      state.observation(rows, relativePath)
  }
}

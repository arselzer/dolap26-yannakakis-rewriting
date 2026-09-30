package at.ac.tuwien.dbai.yannakakis.api.postgresql

import java.io.{ByteArrayOutputStream, InputStream, OutputStream}
import java.lang.reflect.{InvocationHandler, Method, Proxy}
import java.nio.charset.StandardCharsets
import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import java.sql.{Connection, PreparedStatement, ResultSet, ResultSetMetaData, Statement}

import scala.collection.mutable

import munit.FunSuite

final class PostgreSqlEnumerationLiveDataV1CaptureSuite extends FunSuite {
  private val copyBytes = "1\t2\n".getBytes(StandardCharsets.UTF_8)
  private val copySha256 = MessageDigest.getInstance("SHA-256").digest(copyBytes)
    .map(byte => f"${byte & 0xff}%02x").mkString

  test("loads once, commits, then captures exact COPY bytes in a separate read-only session") {
    val root = Files.createTempDirectory("yrw-live-jdbc-")
    Files.setPosixFilePermissions(root, PosixFilePermissions.fromString("rwx------"))
    val input = root.resolve("input.copy")
    Files.write(input, copyBytes)
    Files.setPosixFilePermissions(input, PosixFilePermissions.fromString("rw-------"))
    val output = root.resolve("copy-out")
    Files.createDirectory(output)
    Files.setPosixFilePermissions(output, PosixFilePermissions.fromString("rwx------"))

    val state = new JdbcState(schemaCount = 0L)
    val request = fixtureRequest(input)
    val result = PostgreSqlEnumerationLiveDataV1Capture
      .forTest(() => state.open(), state.copyFactory)
      .capture(request, output)
    assert(result.isRight, result.left.toOption.map(_.code).getOrElse("unexpected-right"))
    val batch = result.toOption.get
    assertEquals(state.connectionRequests, 2)
    assertEquals(state.load.commits, 1)
    assertEquals(state.load.rollbacks, 0)
    assertEquals(state.capture.commits, 0)
    assertEquals(state.capture.rollbacks, 1)
    assertEquals(state.copyInCalls, Vector("copy-in"))
    assertEquals(state.copyOutCalls, Vector("copy-out"))
    assertEquals(Files.readAllBytes(output.resolve("edge.copy")).toVector, copyBytes.toVector)
    assertEquals(
      Files.getPosixFilePermissions(output.resolve("edge.copy")),
      PosixFilePermissions.fromString("rw-------")
    )
    assertEquals(batch.load.tables.head.copyIn.sha256, copySha256)
    assertEquals(batch.capture.tables.head.copyOut.sha256, copySha256)
    assert(state.load.executed.contains(PostgreSqlEnumerationLiveDataV1Capture.LoadTransactionStatement))
    assert(state.load.executed.contains(PostgreSqlEnumerationLiveDataV1Capture.TimeoutStatement))
    assert(state.load.executed.contains(PostgreSqlEnumerationLiveDataV1Capture.CaptureSearchPathStatement))
    assert(state.capture.executed.contains(PostgreSqlEnumerationLiveDataV1Capture.CaptureTransactionStatement))
    assert(state.capture.executed.contains(PostgreSqlEnumerationLiveDataV1Capture.CaptureTextSettingsStatement))
    assert(state.load.closed && state.capture.closed)
  }

  test("an existing schema aborts and rolls back before any COPY or capture session") {
    val root = Files.createTempDirectory("yrw-live-existing-")
    Files.setPosixFilePermissions(root, PosixFilePermissions.fromString("rwx------"))
    val input = root.resolve("input.copy")
    Files.write(input, copyBytes)
    Files.setPosixFilePermissions(input, PosixFilePermissions.fromString("rw-------"))
    val output = root.resolve("copy-out")
    Files.createDirectory(output)
    Files.setPosixFilePermissions(output, PosixFilePermissions.fromString("rwx------"))
    val state = new JdbcState(schemaCount = 1L)
    val result = PostgreSqlEnumerationLiveDataV1Capture
      .forTest(() => state.open(), state.copyFactory)
      .capture(fixtureRequest(input), output)
    assertEquals(
      result.left.toOption,
      Some(PostgreSqlEnumerationLiveDataV1CaptureFailure.SchemaAlreadyExists)
    )
    assertEquals(state.connectionRequests, 1)
    assertEquals(state.load.commits, 0)
    assertEquals(state.load.rollbacks, 1)
    assertEquals(state.copyInCalls, Vector.empty)
    assertEquals(state.copyOutCalls, Vector.empty)
  }

  test("a capture-connection failure preserves the already committed load boundary") {
    val root = Files.createTempDirectory("yrw-live-post-commit-")
    Files.setPosixFilePermissions(root, PosixFilePermissions.fromString("rwx------"))
    val input = root.resolve("input.copy")
    Files.write(input, copyBytes)
    Files.setPosixFilePermissions(input, PosixFilePermissions.fromString("rw-------"))
    val output = root.resolve("copy-out")
    Files.createDirectory(output)
    Files.setPosixFilePermissions(output, PosixFilePermissions.fromString("rwx------"))

    val state = new JdbcState(schemaCount = 0L)
    val result = PostgreSqlEnumerationLiveDataV1Capture
      .forTest(
        () => {
          if (state.connectionRequests == 0) state.open()
          else {
            state.connectionRequests += 1
            throw new java.sql.SQLException("injected capture-connection failure")
          }
        },
        state.copyFactory
      )
      .capture(fixtureRequest(input), output)

    assertEquals(
      result.left.toOption,
      Some(PostgreSqlEnumerationLiveDataV1CaptureFailure.CaptureConnectionUnavailable)
    )
    assertEquals(state.connectionRequests, 2)
    assertEquals(state.load.commits, 1)
    assertEquals(state.load.rollbacks, 0)
    assert(state.load.closed)
    assertEquals(state.copyInCalls, Vector("copy-in"))
    assertEquals(state.copyOutCalls, Vector.empty)
    assert(!Files.exists(output.resolve("edge.copy")))
  }

  test("a lost commit acknowledgement remains outcome-indeterminate to the caller") {
    val root = Files.createTempDirectory("yrw-live-commit-ack-")
    Files.setPosixFilePermissions(root, PosixFilePermissions.fromString("rwx------"))
    val input = root.resolve("input.copy")
    Files.write(input, copyBytes)
    Files.setPosixFilePermissions(input, PosixFilePermissions.fromString("rw-------"))
    val output = root.resolve("copy-out")
    Files.createDirectory(output)
    Files.setPosixFilePermissions(output, PosixFilePermissions.fromString("rwx------"))

    val state = new JdbcState(
      schemaCount = 0L,
      loseLoadCommitAcknowledgement = true
    )
    val result = PostgreSqlEnumerationLiveDataV1Capture
      .forTest(() => state.open(), state.copyFactory)
      .capture(fixtureRequest(input), output)

    assertEquals(
      result.left.toOption,
      Some(PostgreSqlEnumerationLiveDataV1CaptureFailure.LoadCommitFailed)
    )
    assertEquals(state.connectionRequests, 1)
    assertEquals(state.load.commits, 1)
    // The best-effort rollback cannot prove whether PostgreSQL committed before
    // the acknowledgement was lost.  A campaign retry must use a fresh database.
    assertEquals(state.load.rollbacks, 1)
    assertEquals(state.copyInCalls, Vector("copy-in"))
    assertEquals(state.copyOutCalls, Vector.empty)
    assert(!Files.exists(output.resolve("edge.copy")))
  }

  test("post-commit connection cleanup failure cannot be treated as a rolled-back load") {
    val root = Files.createTempDirectory("yrw-live-post-commit-cleanup-")
    Files.setPosixFilePermissions(root, PosixFilePermissions.fromString("rwx------"))
    val input = root.resolve("input.copy")
    Files.write(input, copyBytes)
    Files.setPosixFilePermissions(input, PosixFilePermissions.fromString("rw-------"))
    val output = root.resolve("copy-out")
    Files.createDirectory(output)
    Files.setPosixFilePermissions(output, PosixFilePermissions.fromString("rwx------"))

    val state = new JdbcState(
      schemaCount = 0L,
      failLoadAutoCommitResetAfterCommit = true
    )
    val result = PostgreSqlEnumerationLiveDataV1Capture
      .forTest(() => state.open(), state.copyFactory)
      .capture(fixtureRequest(input), output)

    assertEquals(
      result.left.toOption,
      Some(PostgreSqlEnumerationLiveDataV1CaptureFailure.CleanupFailed)
    )
    assertEquals(state.connectionRequests, 1)
    assertEquals(state.load.commits, 1)
    assertEquals(state.load.rollbacks, 0)
    assertEquals(state.copyInCalls, Vector("copy-in"))
    assertEquals(state.copyOutCalls, Vector.empty)
    assert(!Files.exists(output.resolve("edge.copy")))
  }

  test("COPY input mismatch rolls back the all-table load and publishes no output bytes") {
    val root = Files.createTempDirectory("yrw-live-copy-mismatch-")
    Files.setPosixFilePermissions(root, PosixFilePermissions.fromString("rwx------"))
    val input = root.resolve("input.copy")
    Files.write(input, "9\t9\n".getBytes(StandardCharsets.UTF_8))
    Files.setPosixFilePermissions(input, PosixFilePermissions.fromString("rw-------"))
    val output = root.resolve("copy-out")
    Files.createDirectory(output)
    Files.setPosixFilePermissions(output, PosixFilePermissions.fromString("rwx------"))
    val state = new JdbcState(schemaCount = 0L)
    val result = PostgreSqlEnumerationLiveDataV1Capture
      .forTest(() => state.open(), state.copyFactory)
      .capture(fixtureRequest(input), output)
    assertEquals(
      result.left.toOption,
      Some(PostgreSqlEnumerationLiveDataV1CaptureFailure.CopyInputMismatch)
    )
    assertEquals(state.connectionRequests, 1)
    assertEquals(state.load.commits, 0)
    assertEquals(state.load.rollbacks, 1)
    assert(!Files.exists(output.resolve("edge.copy")))
  }

  private def fixtureRequest(input: Path): PostgreSqlEnumerationLiveDataV1Request = {
    def raw(sql: String, columns: Vector[String], rows: Int = 1) =
      PostgreSqlEnumerationLiveDataV1RawQuery(sql, columns, rows)
    val tableQueries = PostgreSqlEnumerationLiveDataV1TableQueries(
      "edge",
      raw("relation", Vector("value")),
      raw("columns", Vector("value")),
      raw("constraints", Vector("definition")),
      raw("indexes", Vector("definition")),
      raw("inheritance", Vector("definition")),
      raw("policies", Vector("definition")),
      raw("rules", Vector("definition")),
      raw("triggers", Vector("definition")),
      raw("stats", Vector("value")),
      raw("extended", Vector("definition"))
    )
    val queries = PostgreSqlEnumerationLiveDataV1RawQueries(
      raw("provenance", Vector("value")),
      raw("server", Vector("value")),
      raw("database", Vector("value")),
      raw("snapshot", Vector("value")),
      raw("schema", Vector("value")),
      raw("owner", Vector("value")),
      raw("relations", Vector("value")),
      Vector(tableQueries)
    )
    PostgreSqlEnumerationLiveDataV1Request(
      "fixture_schema",
      "create-schema",
      Vector(
        PostgreSqlEnumerationLiveDataV1TableRequest(
          "edge",
          "0" * 64,
          "create-table",
          "copy-in",
          "copy-out",
          Vector("set-statistics"),
          "analyze",
          input.toAbsolutePath.normalize(),
          PostgreSqlEnumerationLiveDataV1Content(
            copyBytes.length.toLong,
            1L,
            copySha256,
            PostgreSqlEnumerationLiveDataV1Capture.CopyEncoding
          )
        )
      ),
      queries
    )
  }

  private final class ConnectionState(val captureSession: Boolean) {
    var autoCommit = true
    var closed = false
    var commits = 0
    var rollbacks = 0
    var executed = Vector.empty[String]
  }

  private final class JdbcState(
      schemaCount: Long,
      loseLoadCommitAcknowledgement: Boolean = false,
      failLoadAutoCommitResetAfterCommit: Boolean = false
  ) {
    val load = new ConnectionState(captureSession = false)
    val capture = new ConnectionState(captureSession = true)
    var connectionRequests = 0
    var copyInCalls = Vector.empty[String]
    var copyOutCalls = Vector.empty[String]

    val copyFactory = new PostgreSqlEnumerationLiveDataV1Capture.CopyApiFactory {
      override def forConnection(
          connection: Connection
      ): Either[PostgreSqlEnumerationLiveDataV1CaptureFailure, PostgreSqlEnumerationLiveDataV1Capture.CopyApi] =
        Right(new PostgreSqlEnumerationLiveDataV1Capture.CopyApi {
          override def copyIn(sql: String, input: InputStream): Long = {
            copyInCalls :+= sql
            val sink = new ByteArrayOutputStream()
            input.transferTo(sink)
            sink.toByteArray.count(_ == '\n'.toByte).toLong
          }
          override def copyOut(sql: String, output: OutputStream): Long = {
            copyOutCalls :+= sql
            output.write(copyBytes)
            1L
          }
        })
    }

    def open(): Connection = {
      val state = if (connectionRequests == 0) load else capture
      connectionRequests += 1
      proxy(classOf[Connection]) {
        case ("isClosed", _) => java.lang.Boolean.valueOf(state.closed)
        case ("getAutoCommit", _) => java.lang.Boolean.valueOf(state.autoCommit)
        case ("setAutoCommit", args) =>
          val value = args.head.asInstanceOf[java.lang.Boolean].booleanValue()
          if (
            !state.captureSession && value && state.commits > 0 &&
            failLoadAutoCommitResetAfterCommit
          ) throw new java.sql.SQLException("injected post-commit cleanup failure")
          state.autoCommit = value
          null
        case ("createStatement", _) => statement(state)
        case ("prepareStatement", _) => preparedStatement(state)
        case ("commit", _) =>
          state.commits += 1
          if (!state.captureSession && loseLoadCommitAcknowledgement)
            throw new java.sql.SQLException("injected lost commit acknowledgement")
          null
        case ("rollback", _) => state.rollbacks += 1; null
        case ("close", _) => state.closed = true; null
      }
    }

    private def statement(state: ConnectionState): Statement = proxy(classOf[Statement]) {
      case ("setQueryTimeout", _) => null
      case ("setMaxRows", _)      => null
      case ("execute", args) =>
        state.executed :+= args.head.asInstanceOf[String]
        java.lang.Boolean.FALSE
      case ("executeQuery", args) =>
        val sql = args.head.asInstanceOf[String]
        if (sql == PostgreSqlEnumerationLiveDataV1Capture.PostgreSqlContractStatement) {
          val isolation = if (state.captureSession) "repeatable read" else "read committed"
          val readOnly = if (state.captureSession) "on" else "off"
          resultSet(
            PostgreSqlEnumerationLiveDataV1Capture.PostgreSqlContractColumns,
            Vector(Vector(
              "180004", "UTF8", "UTF8", "fixture_database", "123456789",
              "300000", "30000", isolation, readOnly, "pg_catalog, pg_temp"
            ))
          )
        } else {
          val definitionQuery = sql match {
            case "constraints" | "indexes" | "inheritance" | "policies" |
                "rules" | "triggers" | "extended" => true
            case _ => false
          }
          val columns = if (definitionQuery) Vector("definition") else Vector("value")
          val queryRows = if (definitionQuery) Vector.empty else Vector(Vector("fixture"))
          resultSet(columns, queryRows)
        }
      case ("close", _) => null
    }

    private def preparedStatement(state: ConnectionState): PreparedStatement =
      proxy(classOf[PreparedStatement]) {
        case ("setQueryTimeout", _) => null
        case ("setMaxRows", _)      => null
        case ("setString", _)       => null
        case ("executeQuery", _) =>
          resultSet(Vector("count"), Vector(Vector(java.lang.Long.toString(schemaCount))))
        case ("close", _) => null
      }

    private def resultSet(columns: Vector[String], rows: Vector[Vector[String]]): ResultSet = {
      var position = -1
      var wasNull = false
      proxy(classOf[ResultSet]) {
        case ("next", _) =>
          position += 1; java.lang.Boolean.valueOf(position < rows.size)
        case ("getString", args) =>
          val value = rows(position)(args.head.asInstanceOf[java.lang.Integer].intValue() - 1)
          wasNull = value == null
          value
        case ("getLong", args) =>
          val value = rows(position)(args.head.asInstanceOf[java.lang.Integer].intValue() - 1)
          wasNull = value == null
          java.lang.Long.valueOf(if (value == null) 0L else value.toLong)
        case ("wasNull", _) => java.lang.Boolean.valueOf(wasNull)
        case ("getMetaData", _) => proxy(classOf[ResultSetMetaData]) {
          case ("getColumnCount", _) => java.lang.Integer.valueOf(columns.size)
          case ("getColumnLabel", args) =>
            columns(args.head.asInstanceOf[java.lang.Integer].intValue() - 1)
        }
        case ("close", _) => null
      }
    }
  }

  private def proxy[A](interface: Class[A])(
      handler: PartialFunction[(String, Vector[AnyRef]), AnyRef]
  ): A =
    Proxy.newProxyInstance(
      interface.getClassLoader,
      Array(interface),
      new InvocationHandler {
        override def invoke(proxy: Any, method: Method, arguments: Array[AnyRef]): AnyRef = {
          val args = Option(arguments).fold(Vector.empty[AnyRef])(_.toVector)
          handler.applyOrElse[(String, Vector[AnyRef]), AnyRef](
            method.getName -> args,
            _ => defaultValue(method.getReturnType)
          )
        }
      }
    ).asInstanceOf[A]

  private def defaultValue(returnType: Class[_]): AnyRef =
    if (!returnType.isPrimitive) null
    else if (returnType == java.lang.Boolean.TYPE) java.lang.Boolean.FALSE
    else if (returnType == java.lang.Integer.TYPE) java.lang.Integer.valueOf(0)
    else if (returnType == java.lang.Long.TYPE) java.lang.Long.valueOf(0L)
    else null
}

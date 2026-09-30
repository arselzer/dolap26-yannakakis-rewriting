package at.ac.tuwien.dbai.yannakakis.frontend.calcite

import java.io.PrintWriter
import java.lang.reflect.{InvocationHandler, Method, Proxy}
import java.sql.{
  Connection,
  DatabaseMetaData,
  ResultSet,
  SQLException,
  SQLFeatureNotSupportedException
}
import java.util.logging.Logger
import javax.sql.DataSource

import scala.collection.mutable

class CalciteExactJdbcMetadataDataSourceSuite extends munit.FunSuite {
  test("schema table and column metadata patterns are literal identifiers") {
    val calls = mutable.ArrayBuffer.empty[(String, Vector[AnyRef])]
    val tableRows = proxy(classOf[ResultSet]) { case ("close", _) => null }
    val columnRows = proxy(classOf[ResultSet]) { case ("close", _) => null }
    val metadata = proxy(classOf[DatabaseMetaData]) {
      case ("getSearchStringEscape", _) => "!"
      case ("getTables", arguments) =>
        calls += "getTables" -> arguments
        tableRows
      case ("getColumns", arguments) =>
        calls += "getColumns" -> arguments
        columnRows
    }
    var connectionCloseAttempts = 0
    val connection = proxy(classOf[Connection]) {
      case ("getMetaData", _) => metadata
      case ("close", _) =>
        connectionCloseAttempts += 1
        null
    }
    val delegate = new StubDataSource(connection)
    val dataSource = CalciteExactJdbcMetadataDataSource.wrap(delegate)
    val wrappedConnection = dataSource.getConnection
    val wrappedMetadata = wrappedConnection.getMetaData
    val tableTypes = Array("TABLE", "VIEW")

    val actualTables = wrappedMetadata.getTables(
      "catalog_%",
      "s_a%!tail",
      "t_%!one",
      tableTypes
    )
    val actualColumns = wrappedMetadata.getColumns(
      "catalog_%",
      "s_a%!tail",
      "t_%!one",
      "c!_%"
    )

    assert(actualTables eq tableRows)
    assert(actualColumns eq columnRows)
    assertEquals(calls.map(_._1).toVector, Vector("getTables", "getColumns"))
    val tableArguments = calls(0)._2
    assertEquals(
      tableArguments.take(3),
      Vector("catalog_%", "s!_a!%!!tail", "t!_!%!!one")
    )
    assert(tableArguments(3) eq tableTypes)
    assertEquals(
      calls(1)._2,
      Vector("catalog_%", "s!_a!%!!tail", "t!_!%!!one", "c!!!_!%")
    )

    assert(dataSource.isWrapperFor(classOf[DataSource]))
    assert(dataSource.unwrap(classOf[DataSource]) eq dataSource)
    assert(wrappedConnection.isWrapperFor(classOf[Connection]))
    assert(wrappedConnection.unwrap(classOf[Connection]) eq wrappedConnection)
    assert(wrappedConnection == wrappedConnection)
    assertEquals(
      wrappedConnection.hashCode(),
      System.identityHashCode(wrappedConnection)
    )
    assert(wrappedMetadata.isWrapperFor(classOf[DatabaseMetaData]))
    assert(
      wrappedMetadata.unwrap(classOf[DatabaseMetaData]) eq wrappedMetadata
    )
    assert(wrappedMetadata == wrappedMetadata)
    assertEquals(
      wrappedMetadata.hashCode(),
      System.identityHashCode(wrappedMetadata)
    )
    assert(wrappedMetadata.getConnection eq wrappedConnection)

    wrappedConnection.close()
    assertEquals(connectionCloseAttempts, 1)
    assertEquals(delegate.closeAttempts, 0)
  }

  test("null patterns remain null and credential connections are wrapped") {
    val calls = mutable.ArrayBuffer.empty[Vector[AnyRef]]
    val rows = proxy(classOf[ResultSet]) { case ("close", _) => null }
    val metadata = proxy(classOf[DatabaseMetaData]) {
      case ("getSearchStringEscape", _) => "\\"
      case ("getTables", arguments) =>
        calls += arguments
        rows
    }
    val connection = proxy(classOf[Connection]) {
      case ("getMetaData", _) => metadata
    }
    val delegate = new StubDataSource(connection)
    val dataSource = CalciteExactJdbcMetadataDataSource.wrap(delegate)
    val wrapped = dataSource.getConnection("user", "password")

    assert(wrapped ne connection)
    assert(wrapped.getMetaData.getTables(null, null, null, null) eq rows)
    assertEquals(calls.toVector, Vector(Vector(null, null, null, null)))
    assertEquals(delegate.connectionRequests, 0)
    assertEquals(delegate.credentialConnectionRequests, 1)
  }

  test("metadata SQLExceptions retain their exact type and identity") {
    val failure = new SQLException("sentinel metadata failure")
    val metadata = proxy(classOf[DatabaseMetaData]) {
      case ("getSearchStringEscape", _) => "\\"
      case ("getTables", _)             => throw failure
    }
    val connection = proxy(classOf[Connection]) {
      case ("getMetaData", _) => metadata
    }
    val wrapped = CalciteExactJdbcMetadataDataSource
      .wrap(new StubDataSource(connection))
      .getConnection
      .getMetaData

    val actual = intercept[SQLException] {
      wrapped.getTables(null, "s_a", null, null)
    }
    assert(actual eq failure)
  }

  test("an empty driver search escape fails closed before metadata lookup") {
    var tableLookups = 0
    val metadata = proxy(classOf[DatabaseMetaData]) {
      case ("getSearchStringEscape", _) => ""
      case ("getTables", _) =>
        tableLookups += 1
        proxy(classOf[ResultSet]) { case ("close", _) => null }
    }
    val connection = proxy(classOf[Connection]) {
      case ("getMetaData", _) => metadata
    }
    val wrapped = CalciteExactJdbcMetadataDataSource
      .wrap(new StubDataSource(connection))
      .getConnection
      .getMetaData

    val failure = intercept[SQLException] {
      wrapped.getTables(null, "schema", null, null)
    }
    assertEquals(
      failure.getMessage,
      "JDBC metadata search-string escape must not be empty"
    )
    assertEquals(tableLookups, 0)
  }

  private final class StubDataSource(connection: Connection)
      extends DataSource
      with AutoCloseable {
    var connectionRequests: Int = 0
    var credentialConnectionRequests: Int = 0
    var closeAttempts: Int = 0

    override def getConnection: Connection = {
      connectionRequests += 1
      connection
    }

    override def getConnection(
        username: String,
        password: String
    ): Connection = {
      credentialConnectionRequests += 1
      connection
    }

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
        val arguments =
          Option(rawArguments).fold(Vector.empty[AnyRef])(_.toVector)
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
}

package at.ac.tuwien.dbai.yannakakis.frontend.calcite

import java.io.PrintWriter
import java.lang.reflect.{
  InvocationHandler,
  InvocationTargetException,
  Method,
  Proxy
}
import java.sql.{Connection, DatabaseMetaData, SQLException}
import java.util.logging.Logger
import javax.sql.DataSource

/** Planner-only JDBC metadata boundary with exact identifier semantics.
  *
  * JDBC names the schema, table, and column arguments to `getTables` and
  * `getColumns` as search patterns. Calcite's `JdbcSchema` supplies physical
  * identifiers in those positions without escaping `%` or `_`, so an ordinary
  * PostgreSQL identifier can otherwise bind metadata from a different object.
  * This wrapper delegates every operation except those metadata calls and
  * converts their non-null pattern arguments to literal patterns using the
  * driver's own search-string escape.
  *
  * The wrapper is deliberately used only to construct Calcite's planning
  * schema. Runtime evaluators retain the caller's original `DataSource`, and
  * neither this wrapper nor the planner owns or closes that data source.
  */
private[calcite] object CalciteExactJdbcMetadataDataSource {
  def wrap(delegate: DataSource): DataSource = {
    require(delegate != null, "JDBC metadata data source must not be null")
    new ExactMetadataDataSource(delegate)
  }

  private final class ExactMetadataDataSource(delegate: DataSource)
      extends DataSource {
    override def getConnection: Connection =
      wrapConnection(delegate.getConnection)

    override def getConnection(
        username: String,
        password: String
    ): Connection =
      wrapConnection(delegate.getConnection(username, password))

    override def getLogWriter: PrintWriter = delegate.getLogWriter

    override def setLogWriter(out: PrintWriter): Unit =
      delegate.setLogWriter(out)

    override def setLoginTimeout(seconds: Int): Unit =
      delegate.setLoginTimeout(seconds)

    override def getLoginTimeout: Int = delegate.getLoginTimeout

    override def getParentLogger: Logger = delegate.getParentLogger

    override def unwrap[T](iface: Class[T]): T =
      if (iface != null && iface.isInstance(this)) iface.cast(this)
      else delegate.unwrap(iface)

    override def isWrapperFor(iface: Class[_]): Boolean =
      (iface != null && iface.isInstance(this)) || delegate.isWrapperFor(iface)
  }

  private def wrapConnection(delegate: Connection): Connection = {
    if (delegate == null) return null

    val handler = new InvocationHandler {
      private var wrappedMetadata: DatabaseMetaData = _

      override def invoke(
          proxyValue: Any,
          method: Method,
          rawArguments: Array[AnyRef]
      ): AnyRef = {
        val arguments = argumentsOrEmpty(rawArguments)
        method.getName match {
          case "getMetaData" if arguments.isEmpty =>
            if (wrappedMetadata == null) {
              wrappedMetadata = wrapMetadata(
                delegate.getMetaData,
                proxyValue.asInstanceOf[Connection]
              )
            }
            wrappedMetadata
          case "unwrap" if wrapperRequested(proxyValue, arguments) =>
            proxyValue.asInstanceOf[AnyRef]
          case "isWrapperFor" if wrapperRequested(proxyValue, arguments) =>
            java.lang.Boolean.TRUE
          case "equals" if arguments.size == 1 =>
            java.lang.Boolean.valueOf(
              proxyValue.asInstanceOf[AnyRef] eq arguments.head
            )
          case "hashCode" if arguments.isEmpty =>
            java.lang.Integer.valueOf(System.identityHashCode(proxyValue))
          case _ => invokeDelegate(delegate, method, arguments)
        }
      }
    }
    Proxy
      .newProxyInstance(
        classOf[Connection].getClassLoader,
        Array(classOf[Connection]),
        handler
      )
      .asInstanceOf[Connection]
  }

  private def wrapMetadata(
      delegate: DatabaseMetaData,
      wrappedConnection: Connection
  ): DatabaseMetaData = {
    if (delegate == null) return null

    val handler = new InvocationHandler {
      override def invoke(
          proxyValue: Any,
          method: Method,
          rawArguments: Array[AnyRef]
      ): AnyRef = {
        val arguments = argumentsOrEmpty(rawArguments)
        method.getName match {
          case "getConnection" if arguments.isEmpty => wrappedConnection
          case "getTables" if arguments.size == 4 =>
            invokeDelegate(
              delegate,
              method,
              literalPatternArguments(delegate, arguments, Vector(1, 2))
            )
          case "getColumns" if arguments.size == 4 =>
            invokeDelegate(
              delegate,
              method,
              literalPatternArguments(delegate, arguments, Vector(1, 2, 3))
            )
          case "unwrap" if wrapperRequested(proxyValue, arguments) =>
            proxyValue.asInstanceOf[AnyRef]
          case "isWrapperFor" if wrapperRequested(proxyValue, arguments) =>
            java.lang.Boolean.TRUE
          case "equals" if arguments.size == 1 =>
            java.lang.Boolean.valueOf(
              proxyValue.asInstanceOf[AnyRef] eq arguments.head
            )
          case "hashCode" if arguments.isEmpty =>
            java.lang.Integer.valueOf(System.identityHashCode(proxyValue))
          case _ => invokeDelegate(delegate, method, arguments)
        }
      }
    }
    Proxy
      .newProxyInstance(
        classOf[DatabaseMetaData].getClassLoader,
        Array(classOf[DatabaseMetaData]),
        handler
      )
      .asInstanceOf[DatabaseMetaData]
  }

  private def literalPatternArguments(
      metadata: DatabaseMetaData,
      arguments: Vector[AnyRef],
      patternIndexes: Vector[Int]
  ): Vector[AnyRef] = {
    val escape = Option(metadata.getSearchStringEscape)
      .filter(_.nonEmpty)
      .getOrElse(
        throw new SQLException(
          "JDBC metadata search-string escape must not be empty"
        )
      )
    patternIndexes.foldLeft(arguments) { case (current, index) =>
      current(index) match {
        case value: String =>
          current.updated(index, literalPattern(value, escape))
        case null          => current
        case _ =>
          throw new SQLException(
            "JDBC metadata search pattern must be text or null"
          )
      }
    }
  }

  private def literalPattern(value: String, escape: String): String = {
    val result = new java.lang.StringBuilder(value.length)
    var index = 0
    while (index < value.length) {
      if (value.startsWith(escape, index)) {
        result.append(escape).append(escape)
        index += escape.length
      } else {
        val character = value.charAt(index)
        if (character == '_' || character == '%') result.append(escape)
        result.append(character)
        index += 1
      }
    }
    result.toString
  }

  private def wrapperRequested(
      proxyValue: Any,
      arguments: Vector[AnyRef]
  ): Boolean =
    arguments match {
      case Vector(iface: Class[_]) => iface.isInstance(proxyValue)
      case _                       => false
    }

  private def argumentsOrEmpty(
      rawArguments: Array[AnyRef]
  ): Vector[AnyRef] =
    Option(rawArguments).fold(Vector.empty[AnyRef])(_.toVector)

  private def invokeDelegate(
      delegate: AnyRef,
      method: Method,
      arguments: Vector[AnyRef]
  ): AnyRef =
    try method.invoke(delegate, arguments: _*)
    catch {
      case problem: InvocationTargetException =>
        throw Option(problem.getCause).getOrElse(problem)
    }
}

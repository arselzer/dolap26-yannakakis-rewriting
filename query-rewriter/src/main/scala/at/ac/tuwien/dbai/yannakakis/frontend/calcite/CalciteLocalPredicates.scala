package at.ac.tuwien.dbai.yannakakis.frontend.calcite

import org.apache.calcite.rex.{RexBuilder, RexCall, RexInputRef, RexLiteral, RexNode, RexUtil}
import org.apache.calcite.sql._
import org.apache.calcite.sql.fun.SqlBetweenOperator
import org.apache.calcite.sql.`type`.SqlTypeName
import org.apache.calcite.util.TimestampString
import scala.jdk.CollectionConverters._
import scala.util.Try

/** Bounded, immutable predicate syntax shared by source certification and typed
  * PostgreSQL emission. Diagnostic SQL strings/digests are never parsed.
  * Raw syntax and Rex syntax are independently converted and compared after
  * resolving source aliases to occurrence/column identities.
  */
private[yannakakis] object CalciteLocalPredicates {
  val CapabilityId = "occurrence-local-predicates-v1"
  private val MaximumDepth = 64
  private val MaximumNodes = 512

  sealed abstract class Operator(val kind: SqlKind, val sql: String) extends Product with Serializable
  case object Equal extends Operator(SqlKind.EQUALS, "=")
  case object NotEqual extends Operator(SqlKind.NOT_EQUALS, "<>")
  case object Less extends Operator(SqlKind.LESS_THAN, "<")
  case object LessEqual extends Operator(SqlKind.LESS_THAN_OR_EQUAL, "<=")
  case object Greater extends Operator(SqlKind.GREATER_THAN, ">")
  case object GreaterEqual extends Operator(SqlKind.GREATER_THAN_OR_EQUAL, ">=")
  case object Like extends Operator(SqlKind.LIKE, "LIKE")
  private val operators = Vector(Equal, NotEqual, Less, LessEqual, Greater, GreaterEqual, Like)
    .map(op => op.kind -> op).toMap

  sealed trait Term[+C] extends Product with Serializable
  final case class Column[C](value: C) extends Term[C]
  sealed trait Literal extends Term[Nothing]
  final case class Integral(value: Long) extends Literal
  final case class Text(value: String) extends Literal {
    require(value != null && !value.contains('\\') && !value.contains('\u0000'),
      "text literals containing backslashes or NUL are outside this capability")
  }
  final case class Timestamp(value: String) extends Literal {
    require(value != null && !value.contains('.') && new TimestampString(value).toString == value,
      "timestamp must be a canonical whole-second literal")
  }

  sealed trait Tree[+C] extends Product with Serializable {
    def termCount: Int = this match {
      case Junction(_, terms) => 1 + terms.map(_.termCount).sum
      case _ => 1
    }
    def columns: Vector[C] = this match {
      case Comparison(_, left, right) => Vector(left, right).collect { case Column(c) => c }
      case NullTest(column, _) => Vector(column)
      case BooleanColumn(column) => Vector(column)
      case Junction(_, terms) => terms.flatMap(_.columns)
    }
    def mapColumns[D](f: C => D): Tree[D] = {
      def term(t: Term[C]): Term[D] = t match {
        case Column(c) => Column(f(c))
        case literal: Literal => literal
      }
      this match {
        case Comparison(op, left, right) => Comparison(op, term(left), term(right))
        case NullTest(column, not) => NullTest(f(column), not)
        case BooleanColumn(column) => BooleanColumn(f(column))
        case Junction(and, terms) => Junction(and, terms.map(_.mapColumns(f)))
      }
    }
  }
  final case class Comparison[C](operator: Operator, left: Term[C], right: Term[C]) extends Tree[C]
  final case class NullTest[C](column: C, not: Boolean) extends Tree[C]
  final case class BooleanColumn[C](column: C) extends Tree[C]
  final case class Junction[C](and: Boolean, terms: Vector[Tree[C]]) extends Tree[C]
  final case class SourceColumn(alias: String, name: String)
  final case class SourceOccurrence(name: Vector[String], alias: String)
  final case class SourceCertificate private[calcite] (
      occurrences: Vector[SourceOccurrence], conditions: Vector[Tree[SourceColumn]])

  private def bounded[C](tree: Tree[C]): Boolean = {
    var remaining = MaximumNodes
    def visit(node: Tree[C], depth: Int): Boolean = {
      remaining -= 1
      remaining >= 0 && depth <= MaximumDepth && (node match {
        case Junction(_, terms) => terms.nonEmpty && terms.forall(visit(_, depth + 1))
        case _ => true
      })
    }
    visit(tree, 0)
  }

  private def normalize[C](tree: Tree[C]): Tree[C] = tree match {
    case Junction(and, terms) =>
      val flat = terms.map(normalize).flatMap {
        case Junction(same, children) if same == and => children
        case other => Vector(other)
      }.distinct.sortBy(_.toString)
      if (flat.size == 1) flat.head else Junction(and, flat)
    case other => other
  }

  private def column(node: SqlNode): SourceColumn = node match {
    case id: SqlIdentifier if !id.isStar && id.names.size == 2 =>
      SourceColumn(id.names.get(0), id.names.get(1))
    case _ => throw new IllegalArgumentException("expected qualified direct source column")
  }

  private def sourceTerm(node: SqlNode): Term[SourceColumn] = node match {
    case id: SqlIdentifier => Column(column(id))
    case n: SqlNumericLiteral if n.isExact => Integral(n.bigDecimalValue().longValueExact())
    case s: SqlCharStringLiteral => Text(s.getNlsString.getValue)
    case c: SqlCall if c.getKind == SqlKind.MINUS_PREFIX && c.operandCount() == 1 =>
      sourceTerm(c.operand[SqlNode](0)) match {
        case Integral(value) => Integral(Math.negateExact(value))
        case _ => throw new IllegalArgumentException("only integral unary minus is supported")
      }
    case c: SqlCall if c.getKind == SqlKind.CAST && c.operandCount() == 2 =>
      (c.operand[SqlNode](0), c.operand[SqlNode](1)) match {
        case (s: SqlCharStringLiteral, t: SqlDataTypeSpec)
            if t.getTypeName.getSimple == "TIMESTAMP" &&
              t.getTypeNameSpec.isInstanceOf[SqlBasicTypeNameSpec] &&
              t.getTypeNameSpec.asInstanceOf[SqlBasicTypeNameSpec].getPrecision < 0 =>
          Timestamp(new TimestampString(s.getNlsString.getValue).toString)
        case _ => throw new IllegalArgumentException("only timestamp literal casts are supported")
      }
    case _ => throw new IllegalArgumentException("unsupported source literal")
  }

  private def sourceTree(node: SqlNode, depth: Int = 0): Tree[SourceColumn] = {
    require(depth <= MaximumDepth, "local predicate depth exceeded")
    node match {
      case id: SqlIdentifier => BooleanColumn(column(id))
      case c: SqlCall if (c.getKind == SqlKind.AND || c.getKind == SqlKind.OR) && c.operandCount() >= 2 =>
        Junction(c.getKind == SqlKind.AND, c.getOperandList.asScala.toVector.map(sourceTree(_, depth + 1)))
      case c: SqlCall if operators.contains(c.getKind) && c.operandCount() == 2 =>
        Comparison(operators(c.getKind), sourceTerm(c.operand[SqlNode](0)), sourceTerm(c.operand[SqlNode](1)))
      case c: SqlCall if (c.getKind == SqlKind.IS_NULL || c.getKind == SqlKind.IS_NOT_NULL) && c.operandCount() == 1 =>
        NullTest(column(c.operand[SqlNode](0)), c.getKind == SqlKind.IS_NOT_NULL)
      case c: SqlCall if c.getKind == SqlKind.IN && c.operandCount() == 2 =>
        val left = sourceTerm(c.operand[SqlNode](0))
        c.operand[SqlNode](1) match {
          case list: SqlNodeList if list.size > 0 && list.size <= MaximumNodes =>
            val literals = list.asScala.toVector.map(sourceTerm)
            require(literals.forall(_.isInstanceOf[Literal]), "IN must contain only literal values")
            Junction(false, literals.map(value => Comparison(Equal, left, value)))
          case _ => throw new IllegalArgumentException("IN subqueries are not supported")
        }
      case c: SqlCall if c.getOperator.isInstanceOf[SqlBetweenOperator] && c.operandCount() == 3 =>
        val op = c.getOperator.asInstanceOf[SqlBetweenOperator]
        require(!op.isNegated && op.flag == SqlBetweenOperator.Flag.ASYMMETRIC)
        val value = sourceTerm(c.operand[SqlNode](0))
        Junction(true, Vector(
          Comparison(GreaterEqual, value, sourceTerm(c.operand[SqlNode](1))),
          Comparison(LessEqual, value, sourceTerm(c.operand[SqlNode](2)))))
      case _ => throw new IllegalArgumentException("unsupported source predicate")
    }
  }

  /** Copy immutable values before Calcite validation can mutate the SqlNode tree. */
  def inspect(node: SqlNode): Option[SourceCertificate] = Try {
    val select = node match {
      case s: SqlSelect => s
      case _ => throw new IllegalArgumentException("local predicates require a top-level SELECT")
    }
    val occurrences = Vector.newBuilder[SourceOccurrence]
    val conditions = Vector.newBuilder[Tree[SourceColumn]]
    def from(n: SqlNode): Unit = n match {
      case join: SqlJoin if Set(JoinType.INNER, JoinType.COMMA, JoinType.CROSS)(join.getJoinType) && !join.isNatural =>
        require(join.getConditionType != JoinConditionType.USING)
        from(join.getLeft); from(join.getRight)
        if (join.getCondition != null) conditions += sourceTree(join.getCondition)
      case id: SqlIdentifier if !id.isStar =>
        occurrences += SourceOccurrence(id.names.asScala.toVector, id.names.asScala.last)
      case alias: SqlCall if alias.getKind == SqlKind.AS && alias.operandCount() == 2 =>
        (alias.operand[SqlNode](0), alias.operand[SqlNode](1)) match {
          case (id: SqlIdentifier, name: SqlIdentifier) if !id.isStar && name.isSimple =>
            occurrences += SourceOccurrence(id.names.asScala.toVector, name.getSimple)
          case _ => throw new IllegalArgumentException("only named base relations are supported")
        }
      case _ => throw new IllegalArgumentException("only named base relations are supported")
    }
    from(select.getFrom)
    if (select.getWhere != null) conditions += sourceTree(select.getWhere)
    val result = SourceCertificate(occurrences.result(), conditions.result())
    require(result.occurrences.map(_.alias).distinct.size == result.occurrences.size)
    require(result.conditions.nonEmpty && result.conditions.forall(bounded))
    result
  }.toOption

  private def rexTerm(node: RexNode, input: Vector[OutputSlot]): Term[ColumnRef] = node match {
    case ref: RexInputRef => input(ref.getIndex).expression match {
      case c: ScalarExpression.Column => Column(c.ref)
      case _ => throw new IllegalArgumentException("indirect predicate column")
    }
    case lit: RexLiteral if !lit.isNull => lit.getType.getSqlTypeName match {
      case SqlTypeName.SMALLINT | SqlTypeName.INTEGER | SqlTypeName.BIGINT | SqlTypeName.DECIMAL =>
        Integral(lit.getValueAs(classOf[java.math.BigDecimal]).longValueExact())
      case SqlTypeName.CHAR | SqlTypeName.VARCHAR => Text(lit.getValueAs(classOf[String]))
      case SqlTypeName.TIMESTAMP => Timestamp(lit.getValueAs(classOf[TimestampString]).toString)
      case _ => throw new IllegalArgumentException("unsupported predicate literal type")
    }
    case c: RexCall if c.getKind == SqlKind.CAST && c.operands.size == 1 &&
        c.getType.getSqlTypeName == SqlTypeName.TIMESTAMP =>
      rexTerm(c.operands.get(0), input) match {
        case Text(value) => Timestamp(new TimestampString(value).toString)
        case value => value
      }
    // Only concordance with the *uncast source operand* can authorize removal
    // of a Calcite-inserted coercion. Explicit source casts remain rejected.
    case c: RexCall if c.getKind == SqlKind.CAST && c.operands.size == 1 => rexTerm(c.operands.get(0), input)
    case _ => throw new IllegalArgumentException("unsupported Rex operand")
  }

  def fromRex(node: RexNode, input: Vector[OutputSlot], builder: RexBuilder): Option[Tree[ColumnRef]] = Try {
    require(RexUtil.isDeterministic(node), "predicate is not deterministic")
    val expanded = RexUtil.expandSearch(builder, null, node)
    def tree(n: RexNode, depth: Int): Tree[ColumnRef] = {
      require(depth <= MaximumDepth)
      n match {
        case ref: RexInputRef => rexTerm(ref, input) match {
          case Column(c) if ref.getType.getSqlTypeName == SqlTypeName.BOOLEAN => BooleanColumn(c)
          case _ => throw new IllegalArgumentException("bare predicate must be BOOLEAN")
        }
        case c: RexCall if (c.getKind == SqlKind.AND || c.getKind == SqlKind.OR) && c.operands.size >= 2 =>
          Junction(c.getKind == SqlKind.AND, c.operands.asScala.toVector.map(tree(_, depth + 1)))
        case c: RexCall if operators.contains(c.getKind) && c.operands.size == 2 =>
          val op = operators(c.getKind)
          require(c.getOperator.getName == op.sql, "noncanonical predicate operator")
          Comparison(op, rexTerm(c.operands.get(0), input), rexTerm(c.operands.get(1), input))
        case c: RexCall if (c.getKind == SqlKind.IS_NULL || c.getKind == SqlKind.IS_NOT_NULL) && c.operands.size == 1 =>
          rexTerm(c.operands.get(0), input) match {
            case Column(col) => NullTest(col, c.getKind == SqlKind.IS_NOT_NULL)
            case _ => throw new IllegalArgumentException("NULL test requires a direct column")
          }
        case _ => throw new IllegalArgumentException("unsupported Rex predicate")
      }
    }
    val result = tree(expanded, 0)
    require(bounded(result))
    normalize(result)
  }.toOption

  private def scalarTree(expression: ScalarExpression): Option[Tree[ColumnRef]] = expression match {
    case p: ScalarExpression.LocalPredicate => Some(p.tree)
    case c: ScalarExpression.Column => Some(BooleanColumn(c.ref))
    case ScalarExpression.Call("EQUALS", "=", Vector(a: ScalarExpression.Column, b: ScalarExpression.Column), _, _, _, true) =>
      Some(Comparison(Equal, Column(a.ref), Column(b.ref)))
    case _ => None
  }

  /** Replayed by both analyzer and renderer; no predicate may be dropped. */
  def concordant(source: SourceCertificate, query: QueryIR): Boolean = Try {
    require(source.occurrences.size == query.relations.size)
    val byAlias = source.occurrences.zip(query.relations).map { case (s, r) =>
      require(r.qualifiedName.endsWith(s.name))
      s.alias -> r
    }.toMap
    require(byAlias.size == source.occurrences.size)
    val expected = source.conditions.map(_.mapColumns { col =>
      val matches = byAlias(col.alias).columns.filter(_.sourceName == col.name)
      require(matches.size == 1)
      matches.head.ref
    })
    val actual = query.predicates.map(p => scalarTree(p.expression).get)
    require(expected.forall(bounded) && actual.forall(bounded))
    normalize(Junction(true, expected)) == normalize(Junction(true, actual))
  }.getOrElse(false)

  def validLocal(tree: Tree[ColumnRef], metadata: Map[ColumnRef, ColumnMetadata]): Boolean = {
    def family(sqlType: String): Option[String] = sqlType match {
      case "SMALLINT" | "INTEGER" | "BIGINT" => Some("integral")
      case "CHAR" | "VARCHAR" => Some("text")
      // DATE -> timestamp without time zone is midnight, independent of the
      // session timezone. The emitted source comparison retains the DATE
      // column and explicit TIMESTAMP literal, just as PostgreSQL resolves it.
      case "DATE" | "TIMESTAMP" => Some("timestamp")
      case "BOOLEAN" => Some("boolean")
      case _ => None
    }
    def termFamily(term: Term[ColumnRef]): Option[String] = term match {
      case Column(c) => metadata.get(c).flatMap(m => family(m.sqlTypeName))
      case _: Integral => Some("integral")
      case _: Text => Some("text")
      case _: Timestamp => Some("timestamp")
    }
    def valid(node: Tree[ColumnRef]): Boolean = node match {
      case Comparison(op, left, right) =>
        val f = termFamily(left)
        f.nonEmpty && f == termFamily(right) &&
          (op != Like || f.contains("text")) &&
          (left.isInstanceOf[Column[_]] || right.isInstanceOf[Column[_]])
      case NullTest(c, _) => termFamily(Column(c)).nonEmpty
      case BooleanColumn(c) => termFamily(Column(c)).contains("boolean")
      case Junction(_, terms) => terms.forall(valid)
    }
    bounded(tree) && tree.columns.nonEmpty &&
      tree.columns.map(_.relationOccurrenceId).distinct.size == 1 &&
      valid(tree)
  }
}

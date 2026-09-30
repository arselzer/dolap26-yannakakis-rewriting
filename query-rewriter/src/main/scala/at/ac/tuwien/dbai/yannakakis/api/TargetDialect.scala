package at.ac.tuwien.dbai.yannakakis.api

/** SQL dialect targeted by a rendered rewrite.
  *
  * The ADT deliberately contains only dialects with an implemented and tested
  * renderer. Adding a case is therefore an explicit API change rather than a
  * promise that arbitrary Calcite dialects are supported.
  */
sealed trait TargetDialect extends Product with Serializable {
  def id: String
}

object TargetDialect {
  case object PostgreSql extends TargetDialect {
    override val id: String = "postgresql"
  }

  val supported: Vector[TargetDialect] = Vector(PostgreSql)
}

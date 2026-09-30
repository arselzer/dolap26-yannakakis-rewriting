package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

/** Versioned enumeration-SQL emission contract.
  *
  * `LegacyV1` exists only so frozen campaign-v2 acquisition bytes remain
  * reproducible. New rendering uses `StatisticsSafeV2`: typed literal
  * predicates leave the source column visible to PostgreSQL statistics, and
  * reconstruction omits an original equality only after the compiler proves
  * that factor bindings plus every separator on the owner path imply it.
  */
private[yannakakis] sealed trait PostgreSqlEnumerationRenderingProfile
    extends Product
    with Serializable {
  def id: String
  private[postgresql] def castIntegralSourceColumn: Boolean
  private[postgresql] def retainAllReconstructionEqualities: Boolean
}

private[yannakakis] object PostgreSqlEnumerationRenderingProfile {
  case object LegacyV1 extends PostgreSqlEnumerationRenderingProfile {
    override val id: String = "legacy-v1"
    override private[postgresql] val castIntegralSourceColumn: Boolean = true
    override private[postgresql] val retainAllReconstructionEqualities: Boolean =
      true
  }

  case object StatisticsSafeV2 extends PostgreSqlEnumerationRenderingProfile {
    override val id: String = "statistics-safe-v2"
    override private[postgresql] val castIntegralSourceColumn: Boolean = false
    override private[postgresql] val retainAllReconstructionEqualities: Boolean =
      false
  }

  val default: PostgreSqlEnumerationRenderingProfile = StatisticsSafeV2
}

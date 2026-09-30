package at.ac.tuwien.dbai.yannakakis.algorithm.lowering

import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.BagFactorPlanningError.InvalidResourceLimit

sealed trait BagFactorResource extends Product with Serializable {
  def code: String
}

object BagFactorResource {
  case object Bags extends BagFactorResource {
    val code = "bags"
  }
  case object TreeEdges extends BagFactorResource {
    val code = "tree-edges"
  }
  case object SourceEdges extends BagFactorResource {
    val code = "source-edges"
  }
  case object OwnershipChecks extends BagFactorResource {
    val code = "ownership-checks"
  }
}

final class BagFactorPlanningLimits private (
    val maxBags: Long,
    val maxOwnershipChecks: Long,
    val maxTreeEdges: Long,
    val maxSourceEdges: Long
) extends Serializable {
  override def equals(other: Any): Boolean = other match {
    case that: BagFactorPlanningLimits =>
      maxBags == that.maxBags &&
        maxOwnershipChecks == that.maxOwnershipChecks &&
        maxTreeEdges == that.maxTreeEdges &&
        maxSourceEdges == that.maxSourceEdges
    case _ => false
  }

  override def hashCode(): Int =
    (maxBags, maxOwnershipChecks, maxTreeEdges, maxSourceEdges).hashCode()
  override def toString: String =
    s"BagFactorPlanningLimits($maxBags,$maxOwnershipChecks,$maxTreeEdges,$maxSourceEdges)"
}

object BagFactorPlanningLimits {
  val default: BagFactorPlanningLimits = new BagFactorPlanningLimits(
    maxBags = 250000L,
    maxOwnershipChecks = 10000000L,
    maxTreeEdges = 250000L,
    maxSourceEdges = 250000L
  )

  def create(
      maxBags: Long = default.maxBags,
      maxOwnershipChecks: Long = default.maxOwnershipChecks,
      maxTreeEdges: Long = default.maxTreeEdges,
      maxSourceEdges: Long = default.maxSourceEdges
  ): Either[Vector[InvalidResourceLimit], BagFactorPlanningLimits] = {
    val errors = Vector(
      BagFactorResource.Bags -> maxBags,
      BagFactorResource.OwnershipChecks -> maxOwnershipChecks,
      BagFactorResource.TreeEdges -> maxTreeEdges,
      BagFactorResource.SourceEdges -> maxSourceEdges
    ).collect {
      case (resource, value) if value <= 0L =>
        InvalidResourceLimit(resource, value)
    }

    if (errors.nonEmpty) Left(errors)
    else
      Right(
        new BagFactorPlanningLimits(
          maxBags,
          maxOwnershipChecks,
          maxTreeEdges,
          maxSourceEdges
        )
      )
  }
}

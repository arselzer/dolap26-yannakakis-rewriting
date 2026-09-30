package at.ac.tuwien.dbai.yannakakis.algorithm.candidate

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidateFailure.InvalidResourceLimit

sealed trait CandidateResource extends Product with Serializable {
  def code: String
}

object CandidateResource {
  case object Blocks extends CandidateResource {
    val code = "blocks"
  }
  case object BasisChecks extends CandidateResource {
    val code = "basis-checks"
  }
  case object BasisAlternatives extends CandidateResource {
    val code = "basis-alternatives"
  }
  case object WitnessNodes extends CandidateResource {
    val code = "witness-nodes"
  }
  case object PlanLimit extends CandidateResource {
    val code = "plan-limit"
  }
  case object RetainedPartialPlans extends CandidateResource {
    val code = "retained-partial-plans"
  }
  case object CombinationStates extends CandidateResource {
    val code = "combination-states"
  }
}

/** Hard limits shared by Boolean and exact top-K CandidateTD search. */
final class CandidateResourceLimits private (
    val maxBlocks: Long,
    val maxBasisChecks: Long,
    val maxBasisAlternatives: Long,
    val maxWitnessNodes: Long,
    val maxPlanLimit: Long,
    val maxRetainedPartialPlans: Long,
    val maxCombinationStates: Long
) extends Serializable {
  override def equals(other: Any): Boolean = other match {
    case that: CandidateResourceLimits =>
      maxBlocks == that.maxBlocks &&
        maxBasisChecks == that.maxBasisChecks &&
        maxBasisAlternatives == that.maxBasisAlternatives &&
        maxWitnessNodes == that.maxWitnessNodes &&
        maxPlanLimit == that.maxPlanLimit &&
        maxRetainedPartialPlans == that.maxRetainedPartialPlans &&
        maxCombinationStates == that.maxCombinationStates
    case _ => false
  }

  override def hashCode(): Int =
    (
      maxBlocks,
      maxBasisChecks,
      maxBasisAlternatives,
      maxWitnessNodes,
      maxPlanLimit,
      maxRetainedPartialPlans,
      maxCombinationStates
    ).hashCode()

  override def toString: String =
    s"CandidateResourceLimits($maxBlocks,$maxBasisChecks,$maxBasisAlternatives," +
      s"$maxWitnessNodes,$maxPlanLimit,$maxRetainedPartialPlans," +
      s"$maxCombinationStates)"
}

object CandidateResourceLimits {
  val default: CandidateResourceLimits = new CandidateResourceLimits(
    maxBlocks = 250000L,
    maxBasisChecks = 10000000L,
    maxBasisAlternatives = 1000000L,
    maxWitnessNodes = 250000L,
    maxPlanLimit = 100L,
    maxRetainedPartialPlans = 1000000L,
    maxCombinationStates = 5000000L
  )

  def create(
      maxBlocks: Long = default.maxBlocks,
      maxBasisChecks: Long = default.maxBasisChecks,
      maxBasisAlternatives: Long = default.maxBasisAlternatives,
      maxWitnessNodes: Long = default.maxWitnessNodes,
      maxPlanLimit: Long = default.maxPlanLimit,
      maxRetainedPartialPlans: Long = default.maxRetainedPartialPlans,
      maxCombinationStates: Long = default.maxCombinationStates
  ): Either[Vector[InvalidResourceLimit], CandidateResourceLimits] = {
    val supplied = Vector(
      CandidateResource.Blocks -> maxBlocks,
      CandidateResource.BasisChecks -> maxBasisChecks,
      CandidateResource.BasisAlternatives -> maxBasisAlternatives,
      CandidateResource.WitnessNodes -> maxWitnessNodes,
      CandidateResource.PlanLimit -> maxPlanLimit,
      CandidateResource.RetainedPartialPlans -> maxRetainedPartialPlans,
      CandidateResource.CombinationStates -> maxCombinationStates
    )
    val errors = supplied.collect {
      case (resource, value) if value <= 0L =>
        InvalidResourceLimit(resource, value)
    }
    if (errors.nonEmpty) Left(errors)
    else
      Right(
        new CandidateResourceLimits(
          maxBlocks,
          maxBasisChecks,
          maxBasisAlternatives,
          maxWitnessNodes,
          maxPlanLimit,
          maxRetainedPartialPlans,
          maxCombinationStates
        )
      )
  }
}

package at.ac.tuwien.dbai.yannakakis.algorithm.lowering

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidateCertificate
import at.ac.tuwien.dbai.yannakakis.core.{BagId, EdgeId}
import at.ac.tuwien.dbai.yannakakis.core.decomposition.CoveredDecomposition

/** One exact admissible support-cover choice for a structural bag. */
final case class BagFactorCoverAssignment(
    bagId: BagId,
    edgeIds: Vector[EdgeId]
) extends Serializable {
  require(bagId != null, "physical cover bag id must not be null")
  require(edgeIds != null, "physical cover edge ids must not be null")
  require(edgeIds.nonEmpty, "physical cover must not be empty")
  require(edgeIds.forall(_ != null), "physical cover must not contain null")
  require(
    edgeIds == edgeIds.sorted.distinct,
    "physical cover edge ids must be unique and increasing"
  )
}

/**
  * Physical choices layered over, but never substituted for, one certified
  * structural decomposition.
  */
final case class BagFactorPhysicalConfiguration(
    executionRoot: BagId,
    coverAssignments: Vector[BagFactorCoverAssignment]
) extends Serializable {
  require(executionRoot != null, "physical execution root must not be null")
  require(
    coverAssignments != null && coverAssignments.nonEmpty,
    "physical cover assignments must not be empty"
  )
  require(
    coverAssignments.forall(_ != null),
    "physical cover assignments must not contain null"
  )
  require(
    coverAssignments.map(_.bagId) ==
      coverAssignments.map(_.bagId).sorted.distinct,
    "physical cover assignments must have unique increasing bag ids"
  )
}

/**
  * A physically configured factor plan with its structural proof kept
  * separate. The wrapped [[BagFactorPlan]] intentionally retains the original
  * structural decomposition and certificate; only its derived orientation,
  * ownership, factors, and schedule reflect `configuration`.
  */
final case class ConfiguredBagFactorPlan private[lowering] (
    structuralDecomposition: CoveredDecomposition,
    structuralCertificate: CandidateCertificate,
    configuration: BagFactorPhysicalConfiguration,
    plan: BagFactorPlan
) extends Serializable {
  require(
    structuralDecomposition != null,
    "configured plan structural decomposition must not be null"
  )
  require(
    structuralCertificate != null,
    "configured plan structural certificate must not be null"
  )
  require(configuration != null, "physical configuration must not be null")
  require(plan != null, "configured bag-factor plan must not be null")
  require(
    plan.decomposition == structuralDecomposition,
    "configured plan must retain the structural decomposition"
  )
  require(
    plan.certificate == structuralCertificate,
    "configured plan must retain the structural certificate"
  )
  require(
    plan.root == configuration.executionRoot,
    "configured plan root must equal the physical execution root"
  )
  require(
    plan.factors.map(factor =>
      BagFactorCoverAssignment(factor.bagId, factor.supportCover)
    ) == configuration.coverAssignments,
    "configured plan factors must equal the physical cover assignment"
  )
}

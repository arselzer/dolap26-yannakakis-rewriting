package at.ac.tuwien.dbai.yannakakis.algorithm.lowering

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.{
  CandidateCertificateError,
  CandidateSearchInput
}
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.BagFactorPlanValidationError.{
  AdmissibilityModeMismatch,
  ArcsMismatch,
  CertificateMismatch,
  DecompositionMismatch,
  FactorsMismatch,
  OwnershipMismatch,
  RootMismatch,
  ScheduleMismatch,
  SourceHypergraphMismatch,
  SourceWidthMismatch
}
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.BagFactorPlanningError.{
  AmbiguousRootNearestOwner,
  CandidateCertificateViolations,
  EmptyBag,
  MissingEligibleOwner,
  MissingValue,
  ResourceLimitExceeded
}
import at.ac.tuwien.dbai.yannakakis.core.QueryHypergraph
import at.ac.tuwien.dbai.yannakakis.core.decomposition.{
  CoveredBag,
  CoveredDecomposition,
  DecompositionValidationError
}
import BagFactorPlanningOracleTestSupport._

class BagFactorPlannerOracleSuite extends munit.FunSuite {
  test("chain ownership, separators, and both DFS passes match the oracle") {
    val fixture = chainFixture()
    val plan = assertCanonicalPlan(fixture)

    assertEquals(
      plan.ownership.map(owner =>
        (owner.edgeId.value, owner.owner.value, owner.ownerDepth)
      ),
      Vector((0, 7, 0), (1, 3, 1), (2, 9, 2))
    )
    assertEquals(
      plan.arcs.map(arc =>
        (arc.parent.value, arc.child.value, arc.separator.map(_.value))
      ),
      Vector((7, 3, Vector(1)), (3, 9, Vector(2)))
    )
    assertEquals(
      plan.schedule.bottomUp.map(step =>
        (step.ordinal, step.target.value, step.source.value, step.separator.map(_.value))
      ),
      Vector((0, 3, 9, Vector(2)), (1, 7, 3, Vector(1)))
    )
    assertEquals(
      plan.schedule.topDown.map(step =>
        (step.ordinal, step.target.value, step.source.value, step.separator.map(_.value))
      ),
      Vector((0, 3, 7, Vector(1)), (1, 9, 3, Vector(2)))
    )
    assertOwnershipContract(fixture, plan)
  }

  test("triangle k=2 retains one support set and owns every occurrence once") {
    val fixture = triangleFixture()
    val plan = assertCanonicalPlan(fixture)

    assertEquals(plan.factors.size, 1)
    assertEquals(plan.factors.head.supportCover.map(_.value), Vector(0, 1))
    assertEquals(plan.factors.head.ownedEdgeIds.map(_.value), Vector(0, 1, 2))
    assertEquals(plan.ownership.map(_.edgeId.value), Vector(0, 1, 2))
    assert(plan.ownership.forall(_.owner == bagId(4)))
    assert(plan.ownership.forall(_.ownerDepth == 0))
    assertEquals(plan.arcs, Vector.empty)
    assertEquals(plan.schedule.bottomUp, Vector.empty)
    assertEquals(plan.schedule.topDown, Vector.empty)
    assertOwnershipContract(fixture, plan)
  }

  test("repeated-edge self joins retain occurrence identity in ownership") {
    val fixture = repeatedEdgeFixture()
    val plan = assertCanonicalPlan(fixture)

    assertEquals(fixture.hypergraph.edges.map(_.vertices).distinct.size, 1)
    assertEquals(
      fixture.hypergraph.relationOccurrenceIds.map(_.value),
      Vector(3, 7, 9)
    )
    assertEquals(plan.factors.head.supportCover.map(_.value), Vector(3))
    assertEquals(plan.factors.head.ownedEdgeIds.map(_.value), Vector(3, 7, 9))
    assertEquals(plan.ownership.map(_.edgeId.value), Vector(3, 7, 9))
    assertEquals(plan.ownership.map(_.owner.value), Vector(8, 8, 8))
    assertOwnershipContract(fixture, plan)
  }

  test("support covers cannot own extending edges and do not affect ownership") {
    val first = branchingFixture(redundantSupportEdges = true)
    val second = branchingFixture(redundantSupportEdges = false)
    val firstPlan = assertCanonicalPlan(first)
    val secondPlan = assertCanonicalPlan(second)

    assert(first.decomposition.bag(bagId(9)).get.cover.contains(edgeId(1)))
    assert(
      !first.decomposition.bag(bagId(9)).get.containsAll(
        first.hypergraph.edge(edgeId(1)).get.vertices
      )
    )
    assertEquals(
      firstPlan.ownership.map(owner =>
        (owner.edgeId.value, owner.owner.value, owner.ownerDepth)
      ),
      Vector((0, 9, 0), (1, 5, 1), (2, 2, 1))
    )
    assertEquals(firstPlan.ownership, secondPlan.ownership)
    assert(firstPlan.factors.map(_.supportCover) != secondPlan.factors.map(_.supportCover))

    assertEquals(firstPlan.factors.find(_.bagId == bagId(9)).get.children.map(_.value), Vector(2, 5))
    assertEquals(
      firstPlan.schedule.bottomUp.map(step =>
        (step.ordinal, step.target.value, step.source.value)
      ),
      Vector((0, 9, 2), (1, 9, 5))
    )
    assertEquals(
      firstPlan.schedule.topDown.map(step =>
        (step.ordinal, step.target.value, step.source.value)
      ),
      Vector((0, 2, 9), (1, 5, 9))
    )
    assert(firstPlan.arcs.forall(_.separator == Vector(variable(0))))
  }

  test("edge, bag, and raw tree-edge input permutations are deterministic") {
    val edgeDefinitions = Vector(
      2 -> Seq(0, 2),
      0 -> Seq(0),
      1 -> Seq(0, 1)
    )
    val canonical = branchingFixture(redundantSupportEdges = true)
    val expected = assertCanonicalPlan(canonical)
    val bags = canonical.decomposition.bags
    val treeEdges = canonical.decomposition.treeEdges

    edgeDefinitions.permutations.zipWithIndex.foreach {
      case (permutedEdges, index) =>
        val hypergraph = graphWithIds(permutedEdges)
        val input = candidateInput(hypergraph, 2, requireConnectedCover = true)
        val rotatedBags = bags.drop(index % bags.size) ++ bags.take(index % bags.size)
        val suppliedTreeEdges = if (index % 2 == 0) treeEdges else treeEdges.reverse
        val decomposition = coveredDecomposition(
          root = 9,
          rotatedBags,
          suppliedTreeEdges
        )
        val actual = assertCanonicalPlan(
          PlanningFixture(hypergraph, input, decomposition)
        )

        assertEquals(hypergraph, canonical.hypergraph)
        assertEquals(decomposition, canonical.decomposition)
        assertEquals(actual, expected)
    }
  }

  test("planner fails closed on missing, empty, invalid, and mismatched inputs") {
    val fixture = chainFixture()
    assert(
      BagFactorPlanner
        .plan(null, null)
        .swap
        .toOption
        .get
        .exists(_.isInstanceOf[MissingValue])
    )
    assert(
      BagFactorPlanner
        .plan(fixture.input, fixture.decomposition, null)
        .swap
        .toOption
        .get
        .exists(_.isInstanceOf[MissingValue])
    )

    val oneEdge = graph(Seq(Seq(0, 1)))
    val oneEdgeInput = candidateInput(oneEdge, 1, requireConnectedCover = true)
    val emptyRoot = coveredDecomposition(
      root = 0,
      bags = Vector(coveredBag(0, Vector.empty, Vector(0))),
      edges = Vector.empty
    )
    val emptyErrors = BagFactorPlanner
      .plan(oneEdgeInput, emptyRoot)
      .swap
      .toOption
      .get
    assert(emptyErrors.exists(_.isInstanceOf[EmptyBag]))
    assert(emptyErrors.exists(_.isInstanceOf[CandidateCertificateViolations]))

    val uncovered = coveredDecomposition(
      root = 7,
      bags = Vector(coveredBag(7, Vector(0, 1), Vector(0))),
      edges = Vector.empty
    )
    val uncoveredErrors = BagFactorPlanner
      .plan(fixture.input, uncovered)
      .swap
      .toOption
      .get
    assert(
      uncoveredErrors.exists(_.isInstanceOf[CandidateCertificateViolations])
    )

    val differentHypergraph = graphWithIds(
      Seq(
        0 -> Seq(0, 2),
        1 -> Seq(1, 2),
        2 -> Seq(0, 3)
      )
    )
    assertEquals(differentHypergraph.edgeIds, fixture.hypergraph.edgeIds)
    assertEquals(differentHypergraph.variables, fixture.hypergraph.variables)
    val differentInput = candidateInput(
      differentHypergraph,
      1,
      requireConnectedCover = true
    )
    val differentDecomposition = coveredDecomposition(
      root = 7,
      bags = Vector(
        coveredBag(7, Vector(0, 2), Vector(0)),
        coveredBag(3, Vector(1, 2), Vector(1)),
        coveredBag(9, Vector(0, 3), Vector(2))
      ),
      edges = Vector(treeEdge(7, 3), treeEdge(7, 9))
    )
    certificate(differentInput, differentDecomposition)
    val mismatchErrors = BagFactorPlanner
      .plan(fixture.input, differentDecomposition)
      .swap
      .toOption
      .get
    assert(
      mismatchErrors.exists(_.isInstanceOf[CandidateCertificateViolations])
    )
  }

  test("defensive validated boundary diagnoses missing and ambiguous owners") {
    val hypergraph = graph(Seq(Seq(0, 1)))
    val input = candidateInput(hypergraph, 1, requireConnectedCover = true)
    val valid = coveredDecomposition(
      root = 0,
      bags = Vector(coveredBag(0, Vector(0, 1), Vector(0))),
      edges = Vector.empty
    )
    val validCertificate = certificate(input, valid)

    val missing = coveredDecomposition(
      root = 9,
      bags = Vector(coveredBag(9, Vector(0), Vector(0))),
      edges = Vector.empty
    )
    assertEquals(
      BagFactorPlanner.buildValidated(
        input,
        missing,
        validCertificate
      ),
      Left(Vector(MissingEligibleOwner(edgeId(0))))
    )

    val ambiguous = coveredDecomposition(
      root = 9,
      bags = Vector(
        coveredBag(9, Vector(0), Vector(0)),
        coveredBag(1, Vector(0, 1), Vector(0)),
        coveredBag(2, Vector(0, 1), Vector(0))
      ),
      edges = Vector(treeEdge(9, 1), treeEdge(9, 2))
    )
    assertEquals(
      BagFactorPlanner.buildValidated(
        input,
        ambiguous,
        validCertificate
      ),
      Left(
        Vector(
          AmbiguousRootNearestOwner(
            edgeId(0),
            depth = 1,
            candidateBagIds = Vector(bagId(1), bagId(2))
          )
        )
      )
    )
  }

  test("independent validator rejects every corrupted plan field") {
    val fixture = branchingFixture(redundantSupportEdges = true)
    val plan = assertCanonicalPlan(fixture)
    assertEquals(
      BagFactorPlanValidator.validate(
        fixture.input,
        fixture.decomposition,
        plan
      ),
      Right(plan)
    )

    val otherHypergraph = graphWithIds(
      Seq(0 -> Seq(0), 1 -> Seq(0, 2), 2 -> Seq(0, 1))
    )
    val otherDecomposition = branchingFixture(
      redundantSupportEdges = false
    ).decomposition
    val corruptions = Vector[(BagFactorPlan, BagFactorPlanValidationError => Boolean)](
      plan.copy(sourceHypergraph = otherHypergraph) ->
        (_.isInstanceOf[SourceHypergraphMismatch]),
      plan.copy(sourceWidthBound = plan.sourceWidthBound + 1) ->
        (_.isInstanceOf[SourceWidthMismatch]),
      plan.copy(requireConnectedCover = !plan.requireConnectedCover) ->
        (_.isInstanceOf[AdmissibilityModeMismatch]),
      plan.copy(decomposition = otherDecomposition) ->
        (_.isInstanceOf[DecompositionMismatch]),
      plan.copy(certificate = plan.certificate.copy(bagCount = 99)) ->
        (_.isInstanceOf[CertificateMismatch]),
      plan.copy(root = bagId(2)) ->
        (_.isInstanceOf[RootMismatch]),
      plan.copy(
        factors = plan.factors.updated(
          0,
          plan.factors.head.copy(depth = plan.factors.head.depth + 1)
        )
      ) -> (_.isInstanceOf[FactorsMismatch]),
      plan.copy(ownership = plan.ownership.drop(1)) ->
        (_.isInstanceOf[OwnershipMismatch]),
      plan.copy(arcs = plan.arcs.drop(1)) ->
        (_.isInstanceOf[ArcsMismatch]),
      plan.copy(
        schedule = plan.schedule.copy(topDown = plan.schedule.topDown.reverse)
      ) -> (_.isInstanceOf[ScheduleMismatch])
    )

    corruptions.foreach { case (corrupted, expected) =>
      val errors = BagFactorPlanValidator
        .validate(fixture.input, fixture.decomposition, corrupted)
        .swap
        .toOption
        .getOrElse(Vector.empty)
      assertEquals(errors.size, 1)
      assert(expected(errors.head), errors.head.message)
    }
  }

  test("all planner resources accept the exact boundary and fail one below") {
    val fixture = chainFixture()
    val expected = assertCanonicalPlan(fixture)
    val exact = planningLimits(
      maxBags = 3,
      maxTreeEdges = 2,
      maxSourceEdges = 3,
      maxOwnershipChecks = 18
    )
    assertEquals(
      BagFactorPlanner.plan(fixture.input, fixture.decomposition, exact),
      Right(expected)
    )

    val boundaries = Vector(
      (
        planningLimits(maxBags = 2),
        ResourceLimitExceeded(BagFactorResource.Bags, required = 3, limit = 2)
      ),
      (
        planningLimits(maxTreeEdges = 1),
        ResourceLimitExceeded(
          BagFactorResource.TreeEdges,
          required = 2,
          limit = 1
        )
      ),
      (
        planningLimits(maxSourceEdges = 2),
        ResourceLimitExceeded(
          BagFactorResource.SourceEdges,
          required = 3,
          limit = 2
        )
      ),
      (
        planningLimits(maxOwnershipChecks = 17),
        ResourceLimitExceeded(
          BagFactorResource.OwnershipChecks,
          required = 18,
          limit = 17
        )
      )
    )
    boundaries.foreach { case (limits, expectedFailure) =>
      val errors = BagFactorPlanner
        .plan(fixture.input, fixture.decomposition, limits)
        .swap
        .toOption
        .getOrElse(Vector.empty)
      assertEquals(errors, Vector(expectedFailure))

      val validationErrors = BagFactorPlanValidator
        .validate(fixture.input, fixture.decomposition, expected, limits)
        .swap
        .toOption
        .getOrElse(Vector.empty)
      assertEquals(
        validationErrors,
        Vector(
          BagFactorPlanValidationError.ResourceLimitExceeded(
            expectedFailure.resource,
            expectedFailure.required,
            expectedFailure.limit
          )
        )
      )
    }

    val invalid = BagFactorPlanningLimits
      .create(0, 0, 0, 0)
      .swap
      .toOption
      .get
    assertEquals(invalid.size, 4)
  }

  test("raw duplicate tree edges count against the tree-edge boundary") {
    val fixture = chainFixture()
    val duplicateEdge = fixture.decomposition.treeEdges.head
    val malformed = coveredDecomposition(
      fixture.decomposition.root.value,
      fixture.decomposition.bags,
      fixture.decomposition.treeEdges :+ duplicateEdge
    )
    val errors = BagFactorPlanner
      .plan(
        fixture.input,
        malformed,
        planningLimits(maxTreeEdges = 2)
      )
      .swap
      .toOption
      .get

    assert(
      errors.exists {
        case ResourceLimitExceeded(BagFactorResource.TreeEdges, required, 2) =>
          required == BigInt(3)
        case _ => false
      }
    )
    assertEquals(errors.size, 1)
  }

  private def chainFixture(): PlanningFixture = {
    val hypergraph = graph(
      Seq(Seq(0, 1), Seq(1, 2), Seq(2, 3))
    )
    val input = candidateInput(hypergraph, 1, requireConnectedCover = true)
    val decomposition = coveredDecomposition(
      root = 7,
      bags = Vector(
        coveredBag(7, Vector(0, 1), Vector(0)),
        coveredBag(3, Vector(1, 2), Vector(1)),
        coveredBag(9, Vector(2, 3), Vector(2))
      ),
      edges = Vector(treeEdge(3, 9), treeEdge(7, 3))
    )
    certificate(input, decomposition)
    PlanningFixture(hypergraph, input, decomposition)
  }

  private def triangleFixture(): PlanningFixture = {
    val hypergraph = graph(
      Seq(Seq(0, 1), Seq(1, 2), Seq(2, 0))
    )
    val input = candidateInput(hypergraph, 2, requireConnectedCover = true)
    val decomposition = coveredDecomposition(
      root = 4,
      bags = Vector(coveredBag(4, Vector(0, 1, 2), Vector(0, 1))),
      edges = Vector.empty
    )
    certificate(input, decomposition)
    PlanningFixture(hypergraph, input, decomposition)
  }

  private def repeatedEdgeFixture(): PlanningFixture = {
    val hypergraph = graphWithIds(
      Seq(9 -> Seq(0, 1), 3 -> Seq(0, 1), 7 -> Seq(0, 1))
    )
    val input = candidateInput(hypergraph, 1, requireConnectedCover = true)
    val decomposition = coveredDecomposition(
      root = 8,
      bags = Vector(coveredBag(8, Vector(0, 1), Vector(3))),
      edges = Vector.empty
    )
    certificate(input, decomposition)
    PlanningFixture(hypergraph, input, decomposition)
  }

  private def branchingFixture(
      redundantSupportEdges: Boolean
  ): PlanningFixture = {
    val hypergraph = graphWithIds(
      Seq(0 -> Seq(0), 1 -> Seq(0, 1), 2 -> Seq(0, 2))
    )
    val input = candidateInput(hypergraph, 2, requireConnectedCover = true)
    val bags =
      if (redundantSupportEdges) {
        Vector(
          coveredBag(9, Vector(0), Vector(1)),
          coveredBag(2, Vector(0, 2), Vector(0, 2)),
          coveredBag(5, Vector(0, 1), Vector(0, 1))
        )
      } else {
        Vector(
          coveredBag(9, Vector(0), Vector(2)),
          coveredBag(2, Vector(0, 2), Vector(2)),
          coveredBag(5, Vector(0, 1), Vector(1))
        )
      }
    val decomposition = coveredDecomposition(
      root = 9,
      bags,
      edges = Vector(treeEdge(9, 5), treeEdge(2, 9))
    )
    certificate(input, decomposition)
    PlanningFixture(hypergraph, input, decomposition)
  }

  private def assertCanonicalPlan(fixture: PlanningFixture): BagFactorPlan = {
    val plan = planned(fixture.input, fixture.decomposition)
    assertEquals(
      normalize(plan),
      derive(fixture.hypergraph, fixture.decomposition)
    )
    assertEquals(plan.sourceHypergraph, fixture.hypergraph)
    assertEquals(plan.sourceWidthBound, fixture.input.widthBound)
    assertEquals(
      plan.requireConnectedCover,
      fixture.input.requireConnectedCover
    )
    assertEquals(plan.decomposition, fixture.decomposition)
    assertEquals(plan.certificate, certificate(fixture.input, fixture.decomposition))
    assertEquals(
      BagFactorPlanValidator.validate(
        fixture.input,
        fixture.decomposition,
        plan
      ),
      Right(plan)
    )
    plan
  }

  private def assertOwnershipContract(
      fixture: PlanningFixture,
      plan: BagFactorPlan
  ): Unit = {
    assertEquals(plan.ownership.map(_.edgeId), fixture.hypergraph.edgeIds)
    assertEquals(plan.ownership.map(_.edgeId).distinct, fixture.hypergraph.edgeIds)
    assertEquals(
      plan.factors.flatMap(_.ownedEdgeIds).sorted,
      fixture.hypergraph.edgeIds
    )
    assertEquals(
      plan.factors.flatMap(_.ownedEdgeIds).distinct.sorted,
      fixture.hypergraph.edgeIds
    )
    plan.ownership.foreach { ownership =>
      val edge = fixture.hypergraph.edge(ownership.edgeId).get
      val owner = fixture.decomposition.bag(ownership.owner).get
      assert(owner.containsAll(edge.vertices))
      val containingDepths = plan.factors
        .filter(factor =>
          fixture.decomposition.bag(factor.bagId).get.containsAll(edge.vertices)
        )
        .map(_.depth)
      assertEquals(ownership.ownerDepth, containingDepths.min)
      assertEquals(
        plan.factors.count(factor => factor.ownedEdgeIds.contains(edge.id)),
        1
      )
    }
  }

  private def planningLimits(
      maxBags: Long = BagFactorPlanningLimits.default.maxBags,
      maxOwnershipChecks: Long = BagFactorPlanningLimits.default.maxOwnershipChecks,
      maxTreeEdges: Long = BagFactorPlanningLimits.default.maxTreeEdges,
      maxSourceEdges: Long = BagFactorPlanningLimits.default.maxSourceEdges
  ): BagFactorPlanningLimits =
    BagFactorPlanningLimits
      .create(
        maxBags,
        maxOwnershipChecks,
        maxTreeEdges,
        maxSourceEdges
      )
      .fold(
        errors => throw new AssertionError(errors.map(_.message).mkString("; ")),
        identity
      )
}

package at.ac.tuwien.dbai.yannakakis.algorithm.lowering

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidateSearchInput
import at.ac.tuwien.dbai.yannakakis.core.decomposition.CoveredDecomposition

class ConfiguredBagFactorPhysicalContractSuite extends munit.FunSuite {
  import BagFactorPlanningOracleTestSupport._

  private final case class ConfiguredFixture(
      input: CandidateSearchInput,
      structural: CoveredDecomposition,
      physical: CoveredDecomposition,
      configuration: BagFactorPhysicalConfiguration,
      configured: ConfiguredBagFactorPlan
  )

  private lazy val fixture: ConfiguredFixture = {
    val hypergraph = graphWithIds(
      Seq(
        0 -> Seq(0, 1),
        1 -> Seq(1, 2),
        2 -> Seq(0, 2),
        3 -> Seq(2, 3),
        4 -> Seq(3, 4),
        5 -> Seq(2, 4),
        6 -> Seq(2)
      )
    )
    val input = candidateInput(hypergraph, widthBound = 2, requireConnectedCover = true)
    val structural = coveredDecomposition(
      root = 10,
      bags = Vector(
        coveredBag(10, Seq(0, 1, 2), Seq(0, 1)),
        coveredBag(20, Seq(2, 3, 4), Seq(3, 4))
      ),
      edges = Vector(treeEdge(10, 20))
    )
    val physical = coveredDecomposition(
      root = 20,
      bags = Vector(
        coveredBag(10, Seq(0, 1, 2), Seq(0, 2)),
        coveredBag(20, Seq(2, 3, 4), Seq(3, 5))
      ),
      edges = Vector(treeEdge(10, 20))
    )
    val configuration = BagFactorPhysicalConfiguration(
      executionRoot = bagId(20),
      coverAssignments = Vector(
        BagFactorCoverAssignment(bagId(10), Vector(edgeId(0), edgeId(2))),
        BagFactorCoverAssignment(bagId(20), Vector(edgeId(3), edgeId(5)))
      )
    )
    val configured = ConfiguredBagFactorPlanner
      .plan(input, structural, configuration)
      .fold(errors => fail(errors.map(_.message).mkString("; ")), identity)
    ConfiguredFixture(input, structural, physical, configuration, configured)
  }

  test("configured planning preserves the structural proof while independently changing physical orientation and covers") {
    val value = fixture

    assertEquals(value.configured.structuralDecomposition, value.structural)
    assertEquals(value.configured.plan.decomposition, value.structural)
    assertEquals(value.configured.structuralCertificate, certificate(value.input, value.structural))
    assertEquals(value.configured.plan.certificate, value.configured.structuralCertificate)
    assertEquals(value.configured.configuration, value.configuration)
    assertEquals(normalize(value.configured.plan), derive(value.input.hypergraph, value.physical))

    val structuralPlan = planned(value.input, value.structural)
    val sharedEdge = edgeId(6)
    assertEquals(structuralPlan.ownership.find(_.edgeId == sharedEdge).map(_.owner), Some(bagId(10)))
    assertEquals(value.configured.plan.ownership.find(_.edgeId == sharedEdge).map(_.owner), Some(bagId(20)))
    assertEquals(
      value.configured.plan.ownership.map(_.edgeId).sorted,
      value.input.hypergraph.edges.map(_.id).sorted
    )
  }

  test("configured planner rejects invalid roots, incomplete assignments, and non-admissible covers before construction") {
    val value = fixture
    val assignments = value.configuration.coverAssignments
    val cases = Vector(
      BagFactorPhysicalConfiguration(bagId(999), assignments) ->
        "physical-lowering.root.invalid",
      BagFactorPhysicalConfiguration(bagId(20), assignments.take(1)) ->
        "physical-lowering.cover.bag-ids",
      BagFactorPhysicalConfiguration(
        bagId(20),
        assignments.updated(
          0,
          BagFactorCoverAssignment(bagId(10), Vector(edgeId(6)))
        )
      ) -> "physical-lowering.cover.not-admissible"
    )

    cases.foreach { case (configuration, expectedCode) =>
      val errors = ConfiguredBagFactorPlanner
        .plan(value.input, value.structural, configuration)
        .fold(identity, _ => fail(s"configuration unexpectedly succeeded: $configuration"))
      assert(errors.exists(_.code == expectedCode), clues(errors.map(_.code)))
    }
  }

  test("physical configuration constructors enforce exact nonempty canonical assignments") {
    intercept[IllegalArgumentException] {
      BagFactorCoverAssignment(bagId(10), Vector.empty)
    }
    intercept[IllegalArgumentException] {
      BagFactorCoverAssignment(bagId(10), Vector(edgeId(2), edgeId(0)))
    }
    intercept[IllegalArgumentException] {
      BagFactorCoverAssignment(bagId(10), Vector(edgeId(0), edgeId(0)))
    }
    intercept[IllegalArgumentException] {
      BagFactorPhysicalConfiguration(bagId(10), Vector.empty)
    }
    intercept[IllegalArgumentException] {
      BagFactorPhysicalConfiguration(
        bagId(10),
        Vector(
          BagFactorCoverAssignment(bagId(20), Vector(edgeId(3))),
          BagFactorCoverAssignment(bagId(10), Vector(edgeId(0)))
        )
      )
    }
  }

  test("independent validator detects ownership, factor, arc, and schedule corruption without invoking the planner") {
    val value = fixture
    val good = value.configured
    val plan = good.plan
    val corruptions = Vector(
      good.copy(plan = plan.copy(ownership = plan.ownership.reverse)) ->
        "physical-lowering.validation.ownership",
      good.copy(
        plan = plan.copy(
          factors = plan.factors.updated(
            0,
            plan.factors.head.copy(ownedEdgeIds = Vector.empty)
          )
        )
      ) -> "physical-lowering.validation.factors",
      good.copy(
        plan = plan.copy(
          arcs = plan.arcs.map(arc => arc.copy(parent = arc.child, child = arc.parent))
        )
      ) -> "physical-lowering.validation.arcs",
      good.copy(
        plan = plan.copy(schedule = plan.schedule.copy(bottomUp = Vector.empty))
      ) -> "physical-lowering.validation.schedule"
    )

    corruptions.foreach { case (configured, expectedCode) =>
      val errors = ConfiguredBagFactorPlanValidator
        .validate(value.input, value.structural, configured)
        .fold(identity, _ => fail(s"corruption unexpectedly validated: $expectedCode"))
      assert(errors.exists(_.code == expectedCode), clues(errors.map(_.code)))
    }
  }

  test("independent validator binds source provenance and the certified structural input") {
    val value = fixture
    val good = value.configured
    val wrongWidth = good.copy(
      plan = good.plan.copy(sourceWidthBound = good.plan.sourceWidthBound + 1)
    )
    val wrongMode = good.copy(
      plan = good.plan.copy(requireConnectedCover = !good.plan.requireConnectedCover)
    )

    val widthErrors = ConfiguredBagFactorPlanValidator
      .validate(value.input, value.structural, wrongWidth)
      .fold(identity, _ => fail("wrong source width unexpectedly validated"))
    val modeErrors = ConfiguredBagFactorPlanValidator
      .validate(value.input, value.structural, wrongMode)
      .fold(identity, _ => fail("wrong admissibility mode unexpectedly validated"))
    assertEquals(widthErrors.map(_.code), Vector("physical-lowering.validation.provenance.width"))
    assertEquals(modeErrors.map(_.code), Vector("physical-lowering.validation.provenance.admissibility"))
  }

  test("planner and validator fail closed on missing values and the same resource envelope") {
    val value = fixture
    val oneBag = BagFactorPlanningLimits
      .create(maxBags = 1)
      .fold(errors => fail(errors.map(_.message).mkString("; ")), identity)

    val planningMissing = ConfiguredBagFactorPlanner
      .plan(null, null, null, null)
      .fold(identity, _ => fail("missing planner inputs unexpectedly succeeded"))
    assertEquals(
      planningMissing.map(_.code),
      Vector.fill(4)("physical-lowering.input.missing")
    )

    val validationMissing = ConfiguredBagFactorPlanValidator
      .validate(null, null, null, null)
      .fold(identity, _ => fail("missing validator inputs unexpectedly succeeded"))
    assertEquals(
      validationMissing.map(_.code),
      Vector.fill(4)("physical-lowering.validation.missing")
    )

    val planningResource = ConfiguredBagFactorPlanner
      .plan(value.input, value.structural, value.configuration, oneBag)
      .fold(identity, _ => fail("planner ignored bag limit"))
    val validationResource = ConfiguredBagFactorPlanValidator
      .validate(value.input, value.structural, value.configured, oneBag)
      .fold(identity, _ => fail("validator ignored bag limit"))
    assertEquals(
      planningResource.map(_.code),
      Vector("physical-lowering.resource.exceeded.bags")
    )
    assertEquals(
      validationResource.map(_.code),
      Vector("physical-lowering.validation.resource.exceeded.bags")
    )
  }
}

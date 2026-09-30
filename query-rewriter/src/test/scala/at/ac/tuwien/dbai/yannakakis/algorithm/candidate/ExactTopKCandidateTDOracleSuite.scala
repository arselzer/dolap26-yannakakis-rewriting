package at.ac.tuwien.dbai.yannakakis.algorithm.candidate

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidateDecision.{
  Accepted => BooleanAccepted,
  Rejected => BooleanRejected
}
import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidateFailure.{
  InvalidPlanLimit,
  ResourceLimitExceeded
}
import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidatePlanDecision.{
  Found,
  Rejected => TopKRejected
}
import at.ac.tuwien.dbai.yannakakis.algorithm.soft.{
  AdmissibleSoftBagGeneration,
  SoftBagGeneration,
  SoftBagGenerator
}
import at.ac.tuwien.dbai.yannakakis.core.{QueryHypergraph, VariableId}
import at.ac.tuwien.dbai.yannakakis.core.decomposition.DecompositionValidator
import CandidateTDOracleTestSupport._
import ExactTopKCandidateTDOracleTestSupport._

class ExactTopKCandidateTDOracleSuite extends munit.FunSuite {
  test("K=1 is exactly the Boolean decision and witness") {
    val cases = Vector(
      prepared(graph(Seq(Seq(0, 1))), 1, connected = true),
      prepared(
        graph(Seq(Seq(0, 1), Seq(1, 2), Seq(2, 3))),
        1,
        connected = true
      ),
      prepared(
        graph(Seq(Seq(0, 1), Seq(1, 2), Seq(2, 0))),
        2,
        connected = true
      )
    )

    cases.foreach { fixture =>
      val oracle = enumerate(oracleInput(fixture.hypergraph, fixture.generation))
      val topK = assertMatchesOracle(fixture.input, oracle, planLimit = 1)
      BooleanCandidateTD.decide(fixture.input) match {
        case Right(boolean: BooleanAccepted) =>
          val found = topK.getOrElse(
            throw new AssertionError("Boolean accepted but top-K rejected")
          )
          assertEquals(found.plans.size, 1)
          assertEquals(found.plans.head.witness, boolean.witness)
          assertEquals(found.plans.head.decomposition, boolean.decomposition)
          assertEquals(found.plans.head.certificate, boolean.certificate)
        case Right(_: BooleanRejected) => assertEquals(topK, None)
        case Left(failure) => throw new AssertionError(failure.message)
      }
    }
  }

  test("full path frontier covers fewer, exactly, and more available plans than K") {
    val fixture = prepared(
      graph(Seq(Seq(0, 1), Seq(1, 2), Seq(2, 3))),
      1,
      connected = true
    )
    val oracle = enumerate(oracleInput(fixture.hypergraph, fixture.generation))

    assertEquals(oracle.ranked.size, 16)

    // More than K plans are available.
    assertEquals(
      assertMatchesOracle(fixture.input, oracle, planLimit = 4).get.plans.size,
      4
    )
    // Exactly K plans are available.
    assertEquals(
      assertMatchesOracle(
        fixture.input,
        oracle,
        planLimit = oracle.ranked.size
      ).get.plans.size,
      oracle.ranked.size
    )
    // Fewer than K plans are available; this remains Found, not Rejected.
    val oversizedLimit = oracle.ranked.size + 3
    val fewer = assertMatchesOracle(
      fixture.input,
      oracle,
      planLimit = oversizedLimit
    ).get
    assertEquals(fewer.requestedLimit, oversizedLimit)
    assertEquals(fewer.plans.size, oracle.ranked.size)
  }

  test("multi-child Cartesian alternatives merge competing roots globally") {
    val fixture = prepared(
      graph(Seq(Seq(0, 1), Seq(1, 2), Seq(2, 3))),
      1,
      connected = true
    )
    val oracle = enumerate(oracleInput(fixture.hypergraph, fixture.generation))
    val statistics = oracle.statistics

    assert(statistics.maxNonEmptyChildArity >= 2)
    assert(statistics.maxMultiChoiceChildArity >= 2)
    assert(statistics.maxCartesianCardinality >= 4L)
    assert(statistics.rootBasisAlternativeCount >= 3)

    val roots = oracle.ranked.map(_.tree.bag)
    val interleaves = roots.indices.exists { first =>
      ((first + 2) until roots.size).exists { last =>
        roots(first) == roots(last) &&
        ((first + 1) until last).exists(middle => roots(middle) != roots(first))
      }
    }
    assert(interleaves, s"expected root-basis interleaving, got $roots")

    assertMatchesOracle(
      fixture.input,
      oracle,
      planLimit = oracle.ranked.size
    )
  }

  test("Cartesian helper exhausts coordinates and deduplicates symmetric children") {
    val rootBag = CandidateBagKey.canonical(Vector(variable(9)))
    val firstBag = CandidateBagKey.canonical(Vector(variable(0)))
    val secondBag = CandidateBagKey.canonical(Vector(variable(1)))
    val basis = new CandidateBasis(rootBag, Vector.empty)
    val first = new CandidateTreeNode(firstBag, Vector.empty)
    val second = new CandidateTreeNode(secondBag, Vector.empty)
    val frontiers = Vector(
      Vector(first, second),
      Vector(first, second)
    )
    val budget = new CandidateTopKBudget(CandidateResourceLimits.default)

    val actual = TopKCombinationEnumerator
      .enumerate(basis, frontiers, planLimit = 10, budget)
      .fold(failure => throw new AssertionError(failure.message), identity)

    val oracleFirst = canonicalTree(Vector(variable(0)), Vector.empty)
    val oracleSecond = canonicalTree(Vector(variable(1)), Vector.empty)
    val expected = rankAndDeduplicate(
      cartesianProduct(
        Vector(
          Vector(oracleFirst, oracleSecond),
          Vector(oracleFirst, oracleSecond)
        )
      ).map(children => canonicalTree(Vector(variable(9)), children))
    )

    assertEquals(actual.map(toOracleTree), expected)
    assertEquals(actual.size, 3)
    assertEquals(budget.combinationStateCount, 4L)
  }

  test("triangle rejection and complete accepted ordering match the oracle") {
    val triangle = graph(Seq(Seq(0, 1), Seq(1, 2), Seq(2, 0)))
    val widthOne = prepared(triangle, 1, connected = false)
    val rejectedOracle = enumerate(
      oracleInput(widthOne.hypergraph, widthOne.generation)
    )

    assertEquals(rejectedOracle.ranked, Vector.empty)
    assertEquals(
      assertMatchesOracle(widthOne.input, rejectedOracle, planLimit = 3),
      None
    )

    val widthTwo = prepared(triangle, 2, connected = true)
    val acceptedOracle = enumerate(
      oracleInput(widthTwo.hypergraph, widthTwo.generation)
    )
    assertEquals(acceptedOracle.ranked.size, 4)
    assertEquals(acceptedOracle.ranked.head.tree.nodeCount, BigInt(1))
    assertMatchesOracle(widthTwo.input, acceptedOracle, planLimit = 10)
  }

  test("triangle full top-K matches the distinct-bag rooted-tree sub-universe") {
    val triangle = graph(Seq(Seq(0, 1), Seq(1, 2), Seq(2, 0)))
    val fixture = prepared(triangle, 2, connected = true)
    val oracleInputValue = oracleInput(triangle, fixture.generation)
    val recurrence = enumerate(oracleInputValue)
    val distinctBagRootedTrees = bruteForceDistinctBagAll(oracleInputValue)

    assertEquals(oracleInputValue.candidates.size, 5)
    assertEquals(distinctBagRootedTrees.size, 4)
    assertEquals(
      recurrence.ranked.map(_.decomposition),
      distinctBagRootedTrees
    )

    val production = assertMatchesOracle(
      fixture.input,
      recurrence,
      planLimit = distinctBagRootedTrees.size
    ).get
    assertEquals(
      production.plans.map(_.decomposition),
      distinctBagRootedTrees
    )
  }

  test("star top-K includes valid non-adjacent repeated-bag trees") {
    val cases = Vector(
      (graph(Seq(Seq(0, 1), Seq(0, 2), Seq(0, 3))), 13, 10),
      (
        graph(Seq(Seq(0, 1), Seq(0, 2), Seq(0, 3), Seq(0, 4))),
        33,
        17
      )
    )

    cases.foreach { case (star, expectedFullCount, expectedDistinctBagCount) =>
      val fixture = prepared(star, 1, connected = true)
      val oracleInputValue = oracleInput(star, fixture.generation)
      val recurrence = enumerate(oracleInputValue)
      val distinctBagTrees = bruteForceDistinctBagAll(oracleInputValue)
      val recurrenceDecompositions = recurrence.ranked.map(_.decomposition)

      def bags(tree: OracleTree): Vector[Vector[VariableId]] =
        tree.bag +: tree.children.flatMap(bags)

      def hasDistinctBags(tree: OracleTree): Boolean = {
        val structuralBags = bags(tree)
        structuralBags.distinct.size == structuralBags.size
      }

      def hasNoEqualAdjacentBags(tree: OracleTree): Boolean =
        tree.children.forall { child =>
          child.bag != tree.bag && hasNoEqualAdjacentBags(child)
        }

      val distinctBagRecurrence = recurrence.ranked.filter(ranked =>
        hasDistinctBags(ranked.tree)
      )
      val repeatedBagRecurrence = recurrence.ranked.filterNot(ranked =>
        hasDistinctBags(ranked.tree)
      )

      assertEquals(recurrence.ranked.size, expectedFullCount)
      assertEquals(distinctBagTrees.size, expectedDistinctBagCount)
      assertEquals(
        distinctBagRecurrence.map(_.decomposition),
        distinctBagTrees
      )
      assertEquals(
        repeatedBagRecurrence.size,
        expectedFullCount - expectedDistinctBagCount
      )
      assert(
        repeatedBagRecurrence.forall(ranked => !hasDistinctBags(ranked.tree))
      )
      assert(
        recurrence.ranked.forall(ranked => hasNoEqualAdjacentBags(ranked.tree))
      )

      val production = assertMatchesOracle(
        fixture.input,
        recurrence,
        planLimit = expectedFullCount
      ).get
      assertEquals(
        production.plans.map(_.decomposition),
        recurrenceDecompositions
      )
    }
  }

  test("C5 ConCov rejection at k=2 and top-K acceptance at k=3 match the oracle") {
    val cycle = graph(
      Seq(
        Seq(0, 1),
        Seq(1, 2),
        Seq(2, 3),
        Seq(3, 4),
        Seq(4, 0)
      )
    )
    val bounds = OracleBounds(
      maxCandidateBags = 40,
      maxBlocks = 256,
      maxTreesPerBlock = 20000,
      maxCartesianCombinations = 1000000L
    )

    val widthTwo = prepared(cycle, 2, connected = true)
    val rejectedOracle = enumerate(
      oracleInput(widthTwo.hypergraph, widthTwo.generation),
      bounds
    )
    assertEquals(rejectedOracle.ranked, Vector.empty)
    assertEquals(
      assertMatchesOracle(widthTwo.input, rejectedOracle, planLimit = 5),
      None
    )

    val widthThree = prepared(cycle, 3, connected = true)
    val acceptedOracle = enumerate(
      oracleInput(widthThree.hypergraph, widthThree.generation),
      bounds
    )
    assert(acceptedOracle.ranked.size > 5)
    assertMatchesOracle(widthThree.input, acceptedOracle, planLimit = 5)
  }

  test("self-join witnesses deduplicate structurally but preserve first EdgeId cover") {
    val repeated = graphWithIds(
      Seq(
        9 -> Seq(0, 1),
        3 -> Seq(0, 1),
        7 -> Seq(0, 1),
        5 -> Seq(0, 1)
      )
    )
    val fixture = prepared(repeated, 2, connected = true)
    val rawFull = fixture.raw.candidates
      .find(_.vertices.map(_.value) == Vector(0, 1))
      .getOrElse(throw new AssertionError("missing full self-join candidate"))
    val oracle = enumerate(oracleInput(repeated, fixture.generation))

    assert(rawFull.witnesses.size > 1)
    assertEquals(
      rawFull.covers.map(_.cover.edgeIds.map(_.value)).take(4),
      Vector(Vector(3), Vector(5), Vector(7), Vector(9))
    )
    assertEquals(oracle.ranked.size, 1)
    val found = assertMatchesOracle(fixture.input, oracle, planLimit = 8).get
    assertEquals(found.plans.size, 1)
    assertEquals(
      found.plans.head.decomposition.bags.head.cover.map(_.value),
      Vector(3)
    )
  }

  test("edge input permutations preserve the entire ranked vector") {
    val edges = Vector(
      30 -> Seq(2, 3),
      10 -> Seq(0, 1),
      20 -> Seq(1, 2)
    )
    val expectedFixture = prepared(graphWithIds(edges), 1, connected = true)
    val expectedOracle = enumerate(
      oracleInput(expectedFixture.hypergraph, expectedFixture.generation)
    )
    val expected = assertMatchesOracle(
      expectedFixture.input,
      expectedOracle,
      planLimit = 10
    ).get

    edges.permutations.foreach { permutation =>
      val fixture = prepared(graphWithIds(permutation), 1, connected = true)
      val oracle = enumerate(oracleInput(fixture.hypergraph, fixture.generation))
      val actual = assertMatchesOracle(
        fixture.input,
        oracle,
        planLimit = 10
      ).get
      assertEquals(actual.plans, expected.plans)
      assertEquals(actual.statistics, expected.statistics)
    }
  }

  test("invalid K and all top-K resource boundaries fail closed") {
    val fixture = prepared(
      graph(Seq(Seq(0, 1), Seq(1, 2), Seq(2, 3))),
      1,
      connected = true
    )

    assertEquals(
      TopKCandidateTD.enumerate(fixture.input, 0),
      Left(InvalidPlanLimit(0))
    )
    assertEquals(
      TopKCandidateTD.enumerate(fixture.input, -7),
      Left(InvalidPlanLimit(-7))
    )

    val planLimit = 5
    val baseline = topKFound(fixture.input, planLimit)

    val exactPlanLimit = candidateLimits(maxPlanLimit = planLimit)
    assertEquals(
      TopKCandidateTD.enumerate(fixture.input, planLimit, exactPlanLimit),
      Right(baseline)
    )
    assertEquals(
      TopKCandidateTD.enumerate(
        fixture.input,
        planLimit,
        candidateLimits(maxPlanLimit = planLimit - 1L)
      ),
      Left(
        ResourceLimitExceeded(
          CandidateResource.PlanLimit,
          required = planLimit,
          limit = planLimit - 1L
        )
      )
    )

    val retained = baseline.statistics.retainedPartialPlanCount
    assert(retained > 1L)
    assertEquals(
      TopKCandidateTD.enumerate(
        fixture.input,
        planLimit,
        candidateLimits(maxRetainedPartialPlans = retained)
      ),
      Right(baseline)
    )
    assertEquals(
      TopKCandidateTD.enumerate(
        fixture.input,
        planLimit,
        candidateLimits(maxRetainedPartialPlans = retained - 1L)
      ),
      Left(
        ResourceLimitExceeded(
          CandidateResource.RetainedPartialPlans,
          required = retained,
          limit = retained - 1L
        )
      )
    )

    val combinations = baseline.statistics.combinationStateCount
    assert(combinations > 1L)
    assertEquals(
      TopKCandidateTD.enumerate(
        fixture.input,
        planLimit,
        candidateLimits(maxCombinationStates = combinations)
      ),
      Right(baseline)
    )
    assertEquals(
      TopKCandidateTD.enumerate(
        fixture.input,
        planLimit,
        candidateLimits(maxCombinationStates = combinations - 1L)
      ),
      Left(
        ResourceLimitExceeded(
          CandidateResource.CombinationStates,
          required = combinations,
          limit = combinations - 1L
        )
      )
    )

    // K spans the three minimum three-node plans and then larger plans, so
    // this verifies the maximum over every selected output, not Boolean K=1.
    val largestSelectedWitness = baseline.plans.map(_.witness.nodeCount).max
    assertEquals(largestSelectedWitness, BigInt(4))
    assertEquals(
      TopKCandidateTD.enumerate(
        fixture.input,
        planLimit,
        candidateLimits(maxWitnessNodes = largestSelectedWitness.longValue)
      ),
      Right(baseline)
    )
    assertEquals(
      TopKCandidateTD.enumerate(
        fixture.input,
        planLimit,
        candidateLimits(maxWitnessNodes = largestSelectedWitness.longValue - 1L)
      ),
      Left(
        ResourceLimitExceeded(
          CandidateResource.WitnessNodes,
          required = largestSelectedWitness,
          limit = largestSelectedWitness.longValue - 1L
        )
      )
    )
  }

  private final class Prepared(
      val hypergraph: QueryHypergraph,
      val raw: SoftBagGeneration,
      val generation: AdmissibleSoftBagGeneration,
      val input: CandidateSearchInput
  )

  private def prepared(
      hypergraph: QueryHypergraph,
      widthBound: Int,
      connected: Boolean
  ): Prepared = {
    val raw = SoftBagGenerator
      .generate(hypergraph, widthBound)
      .fold(failure => throw new AssertionError(failure.message), identity)
    val generation = raw.admissible(requireConnectedCover = connected)
    val input = CandidateSearchInput
      .create(hypergraph, widthBound, generation)
      .fold(
        errors => throw new AssertionError(errors.map(_.message).mkString("; ")),
        identity
      )
    new Prepared(hypergraph, raw, generation, input)
  }

  private def assertMatchesOracle(
      input: CandidateSearchInput,
      oracle: OracleEnumeration,
      planLimit: Int
  ): Option[Found] = {
    val expected = oracle.take(planLimit)
    TopKCandidateTD.enumerate(input, planLimit) match {
      case Left(failure) => throw new AssertionError(failure.message)
      case Right(found: Found) =>
        assert(expected.nonEmpty, s"production found plans but oracle rejected: $found")
        assertEquals(found.requestedLimit, planLimit)
        assertEquals(found.plans.map(_.rank), (1 to expected.size).toVector)
        assertEquals(
          found.plans.map(_.score.nodeCount),
          expected.map(_.tree.nodeCount)
        )
        assertEquals(
          found.plans.map(plan => toOracleTree(plan.witness.root)),
          expected.map(_.tree)
        )
        assertEquals(
          found.plans.map(_.decomposition),
          expected.map(_.decomposition)
        )
        found.plans.foreach { plan =>
          assertEquals(
            DecompositionValidator.validate(input.hypergraph, plan.decomposition),
            Right(plan.decomposition)
          )
          assertEquals(
            CandidateDecompositionValidator.validate(input, plan.decomposition),
            Right(plan.certificate)
          )
          assert(isIndependentCompNF(input.hypergraph, plan.decomposition))
        }
        Some(found)
      case Right(rejected: TopKRejected) =>
        assertEquals(rejected.requestedLimit, planLimit)
        assertEquals(expected, Vector.empty)
        None
    }
  }

  private def toOracleTree(node: CandidateTreeNode): OracleTree =
    canonicalTree(
      node.bag.vertices,
      node.children.map(toOracleTree)
    )

  private def topKFound(
      input: CandidateSearchInput,
      planLimit: Int
  ): Found = TopKCandidateTD
    .enumerate(input, planLimit)
    .fold(failure => throw new AssertionError(failure.message), identity) match {
    case found: Found => found
    case rejected: TopKRejected =>
      throw new AssertionError(s"expected top-K plans, got $rejected")
  }

  private def candidateLimits(
      maxWitnessNodes: Long = CandidateResourceLimits.default.maxWitnessNodes,
      maxPlanLimit: Long = CandidateResourceLimits.default.maxPlanLimit,
      maxRetainedPartialPlans: Long = CandidateResourceLimits.default.maxRetainedPartialPlans,
      maxCombinationStates: Long = CandidateResourceLimits.default.maxCombinationStates
  ): CandidateResourceLimits = CandidateResourceLimits
    .create(
      maxWitnessNodes = maxWitnessNodes,
      maxPlanLimit = maxPlanLimit,
      maxRetainedPartialPlans = maxRetainedPartialPlans,
      maxCombinationStates = maxCombinationStates
    )
    .fold(
      errors => throw new AssertionError(errors.map(_.message).mkString("; ")),
      identity
    )
}

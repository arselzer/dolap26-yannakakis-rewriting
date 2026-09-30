package at.ac.tuwien.dbai.yannakakis.algorithm.candidate

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidateCertificateError.{
  BagNotCandidate,
  ComponentNormalFormViolation,
  ConnectedCoverViolation,
  CoverNotAdmissible,
  EmptyRootBag,
  EqualAdjacentBags
}
import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidateDecision.{
  Accepted,
  Rejected
}
import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidateFailure.ResourceLimitExceeded
import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidateInputError.{
  DisconnectedHypergraph,
  EmptyHypergraph,
  HypergraphWithoutVertices
}
import at.ac.tuwien.dbai.yannakakis.algorithm.soft.{
  SoftBagGeneration,
  SoftBagGenerator
}
import at.ac.tuwien.dbai.yannakakis.core.QueryHypergraph
import at.ac.tuwien.dbai.yannakakis.core.decomposition.DecompositionValidator
import CandidateTDOracleTestSupport._

class CandidateTDOracleSuite extends munit.FunSuite {
  test("singleton/root-single-cover is one real bag, not an empty fake root") {
    val hypergraph = graph(Seq(Seq(0, 1)))
    val (_, input) = prepare(hypergraph, widthBound = 1, connected = true)
    val decision = decide(input)
    val accepted = assertAcceptedAndValidated(input, decision)
    val oracle = oracleInput(hypergraph, prepareGeneration(hypergraph, 1, true))

    val direct = directDecision(oracle)
    assert(direct.accepted)
    assertEquals(accepted.decomposition.bags.size, 1)
    assertEquals(accepted.decomposition.treeEdges, Vector.empty)
    assertEquals(accepted.decomposition.bags.head.vertices.map(_.value), Vector(0, 1))
    assertEquals(accepted.decomposition.bags.head.cover.map(_.value), Vector(0))
    assertEquals(
      bruteForceDistinctBagCanonical(oracle),
      Some(accepted.decomposition)
    )
    assertEquals(direct.canonicalDecomposition, Some(accepted.decomposition))
  }

  test("block limit accepts the exact boundary and fails one below") {
    val hypergraph = graph(Seq(Seq(0, 1)))
    val (_, input) = prepare(hypergraph, 1, connected = true)
    val exact = candidateLimits(maxBlocks = 3)
    val below = candidateLimits(maxBlocks = 2)

    assertAcceptedAndValidated(
      input,
      decide(input, exact)
    )
    assertEquals(
      BooleanCandidateTD.decide(input, below),
      Left(
        ResourceLimitExceeded(
          CandidateResource.Blocks,
          required = 3,
          limit = 2
        )
      )
    )
  }

  test("basis-check limit accepts the exact boundary and fails one below") {
    val hypergraph = graph(Seq(Seq(0, 1)))
    val (_, input) = prepare(hypergraph, 1, connected = true)
    val exact = candidateLimits(maxBasisChecks = 4)
    val below = candidateLimits(maxBasisChecks = 3)

    assertAcceptedAndValidated(
      input,
      decide(input, exact)
    )
    assertEquals(
      BooleanCandidateTD.decide(input, below),
      Left(
        ResourceLimitExceeded(
          CandidateResource.BasisChecks,
          required = 4,
          limit = 3
        )
      )
    )
  }

  test("basis-alternative limit accepts the exact boundary and fails one below") {
    val hypergraph = graph(Seq(Seq(0, 1)))
    val (_, input) = prepare(hypergraph, 1, connected = true)
    val exact = candidateLimits(maxBasisAlternatives = 2)
    val below = candidateLimits(maxBasisAlternatives = 1)

    assertAcceptedAndValidated(
      input,
      decide(input, exact)
    )
    assertEquals(
      BooleanCandidateTD.decide(input, below),
      Left(
        ResourceLimitExceeded(
          CandidateResource.BasisAlternatives,
          required = 2,
          limit = 1
        )
      )
    )
  }

  test("witness-node limit accepts a two-bag path at its exact boundary") {
    val hypergraph = graph(Seq(Seq(0, 1), Seq(1, 2)))
    val (_, input) = prepare(hypergraph, 1, connected = true)
    val exact = candidateLimits(maxWitnessNodes = 2)
    val below = candidateLimits(maxWitnessNodes = 1)

    val accepted = assertAcceptedAndValidated(input, decide(input, exact))
    assertEquals(accepted.witness.nodeCount, BigInt(2))
    assertEquals(
      BooleanCandidateTD.decide(input, below),
      Left(
        ResourceLimitExceeded(
          CandidateResource.WitnessNodes,
          required = 2,
          limit = 1
        )
      )
    )
  }

  test("deep candidate trees have stack-safe structural operations") {
    val depth = 20000
    val bag = CandidateBagKey.canonical(Vector(variable(0)))
    var left = new CandidateTreeNode(bag, Vector.empty)
    var right = new CandidateTreeNode(bag, Vector.empty)
    var level = 0
    while (level < depth) {
      left = new CandidateTreeNode(bag, Vector(left))
      right = new CandidateTreeNode(bag, Vector(right))
      level += 1
    }

    assertEquals(left.nodeCount, BigInt(depth + 1))
    assertEquals(CandidateOrdering.compareTreeNodes(left, right), 0)
    assert(left == right)
    assertEquals(left.hashCode(), right.hashCode())

    val leftTree = new CandidateTree(left)
    val rightTree = new CandidateTree(right)
    assert(leftTree == rightTree)
    assertEquals(leftTree.hashCode(), rightTree.hashCode())
    assertEquals(leftTree.toString, rightTree.toString)
  }

  test("empty and disconnected hypergraphs fail at the typed input boundary") {
    val emptyGeneration = SoftBagGenerator
      .generate(QueryHypergraph.empty, 1)
      .fold(failure => throw new AssertionError(failure.message), identity)
      .admissible(requireConnectedCover = false)
    val emptyErrors = CandidateSearchInput
      .create(QueryHypergraph.empty, 1, emptyGeneration)
      .swap
      .toOption
      .get

    assert(emptyErrors.contains(EmptyHypergraph))
    assert(emptyErrors.contains(HypergraphWithoutVertices))

    val disconnected = graph(Seq(Seq(0), Seq(1)))
    val disconnectedGeneration = SoftBagGenerator
      .generate(disconnected, 1)
      .fold(failure => throw new AssertionError(failure.message), identity)
      .admissible(requireConnectedCover = false)
    val disconnectedErrors = CandidateSearchInput
      .create(disconnected, 1, disconnectedGeneration)
      .swap
      .toOption
      .get

    assert(
      disconnectedErrors.exists(_.isInstanceOf[DisconnectedHypergraph])
    )
  }

  test("direct fixed point and distinct-bag rooted-tree oracle agree on small cases") {
    val cases = Vector(
      (graph(Seq(Seq(0, 1))), 1, false),
      (graph(Seq(Seq(0, 1), Seq(1, 2))), 1, false),
      (graph(Seq(Seq(0, 1), Seq(1, 2))), 1, true),
      (graph(Seq(Seq(0, 1), Seq(1, 2), Seq(2, 0))), 1, false),
      (graph(Seq(Seq(0, 1), Seq(1, 2), Seq(2, 0))), 2, true)
    )

    cases.foreach { case (hypergraph, widthBound, connected) =>
      val generation = prepareGeneration(hypergraph, widthBound, connected)
      val input = candidateInput(hypergraph, widthBound, generation)
      val oracle = oracleInput(hypergraph, generation)
      val directDecisionResult = directDecision(oracle)
      val direct = directDecisionResult.accepted
      val brute = bruteForceDistinctBagCanonical(oracle)
      val production = decide(input)

      assertEquals(direct, brute.nonEmpty)
      assertEquals(directDecisionResult.canonicalDecomposition, brute)
      assertEquals(isAccepted(production), direct)
      production match {
        case accepted: Accepted =>
          assertAcceptedAndValidated(input, accepted)
          assertEquals(brute, Some(accepted.decomposition))
        case _: Rejected => assert(brute.isEmpty)
      }
    }
  }

  test("triangle rejects at k=1 and accepts as a single bag at k=2") {
    val triangle = graph(Seq(Seq(0, 1), Seq(1, 2), Seq(2, 0)))
    val (_, widthOne) = prepare(triangle, 1, connected = false)
    val (_, widthTwo) = prepare(triangle, 2, connected = true)

    assert(decide(widthOne).isInstanceOf[Rejected])
    val accepted = assertAcceptedAndValidated(widthTwo, decide(widthTwo))
    assertEquals(accepted.decomposition.bags.size, 1)
    assertEquals(
      accepted.decomposition.bags.head.vertices.map(_.value),
      Vector(0, 1, 2)
    )
  }

  test("C5 ConCov rejects at k=2 and accepts at k=3") {
    val cycle = graph(
      Seq(
        Seq(0, 1),
        Seq(1, 2),
        Seq(2, 3),
        Seq(3, 4),
        Seq(4, 0)
      )
    )
    val widthTwoGeneration = prepareGeneration(cycle, 2, connected = true)
    val widthThreeGeneration = prepareGeneration(cycle, 3, connected = true)
    val widthTwo = candidateInput(cycle, 2, widthTwoGeneration)
    val widthThree = candidateInput(cycle, 3, widthThreeGeneration)

    assert(!directDecision(oracleInput(cycle, widthTwoGeneration)).accepted)
    assert(decide(widthTwo).isInstanceOf[Rejected])
    val directWidthThree = directDecision(
      oracleInput(cycle, widthThreeGeneration)
    )
    assert(directWidthThree.accepted)
    val accepted = assertAcceptedAndValidated(widthThree, decide(widthThree))
    assertEquals(
      directWidthThree.canonicalDecomposition,
      Some(accepted.decomposition)
    )
  }

  test("duplicate edge vertex sets preserve self-join occurrence cover identity") {
    val repeated = graph(Seq(Seq(0, 1), Seq(0, 1)))
    val (generation, input) = prepare(repeated, 1, connected = true)
    val full = generation.candidates
      .find(_.vertices.map(_.value) == Vector(0, 1))
      .get

    assertEquals(
      full.covers.map(_.cover.edgeIds.map(_.value)),
      Vector(Vector(0), Vector(1))
    )
    val accepted = assertAcceptedAndValidated(input, decide(input))
    assertEquals(accepted.decomposition.bags.head.cover.map(_.value), Vector(0))
    assertEquals(
      bruteForceDistinctBagCanonical(
        oracleInput(
          repeated,
          generation.admissible(requireConnectedCover = true)
        )
      ),
      Some(accepted.decomposition)
    )
  }

  test("many derivation witnesses remain one structural CandidateTD state") {
    val repeated = graph(Vector.fill(5)(Seq(0, 1)))
    val (generation, input) = prepare(repeated, 2, connected = true)
    val full = generation.candidates
      .find(_.vertices.map(_.value) == Vector(0, 1))
      .get

    assert(full.witnesses.size >= 15)
    assertEquals(input.candidateBags.size, generation.candidates.size)
    val accepted = assertAcceptedAndValidated(input, decide(input))
    assertEquals(
      accepted.statistics.candidateBagCount,
      generation.candidates.size
    )
    assertEquals(accepted.decomposition.bags.size, 1)
  }

  test("basis recurrence rejects X=S and X outside extent, but permits lower-rank Y=C") {
    val hypergraph = graph(Seq(Seq(0, 1), Seq(0, 2)))
    val (_, input) = prepare(hypergraph, 2, connected = false)
    val index = CandidateBlockIndex
      .build(input)
      .fold(failure => throw new AssertionError(failure.message), identity)

    val narrowParent = index.blocks.find { block =>
      block.head.vertices.map(_.value) == Vector(0) &&
      block.tail.vertices.map(_.value) == Vector(1)
    }.get
    val narrowBases = index.bases(narrowParent)

    assert(
      !narrowBases.exists(_.bag.vertices.map(_.value) == Vector(0)),
      "X=S must not be a basis, even when its own subordinate block would cover C"
    )
    assert(
      !narrowBases.exists(_.bag.vertices.map(_.value) == Vector(0, 1, 2)),
      "a covering X outside S union C must not be a basis"
    )

    val shrinkingParent = index.blocks.find { block =>
      block.head.vertices.map(_.value) == Vector(0, 1) &&
      block.tail.vertices.map(_.value) == Vector(2)
    }.get
    val shrinkingBasis = index.bases(shrinkingParent).find { basis =>
      basis.bag.vertices.map(_.value) == Vector(0)
    }.get
    val sameTailChild = shrinkingBasis.subordinateBlocks.find { child =>
      child.tail.vertices == shrinkingParent.tail.vertices
    }.get

    assertEquals(sameTailChild.tail.vertices, shrinkingParent.tail.vertices)
    assertEquals(sameTailChild.head.vertices.map(_.value), Vector(0))
    assert(
      sameTailChild.head.vertices.size < shrinkingParent.head.vertices.size,
      "Y=C is valid here because the lexicographic rank drops in head size"
    )
  }

  test("equal adjacent X=S is rejected rather than bypassed by its cover") {
    val hypergraph = graph(Seq(Seq(0, 1), Seq(0, 2)))
    val (_, input) = prepare(hypergraph, 1, connected = false)
    val malformed = coveredDecomposition(
      root = 0,
      bags = Vector(
        coveredBag(0, Vector(0, 1), Vector(0)),
        coveredBag(1, Vector(0, 1), Vector(0)),
        coveredBag(2, Vector(0, 2), Vector(1))
      ),
      edges = Vector(treeEdge(0, 1), treeEdge(1, 2))
    )

    assertEquals(DecompositionValidator.validate(hypergraph, malformed), Right(malformed))
    val errors = certificateErrors(input, malformed)
    assert(errors.exists(_.isInstanceOf[EqualAdjacentBags]))
  }

  test("a child subtree outside one parent-component extent is not CompNF") {
    val hypergraph = graph(Seq(Seq(0, 1), Seq(0, 2)))
    val (_, input) = prepare(hypergraph, 1, connected = false)
    val malformed = coveredDecomposition(
      root = 0,
      bags = Vector(
        coveredBag(0, Vector(0), Vector(0)),
        coveredBag(1, Vector(0, 1), Vector(0)),
        coveredBag(2, Vector(0, 2), Vector(1))
      ),
      edges = Vector(treeEdge(0, 1), treeEdge(1, 2))
    )

    assertEquals(DecompositionValidator.validate(hypergraph, malformed), Right(malformed))
    assert(!isIndependentCompNF(hypergraph, malformed))
    val errors = certificateErrors(input, malformed)
    assert(errors.exists(_.isInstanceOf[ComponentNormalFormViolation]))
  }

  test("certificate validation rejects bags absent from the candidate set") {
    val triangle = graph(Seq(Seq(0, 1), Seq(1, 2), Seq(2, 0)))
    val (_, input) = prepare(triangle, 1, connected = false)
    val malformed = coveredDecomposition(
      root = 0,
      bags = Vector(coveredBag(0, Vector(0, 1, 2), Vector(0, 1))),
      edges = Vector.empty
    )

    assertEquals(DecompositionValidator.validate(triangle, malformed), Right(malformed))
    assert(certificateErrors(input, malformed).exists(_.isInstanceOf[BagNotCandidate]))
  }

  test("certificate validation rejects a covering but unadvertised physical cover") {
    val repeated = graph(Seq(Seq(0, 1), Seq(0, 1)))
    val (_, input) = prepare(repeated, 1, connected = false)
    val malformed = coveredDecomposition(
      root = 0,
      bags = Vector(coveredBag(0, Vector(0, 1), Vector(0, 1))),
      edges = Vector.empty
    )

    assertEquals(DecompositionValidator.validate(repeated, malformed), Right(malformed))
    assert(
      certificateErrors(input, malformed).exists(
        _.isInstanceOf[CoverNotAdmissible]
      )
    )
  }

  test("connected-cover admissibility is enforced only in ConCov mode") {
    val hypergraph = graph(Seq(Seq(0), Seq(1), Seq(0, 1)))
    val (_, unconstrained) = prepare(hypergraph, 2, connected = false)
    val (_, constrained) = prepare(hypergraph, 2, connected = true)
    val disconnectedCover = coveredDecomposition(
      root = 0,
      bags = Vector(coveredBag(0, Vector(0, 1), Vector(0, 1))),
      edges = Vector.empty
    )

    assert(
      CandidateDecompositionValidator
        .validate(unconstrained, disconnectedCover)
        .isRight
    )
    val errors = certificateErrors(constrained, disconnectedCover)
    assert(errors.exists(_.isInstanceOf[CoverNotAdmissible]))
    assert(errors.exists(_.isInstanceOf[ConnectedCoverViolation]))
  }

  test("edge input permutation cannot change the canonical decision") {
    val edges = Vector(
      40 -> Seq(3, 4),
      10 -> Seq(0, 1),
      30 -> Seq(2, 3),
      20 -> Seq(1, 2)
    )
    val expectedGraph = graphWithIds(edges)
    val (_, expectedInput) = prepare(expectedGraph, 1, connected = true)
    val expected = assertAcceptedAndValidated(
      expectedInput,
      decide(expectedInput)
    )

    edges.permutations.foreach { permutation =>
      val hypergraph = graphWithIds(permutation)
      val (_, input) = prepare(hypergraph, 1, connected = true)
      val actual = assertAcceptedAndValidated(input, decide(input))
      assertEquals(actual, expected)
    }
  }

  test("candidate input rejects generation from a different same-id hypergraph") {
    val source = graphWithIds(
      Seq(
        0 -> Seq(0, 1),
        1 -> Seq(1, 2)
      )
    )
    val requested = graphWithIds(
      Seq(
        0 -> Seq(0, 2),
        1 -> Seq(1, 2)
      )
    )
    assertEquals(source.edgeIds, requested.edgeIds)
    assertEquals(source.variables, requested.variables)
    assert(source != requested)

    val generation = prepareGeneration(source, 1, connected = false)
    val errors = CandidateSearchInput
      .create(requested, 1, generation)
      .swap
      .toOption
      .getOrElse(Vector.empty)

    assert(
      errors.exists(
        _.isInstanceOf[CandidateInputError.SourceHypergraphMismatch]
      )
    )
  }

  test("candidate input rejects a generation width provenance mismatch") {
    val hypergraph = graph(Seq(Seq(0, 1), Seq(1, 2)))
    val generation = prepareGeneration(hypergraph, 1, connected = false)
    val errors = CandidateSearchInput
      .create(hypergraph, 2, generation)
      .swap
      .toOption
      .getOrElse(Vector.empty)

    assert(
      errors.exists(_.isInstanceOf[CandidateInputError.SourceWidthMismatch])
    )
  }

  test("candidate certificate rejects an empty actual root") {
    val hypergraph = graph(Seq(Seq(0, 1)))
    val (_, input) = prepare(hypergraph, 1, connected = true)
    val fakeRoot = coveredDecomposition(
      root = 0,
      bags = Vector(
        coveredBag(0, Vector.empty, Vector(0)),
        coveredBag(1, Vector(0, 1), Vector(0))
      ),
      edges = Vector(treeEdge(0, 1))
    )

    assertEquals(
      DecompositionValidator.validate(hypergraph, fakeRoot),
      Right(fakeRoot)
    )
    assert(
      certificateErrors(input, fakeRoot).exists(_.isInstanceOf[EmptyRootBag])
    )
  }

  private def prepare(
      hypergraph: QueryHypergraph,
      widthBound: Int,
      connected: Boolean
  ): (SoftBagGeneration, CandidateSearchInput) = {
    val raw = SoftBagGenerator
      .generate(hypergraph, widthBound)
      .fold(failure => throw new AssertionError(failure.message), identity)
    val admissible = raw.admissible(requireConnectedCover = connected)
    raw -> candidateInput(hypergraph, widthBound, admissible)
  }

  private def prepareGeneration(
      hypergraph: QueryHypergraph,
      widthBound: Int,
      connected: Boolean
  ) = SoftBagGenerator
    .generate(hypergraph, widthBound)
    .fold(failure => throw new AssertionError(failure.message), identity)
    .admissible(requireConnectedCover = connected)

  private def candidateInput(
      hypergraph: QueryHypergraph,
      widthBound: Int,
      generation: at.ac.tuwien.dbai.yannakakis.algorithm.soft.AdmissibleSoftBagGeneration
  ): CandidateSearchInput =
    CandidateSearchInput
      .create(hypergraph, widthBound, generation)
      .fold(
        errors => throw new AssertionError(errors.map(_.message).mkString("; ")),
        identity
      )

  private def decide(input: CandidateSearchInput): CandidateDecision =
    BooleanCandidateTD
      .decide(input)
      .fold(failure => throw new AssertionError(failure.message), identity)

  private def decide(
      input: CandidateSearchInput,
      limits: CandidateResourceLimits
  ): CandidateDecision =
    BooleanCandidateTD
      .decide(input, limits)
      .fold(failure => throw new AssertionError(failure.message), identity)

  private def candidateLimits(
      maxBlocks: Long = CandidateResourceLimits.default.maxBlocks,
      maxBasisChecks: Long = CandidateResourceLimits.default.maxBasisChecks,
      maxBasisAlternatives: Long = CandidateResourceLimits.default.maxBasisAlternatives,
      maxWitnessNodes: Long = CandidateResourceLimits.default.maxWitnessNodes
  ): CandidateResourceLimits =
    CandidateResourceLimits
      .create(
        maxBlocks,
        maxBasisChecks,
        maxBasisAlternatives,
        maxWitnessNodes
      )
      .fold(
        errors => throw new AssertionError(errors.map(_.message).mkString("; ")),
        identity
      )

  private def isAccepted(decision: CandidateDecision): Boolean =
    decision.isInstanceOf[Accepted]

  private def assertAcceptedAndValidated(
      input: CandidateSearchInput,
      decision: CandidateDecision
  ): Accepted = decision match {
    case accepted: Accepted =>
      assertEquals(
        DecompositionValidator.validate(input.hypergraph, accepted.decomposition),
        Right(accepted.decomposition)
      )
      assertEquals(
        CandidateDecompositionValidator.validate(input, accepted.decomposition),
        Right(accepted.certificate)
      )
      assert(isIndependentCompNF(input.hypergraph, accepted.decomposition))
      accepted
    case rejected: Rejected =>
      throw new AssertionError(s"expected acceptance, got $rejected")
  }

  private def certificateErrors(
      input: CandidateSearchInput,
      decomposition: at.ac.tuwien.dbai.yannakakis.core.decomposition.CoveredDecomposition
  ): Vector[CandidateCertificateError] =
    CandidateDecompositionValidator
      .validate(input, decomposition)
      .swap
      .toOption
      .getOrElse(Vector.empty)
}

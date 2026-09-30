package at.ac.tuwien.dbai.yannakakis.algorithm.soft

import at.ac.tuwien.dbai.yannakakis.algorithm.soft.SoftFailure.ResourceLimitExceeded
import at.ac.tuwien.dbai.yannakakis.algorithm.soft.SoftResource.{
  CandidateCoverChecks,
  Candidates,
  Components,
  CoverPairs,
  Derivations
}
import at.ac.tuwien.dbai.yannakakis.core.VariableId
import org.scalacheck.{Gen, Prop}
import SoftTestSupport._

class SoftBagGeneratorSuite extends munit.ScalaCheckSuite {
  test("chain, triangle, and five-cycle match the direct definition") {
    val fixtures = Vector(
      graph(Seq(Seq(0, 1), Seq(1, 2), Seq(2, 3))),
      graph(Seq(Seq(0, 1), Seq(1, 2), Seq(2, 0))),
      graph(
        Seq(
          Seq(0, 1),
          Seq(1, 2),
          Seq(2, 3),
          Seq(3, 4),
          Seq(4, 0)
        )
      )
    )

    fixtures.foreach { hypergraph =>
      val actual = generatedBags(hypergraph, widthBound = 2)
      assertEquals(actual, directSoftBags(hypergraph, widthBound = 2))
    }
  }

  test("edge-distinct witnesses and candidate covers survive structural deduplication") {
    val repeated = graph(Seq(Seq(0, 1), Seq(0, 1), Seq(1, 2)))
    val generation = SoftBagGenerator.generate(repeated, 1).toOption.get
    val repeatedBag = candidate(generation, 0, 1)

    assert(
      repeatedBag.witnesses.exists(_.lambda1 == cover(0)),
      clues(repeatedBag.witnesses)
    )
    assert(
      repeatedBag.witnesses.exists(_.lambda1 == cover(1)),
      clues(repeatedBag.witnesses)
    )
    assertEquals(
      repeatedBag.covers.map(_.cover).filter(c => c == cover(0) || c == cover(1)),
      Vector(cover(0), cover(1))
    )
  }

  test("singleton and empty edges follow the definition without ad-hoc bag filtering") {
    val withSingleton = graph(Seq(Seq.empty, Seq(0)))
    val generation = SoftBagGenerator.generate(withSingleton, 1).toOption.get

    assertEquals(generation.candidates.map(vertexValues), Vector(Vector.empty, Vector(0)))
    assert(generation.enumeratedCovers.contains(EdgeCover.empty))
    assert(generation.candidates.forall(_.covers.forall(_.cover.nonEmpty)))
    assert(candidate(generation).covers.exists(_.cover == cover(0)))
    assert(candidate(generation).hasConnectedCover)
    assert(candidate(generation, 0).hasConnectedCover)

    val onlyEmptyEdges = graph(Seq(Seq.empty, Seq.empty))
    assertEquals(
      SoftBagGenerator.generate(onlyEmptyEdges, 1).toOption.get.candidates,
      Vector.empty
    )
  }

  test("ConCov is existential over independently enumerated bag covers") {
    val hypergraph = graph(Seq(Seq(0), Seq(1), Seq(0, 1)))
    val generation = SoftBagGenerator.generate(hypergraph, 2).toOption.get
    val bag = candidate(generation, 0, 1)
    val disconnected = bag.covers.find(_.cover == cover(0, 1)).get
    val connectedAlternative = bag.covers.find(_.cover == cover(2)).get

    assert(!disconnected.isConnected)
    assert(connectedAlternative.isConnected)
    assert(bag.hasConnectedCover)
  }

  test("empty cover is not connected while every singleton occurrence is") {
    val hypergraph = graph(Seq(Seq.empty, Seq(0)))

    assertEquals(ConnectedCover.isConnected(hypergraph, cover()), Right(false))
    assertEquals(ConnectedCover.isConnected(hypergraph, cover(0)), Right(true))
    assertEquals(ConnectedCover.isConnected(hypergraph, cover(1)), Right(true))
    assertEquals(ConnectedCover.isConnected(hypergraph, cover(0, 1)), Right(false))
    assertEquals(
      ConnectedCover.covers(hypergraph, cover(), null),
      Left(SoftFailure.MissingInput("bag vertices"))
    )
    assertEquals(
      ConnectedCover.covers(
        hypergraph,
        cover(1),
        Vector(variable(0), null.asInstanceOf[VariableId])
      ),
      Left(SoftFailure.NullVertexInBag)
    )
  }

  test("admissibility projection makes unconstrained and ConCov modes explicit") {
    val chain = graph(Seq(Seq(0, 1), Seq(1, 2), Seq(2, 3)))
    val generation = SoftBagGenerator.generate(chain, 2).toOption.get
    val unconstrained = generation.admissible(requireConnectedCover = false)
    val constrained = generation.admissible(requireConnectedCover = true)

    assert(!unconstrained.requireConnectedCover)
    assertEquals(unconstrained.definitionCovers, generation.enumeratedCovers)
    assertEquals(
      unconstrained.candidates.map(_.vertices),
      generation.candidates.map(_.vertices)
    )
    assertEquals(
      unconstrained.candidates.map(_.physicalCovers),
      generation.candidates.map(_.covers)
    )
    assert(
      unconstrained.candidates.exists(
        _.vertices.map(_.value) == Vector(0, 1, 2, 3)
      )
    )

    assert(constrained.requireConnectedCover)
    assert(
      constrained.candidates.forall(
        _.physicalCovers.nonEmpty
      )
    )
    assert(
      constrained.candidates.forall(
        _.physicalCovers.forall(_.isConnected)
      )
    )
    assert(
      !constrained.candidates.exists(
        _.vertices.map(_.value) == Vector(0, 1, 2, 3)
      )
    )

    val alternatives = SoftBagGenerator
      .generate(graph(Seq(Seq(0), Seq(1), Seq(0, 1))), 2)
      .toOption
      .get
    val unconstrainedBag = alternatives
      .admissible(requireConnectedCover = false)
      .candidates
      .find(_.vertices.map(_.value) == Vector(0, 1))
      .get
    val constrainedBag = alternatives
      .admissible(requireConnectedCover = true)
      .candidates
      .find(_.vertices.map(_.value) == Vector(0, 1))
      .get

    assert(unconstrainedBag.physicalCovers.exists(cover => !cover.isConnected))
    assertEquals(
      constrainedBag.physicalCovers,
      unconstrainedBag.physicalCovers.filter(_.isConnected)
    )
  }

  test("cover-pair resource exhaustion is explicit and deterministic") {
    val hypergraph = graph(Seq(Seq(0), Seq(1), Seq(2)))
    val limits = SoftResourceLimits.create(maxCoverPairs = 15).toOption.get

    assertEquals(
      SoftBagGenerator.generate(hypergraph, 1, limits),
      Left(ResourceLimitExceeded(CoverPairs, required = 16, limit = 15))
    )
  }

  test("component resource limit accepts the exact boundary and rejects one less") {
    val hypergraph = graph(Seq(Seq(0, 1), Seq(1, 2)))
    val exact = SoftResourceLimits.create(maxComponents = 3).toOption.get
    val below = SoftResourceLimits.create(maxComponents = 2).toOption.get

    assert(SoftBagGenerator.generate(hypergraph, 1, exact).isRight)
    assertEquals(
      SoftBagGenerator.generate(hypergraph, 1, below),
      Left(ResourceLimitExceeded(Components, required = 3, limit = 2))
    )
  }

  test("derivation resource limit accepts the exact boundary and rejects one less") {
    val hypergraph = graph(Seq(Seq(0, 1), Seq(1, 2)))
    val exact = SoftResourceLimits.create(maxDerivations = 9).toOption.get
    val below = SoftResourceLimits.create(maxDerivations = 8).toOption.get

    assert(SoftBagGenerator.generate(hypergraph, 1, exact).isRight)
    assertEquals(
      SoftBagGenerator.generate(hypergraph, 1, below),
      Left(ResourceLimitExceeded(Derivations, required = 9, limit = 8))
    )
  }

  test("candidate resource limit accepts the exact boundary and rejects one less") {
    val hypergraph = graph(Seq(Seq(0, 1), Seq(1, 2)))
    val exact = SoftResourceLimits.create(maxCandidates = 4).toOption.get
    val below = SoftResourceLimits.create(maxCandidates = 3).toOption.get

    assert(SoftBagGenerator.generate(hypergraph, 1, exact).isRight)
    assertEquals(
      SoftBagGenerator.generate(hypergraph, 1, below),
      Left(ResourceLimitExceeded(Candidates, required = 4, limit = 3))
    )
  }

  test("cover-check resource limit accepts the exact boundary and rejects one less") {
    val hypergraph = graph(Seq(Seq(0, 1), Seq(1, 2)))
    val exact = SoftResourceLimits
      .create(maxCandidateCoverChecks = 8)
      .toOption
      .get
    val below = SoftResourceLimits
      .create(maxCandidateCoverChecks = 7)
      .toOption
      .get

    assert(SoftBagGenerator.generate(hypergraph, 1, exact).isRight)
    assertEquals(
      SoftBagGenerator.generate(hypergraph, 1, below),
      Left(
        ResourceLimitExceeded(
          CandidateCoverChecks,
          required = 8,
          limit = 7
        )
      )
    )
  }

  property("input edge permutation cannot affect covers, witnesses, or bags") {
    val edges = Vector(
      40 -> Seq(4, 0),
      10 -> Seq(1, 2),
      30 -> Seq(3, 4),
      0 -> Seq(0, 1),
      20 -> Seq(2, 3)
    )
    val expected = SoftBagGenerator.generate(graphWithIds(edges), 2)

    Prop.forAll(Gen.pick(edges.size, edges)) { permuted =>
      assertEquals(
        SoftBagGenerator.generate(graphWithIds(permuted.toSeq), 2),
        expected
      )
    }
  }

  property("small hypergraphs agree with the full independent oracle") {
    Prop.forAll(smallHypergraph, Gen.choose(1, 2)) { (hypergraph, widthBound) =>
      assertEquals(
        generatedEvidence(
          SoftBagGenerator.generate(hypergraph, widthBound).toOption.get
        ),
        directSoftCandidates(hypergraph, widthBound)
      )
    }
  }

  property("small-hypergraph components agree with the vertex-path oracle") {
    Prop.forAll(smallHypergraph, Gen.choose(1, 2)) { (hypergraph, widthBound) =>
      directCovers(hypergraph.edgeIds, widthBound).foreach { separatorIds =>
        val separator = EdgeCover.create(separatorIds).toOption.get
        assertEquals(
          EdgeComponentEngine
            .components(hypergraph, separator)
            .toOption
            .get
            .map(component => component.edgeIds.map(_.value)),
          directComponents(hypergraph, separatorIds)
            .map(component => component.toVector.sorted.map(_.value))
        )
      }
    }
  }

  private val smallHypergraph = for {
    edgeCount <- Gen.choose(0, 5)
    vertexSets <- Gen.listOfN(
      edgeCount,
      Gen.someOf(Vector(0, 1, 2, 3)).map(_.toVector.sorted)
    )
  } yield graph(vertexSets)

  private def generatedBags(
      hypergraph: at.ac.tuwien.dbai.yannakakis.core.QueryHypergraph,
      widthBound: Int
  ): Set[Vector[Int]] =
    SoftBagGenerator
      .generate(hypergraph, widthBound)
      .toOption
      .get
      .candidates
      .map(vertexValues)
      .toSet
}

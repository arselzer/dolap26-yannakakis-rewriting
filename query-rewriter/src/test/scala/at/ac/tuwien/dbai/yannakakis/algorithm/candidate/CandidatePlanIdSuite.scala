package at.ac.tuwien.dbai.yannakakis.algorithm.candidate

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidatePlanDecision.{
  Found,
  Rejected
}
import at.ac.tuwien.dbai.yannakakis.algorithm.soft.SoftBagGenerator

class CandidatePlanIdSuite extends munit.FunSuite {
  import CandidateTDOracleTestSupport.{
    coveredBag,
    coveredDecomposition,
    graph,
    treeEdge
  }

  test("candidate ids retain the frozen canonical decomposition JSON identity") {
    val canonical = coveredDecomposition(
      root = 0,
      bags = Vector(coveredBag(0, Vector(0, 1, 2), Vector(0, 1))),
      edges = Vector.empty
    )
    val reorderedInput = coveredDecomposition(
      root = 0,
      bags = List(coveredBag(0, List(2, 0, 1), List(1, 0))),
      edges = List.empty
    )

    assertEquals(CandidatePlanId.Scheme, "covered_decomposition_json_v1")
    assertEquals(
      CandidatePlanId.canonicalJson(canonical),
      "{" +
        "\"root_bag_id\":0,\"width\":2," +
        "\"bags\":[{\"id\":0,\"vertices\":[0,1,2]," +
        "\"cover_edge_ids\":[0,1]}],\"tree_edges\":[]}"
    )
    assertEquals(
      CandidatePlanId.from(canonical).value,
      "sha256:b6f1006706778b364299cb342db7fb6087b8021bd24437c90de8d674135e8e9d"
    )
    assertEquals(CandidatePlanId.from(reorderedInput), CandidatePlanId.from(canonical))
    assertEquals(
      CandidatePlanId.parse(CandidatePlanId.from(canonical).value),
      Right(CandidatePlanId.from(canonical))
    )
  }

  test("candidate ids include root, topology, bags, and selected covers") {
    val twoBag = coveredDecomposition(
      root = 0,
      bags = Vector(
        coveredBag(1, Vector(2, 1, 0), Vector(1, 0)),
        coveredBag(0, Vector(1, 0), Vector(0))
      ),
      edges = Vector(treeEdge(1, 0))
    )
    val sameCanonicalValue = coveredDecomposition(
      root = 0,
      bags = Vector(
        coveredBag(0, Vector(0, 1), Vector(0)),
        coveredBag(1, Vector(0, 1, 2), Vector(0, 1))
      ),
      edges = Vector(treeEdge(0, 1))
    )
    val differentRoot = coveredDecomposition(
      root = 1,
      bags = twoBag.bags,
      edges = twoBag.treeEdges
    )
    val differentCover = coveredDecomposition(
      root = 0,
      bags = Vector(
        coveredBag(0, Vector(0, 1), Vector(2)),
        coveredBag(1, Vector(0, 1, 2), Vector(0, 1))
      ),
      edges = Vector(treeEdge(0, 1))
    )

    assertEquals(CandidatePlanId.from(twoBag), CandidatePlanId.from(sameCanonicalValue))
    assertEquals(
      CandidatePlanId.from(twoBag).value,
      "sha256:3db4f5752c2a2a6de0c2f6c71b80fd0d39af82a9b347750026f29f46b8572d43"
    )
    assertNotEquals(CandidatePlanId.from(differentRoot), CandidatePlanId.from(twoBag))
    assertNotEquals(CandidatePlanId.from(differentCover), CandidatePlanId.from(twoBag))
  }

  test("candidate id parsing rejects normalization and malformed digests") {
    val canonical =
      "sha256:b6f1006706778b364299cb342db7fb6087b8021bd24437c90de8d674135e8e9d"
    val invalid = Vector[String](
      " " + canonical,
      canonical + " ",
      canonical.toUpperCase,
      canonical.replace("sha256:", "SHA256:"),
      canonical.dropRight(1),
      canonical + "0",
      canonical.updated(canonical.length - 1, 'g'),
      canonical.stripPrefix("sha256:")
    )

    assertEquals(
      CandidatePlanId.parse(null).left.toOption.map(_.code),
      Some("candidate-id.missing")
    )
    invalid.foreach { value =>
      assertEquals(
        CandidatePlanId.parse(value).left.toOption.map(_.code),
        Some("candidate-id.invalid-format"),
        value
      )
    }
  }

  test("ranked candidate ids are stable across exact top-k prefix sizes") {
    val hypergraph = graph(
      Seq(Seq(0, 1), Seq(0, 2), Seq(1, 2))
    )
    val generated = SoftBagGenerator
      .generate(hypergraph, widthBound = 2)
      .fold(problem => fail(problem.message), identity)
    val input = CandidateSearchInput
      .create(
        hypergraph,
        widthBound = 2,
        generated.admissible(requireConnectedCover = true)
      )
      .fold(problems => fail(problems.map(_.message).mkString("; ")), identity)

    val firstTwo = found(input, planLimit = 2).plans
    val all = found(input, planLimit = 10).plans

    assertEquals(all.size, 4)
    assertEquals(firstTwo.map(_.candidateId), all.take(2).map(_.candidateId))
    assertEquals(all.map(_.candidateId).distinct.size, all.size)
    assertEquals(
      all.map(_.candidateId.value),
      Vector(
        "sha256:b6f1006706778b364299cb342db7fb6087b8021bd24437c90de8d674135e8e9d",
        "sha256:3db4f5752c2a2a6de0c2f6c71b80fd0d39af82a9b347750026f29f46b8572d43",
        "sha256:2267a158b501a94ad46e8a9056e7f818afd744626099edbb5b0c20db4dcaf876",
        "sha256:db0b21f268414ce32cfb9359f326bf3fa7a0a62d87751884568a36d822efca8b"
      )
    )
    all.foreach(plan => assertEquals(plan.candidateId, CandidatePlanId.from(plan.decomposition)))
  }

  private def found(
      input: CandidateSearchInput,
      planLimit: Int
  ): Found = TopKCandidateTD
    .enumerate(input, planLimit)
    .fold(problem => fail(problem.message), identity) match {
    case value: Found => value
    case value: Rejected => fail(s"expected retained candidates, received $value")
  }
}

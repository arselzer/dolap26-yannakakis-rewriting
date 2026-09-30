package at.ac.tuwien.dbai.yannakakis.cli

import java.nio.charset.StandardCharsets

import at.ac.tuwien.dbai.yannakakis.api.postgresql.{
  PostgreSqlEnumerationBatchArtifact,
  PostgreSqlEnumerationBatchRequest,
  PostgreSqlEnumerationCampaignV2ProjectionView,
  PostgreSqlQueryRewriter
}
import at.ac.tuwien.dbai.yannakakis.calcite.{
  CalcitePlannerFixture,
  CalciteTestSchemas
}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.CalciteSqlPlanner

class PostgreSqlEnumerationCampaignV2CumulativeBudgetSuite
    extends munit.FunSuite {
  import PostgreSqlEnumerationCumulativeBudgetFailure._
  import PostgreSqlEnumerationCumulativeByteResource._
  import PostgreSqlEnumerationCumulativeItemKind._

  private val triangleSql =
    """SELECT r.a AS x, r.b, s.c
      |FROM triangle_r AS r
      |JOIN triangle_s AS s ON r.b = s.b
      |JOIN triangle_t AS t ON s.c = t.c AND t.a = r.a""".stripMargin
  private def authoring(
      artifact: PostgreSqlEnumerationBatchArtifact
  ): PostgreSqlEnumerationCampaignV2AuthoringRequest =
    PostgreSqlEnumerationCampaignV2AuthoringRequest
      .create(
        PostgreSqlEnumerationCampaignV2AuthoringInput(
          sourceSql = artifact.request.originalSql,
          workloadId = "triangle-cell-01",
          lineageId = "triangle-lineage-v2",
          widthBound = artifact.request.widthBound,
          requestedK = artifact.request.planLimit,
          requireConnectedCover = artifact.request.requireConnectedCover,
          dataContract = PostgreSqlEnumerationCampaignV2DataContract(
            "triangle-fixture-v2",
            "a" * 64,
            "b" * 64,
            "c" * 64
          ),
          producerEntrypoint = "campaign-v2-test",
          toolchainComponents = Vector(
            PostgreSqlEnumerationCampaignV2ToolchainComponent(
              "query-rewriter",
              "test",
              "d" * 64
            )
          )
        )
      )
      .fold(problem => fail(problem), identity)

  test("default gate exactly accounts every logical SQL and canonical document byte") {
    val artifact = produce(planLimit = 10)
    val view = PostgreSqlEnumerationCampaignV2ProjectionView.from(artifact)
    val bounded = PostgreSqlEnumerationCampaignV2CumulativeBudget
      .project(artifact, authoring(artifact))
      .fold(failure => fail(failure.toString), identity)
    val projection = bounded.projection
    val evidence = bounded.evidence
    val usage = evidence.usage

    val generatedBytes = view.candidates.iterator
      .map(_.renderedSql.getBytes(StandardCharsets.UTF_8).length.toLong)
      .sum
    val stageBytes = view.candidates.iterator
      .flatMap(_.orderedStages.iterator)
      .map(_.sql.getBytes(StandardCharsets.UTF_8).length.toLong)
      .sum
    val documents = projectionDocuments(projection)
    val documentBytes = documents.iterator.map(_.byteLength.toLong).sum

    assertEquals(usage.sourceSqlItemCount, 1)
    assertEquals(
      usage.sourceSqlBytes,
      view.sourceSql.getBytes(StandardCharsets.UTF_8).length.toLong
    )
    assertEquals(usage.generatedSqlItemCount, view.candidates.size)
    assertEquals(usage.generatedSqlBytes, generatedBytes)
    assertEquals(
      usage.stageSqlItemCount,
      view.candidates.iterator.map(_.orderedStages.size).sum
    )
    assertEquals(usage.stageSqlBytes, stageBytes)
    assertEquals(
      usage.allSqlBytes,
      usage.sourceSqlBytes + generatedBytes + stageBytes
    )
    assertEquals(usage.canonicalJsonDocumentCount, documents.size)
    assertEquals(usage.canonicalJsonDocumentBytes, documentBytes)
    assertEquals(
      documents.size,
      8 + 4 * view.candidates.size
    )
    assertEquals(
      evidence.items.count(_.kind == CanonicalJsonDocument),
      documents.size
    )
    assertEquals(
      evidence.accountingScheme,
      "enumeration-campaign-v2-cumulative-logical-utf8-bytes-v2"
    )
    assertEquals(
      PostgreSqlEnumerationCampaignV2CumulativeBudget
        .account(artifact, projection)
        .fold(failure => fail(failure.toString), identity),
      evidence
    )
  }

  test("every cumulative resource fails closed at its exact byte boundary") {
    val artifact = produce(planLimit = 10)
    val projection = PostgreSqlEnumerationCampaignV2Json.project(
      artifact,
      authoring(artifact)
    )
    val baseline = PostgreSqlEnumerationCampaignV2CumulativeBudget
      .account(artifact, projection)
      .fold(failure => fail(failure.toString), identity)
      .usage

    val cases = Vector(
      SourceSqlBytes -> limits(
        baseline,
        source = baseline.sourceSqlBytes - 1L
      ),
      GeneratedSqlBytes -> limits(
        baseline,
        generated = baseline.generatedSqlBytes - 1L
      ),
      StageSqlBytes -> limits(
        baseline,
        stage = baseline.stageSqlBytes - 1L
      ),
      AllSqlBytes -> limits(
        baseline,
        allSql = baseline.allSqlBytes - 1L
      ),
      CanonicalJsonDocumentBytes -> limits(
        baseline,
        documents = baseline.canonicalJsonDocumentBytes - 1L
      )
    )

    cases.foreach { case (expectedResource, exactLimits) =>
      PostgreSqlEnumerationCampaignV2CumulativeBudget
        .account(artifact, projection, exactLimits) match {
        case Left(failure: LimitExceeded) =>
          assertEquals(failure.resource, expectedResource)
          assertEquals(
            failure.attemptedBytes,
            BigInt(expectedResource match {
              case SourceSqlBytes => baseline.sourceSqlBytes
              case GeneratedSqlBytes => baseline.generatedSqlBytes
              case StageSqlBytes => baseline.stageSqlBytes
              case AllSqlBytes => baseline.allSqlBytes
              case CanonicalJsonDocumentBytes =>
                baseline.canonicalJsonDocumentBytes
            })
          )
          assert(failure.acceptedUsage != null)
          assert(failure.offendingItem.byteLength > 0L)
        case other => fail(s"expected typed $expectedResource failure, got $other")
      }
    }
  }

  test("assembly inventory accounting is reusable and rejects duplicate IDs") {
    val artifact = produce(planLimit = 1)
    val projection = PostgreSqlEnumerationCampaignV2Json.project(
      artifact,
      authoring(artifact)
    )
    val base = namedProjectionDocuments(projection)
    val preSeal = PostgreSqlEnumerationCampaignV2Json
      .sourceSqlDocument("pre-seal")
    val assembly = base :+ ("pre-seal-assembly" -> preSeal)

    val evidence = PostgreSqlEnumerationCampaignV2CumulativeBudget
      .accountLogicalDocuments(artifact, assembly)
      .fold(failure => fail(failure.toString), identity)
    assertEquals(evidence.usage.canonicalJsonDocumentCount, assembly.size)
    assertEquals(
      evidence.usage.canonicalJsonDocumentBytes,
      assembly.iterator.map(_._2.byteLength.toLong).sum
    )

    PostgreSqlEnumerationCampaignV2CumulativeBudget
      .accountLogicalDocuments(artifact, base :+ base.head) match {
      case Left(failure: DocumentInventoryRejected) =>
        assertEquals(failure.reasonCode, "duplicate-logical-id")
        assertEquals(failure.acceptedUsage.canonicalJsonDocumentCount, 0)
        assert(failure.acceptedUsage.allSqlBytes > 0L)
      case other => fail(s"expected typed inventory rejection, got $other")
    }

    val malformed = Vector(
      null,
      Vector.empty,
      Vector(
        null.asInstanceOf[
          (String, PostgreSqlEnumerationCampaignV2JsonDocument)
        ]
      )
    )
    assertEquals(
      malformed.map(inventory =>
        PostgreSqlEnumerationCampaignV2CumulativeBudget
          .accountLogicalDocuments(artifact, inventory)
          .left
          .toOption
          .collect { case failure: DocumentInventoryRejected =>
            failure.reasonCode
          }
          .getOrElse(fail("malformed inventory was not rejected"))
      ),
      Vector("missing-inventory", "empty-inventory", "invalid-entry")
    )
  }

  test("byte counter detects Long overflow without performing overflowing addition") {
    assertEquals(
      PostgreSqlEnumerationCumulativeByteCounter.add(
        current = 0L,
        increment = Long.MaxValue,
        maximum = Long.MaxValue
      ),
      Right(Long.MaxValue)
    )
    PostgreSqlEnumerationCumulativeByteCounter.add(
      current = Long.MaxValue,
      increment = 1L,
      maximum = Long.MaxValue
    ) match {
      case Left(exceeded) =>
        assertEquals(exceeded.maximum, Long.MaxValue)
        assertEquals(exceeded.attempted, BigInt(Long.MaxValue) + 1)
      case other => fail(s"expected overflow-safe rejection, got $other")
    }
  }

  test("the exact composed package is charged before publication") {
    val artifact = produce(planLimit = 1)
    val evidence = PostgreSqlEnumerationCampaignV2CumulativeBudget
      .project(artifact, authoring(artifact))
      .fold(failure => fail(failure.toString), identity)
      .evidence
    val packageBytes = 123L
    val finalized = PostgreSqlEnumerationCampaignV2CumulativeBudget
      .finalizePackage(evidence, packageBytes)
      .fold(failure => fail(failure.toString), identity)

    assertEquals(finalized.preSeal, evidence)
    assertEquals(finalized.finalPackageBytes, packageBytes)
    assertEquals(
      finalized.totalCanonicalJsonDocumentCount,
      evidence.usage.canonicalJsonDocumentCount + 1
    )
    assertEquals(
      finalized.totalCanonicalJsonBytes,
      evidence.usage.canonicalJsonDocumentBytes + packageBytes
    )

    val tight = evidence.copy(
      limits = limits(
        evidence.usage,
        documents = evidence.usage.canonicalJsonDocumentBytes +
          packageBytes - 1L
      )
    )
    PostgreSqlEnumerationCampaignV2CumulativeBudget
      .finalizePackage(tight, packageBytes) match {
      case Left(failure: FinalPackageLimitExceeded) =>
        assertEquals(
          failure.maximumBytes,
          tight.limits.maxCanonicalJsonDocumentBytes
        )
        assertEquals(
          failure.attemptedBytes,
          BigInt(evidence.usage.canonicalJsonDocumentBytes) + packageBytes
        )
      case other => fail(s"expected final-package limit rejection, got $other")
    }
  }

  test("paired replay publication charges package, search input, and commit") {
    val artifact = produce(planLimit = 1)
    val evidence = PostgreSqlEnumerationCampaignV2CumulativeBudget
      .project(artifact, authoring(artifact))
      .fold(failure => fail(failure.toString), identity)
      .evidence
    val packageBytes = 123L
    val searchBytes = 456L
    val completeBytes = 789L
    val finalized = PostgreSqlEnumerationCampaignV2CumulativeBudget
      .finalizeSearchReplayBundle(
        evidence,
        packageBytes,
        searchBytes,
        completeBytes
      )
      .fold(failure => fail(failure.toString), identity)

    val added = packageBytes + searchBytes + completeBytes
    assertEquals(finalized.preSeal, evidence)
    assertEquals(finalized.finalPackageBytes, packageBytes)
    assertEquals(finalized.searchInputBytes, searchBytes)
    assertEquals(finalized.completeRecordBytes, completeBytes)
    assertEquals(
      finalized.totalCanonicalJsonDocumentCount,
      evidence.usage.canonicalJsonDocumentCount + 3
    )
    assertEquals(
      finalized.totalCanonicalJsonBytes,
      evidence.usage.canonicalJsonDocumentBytes + added
    )

    val tight = evidence.copy(
      limits = limits(
        evidence.usage,
        documents = evidence.usage.canonicalJsonDocumentBytes + added - 1L
      )
    )
    PostgreSqlEnumerationCampaignV2CumulativeBudget
      .finalizeSearchReplayBundle(
        tight,
        packageBytes,
        searchBytes,
        completeBytes
      ) match {
      case Left(failure: FinalPackageLimitExceeded) =>
        assertEquals(
          failure.attemptedBytes,
          BigInt(evidence.usage.canonicalJsonDocumentBytes) + BigInt(added)
        )
      case other => fail(s"expected final-bundle limit rejection, got $other")
    }
  }

  test("limit construction returns all non-positive resources as typed errors") {
    val invalid = PostgreSqlEnumerationCumulativeByteLimits.create(
      maxSourceSqlBytes = 0L,
      maxGeneratedSqlBytes = -1L,
      maxStageSqlBytes = 0L,
      maxAllSqlBytes = -1L,
      maxCanonicalJsonDocumentBytes = 0L
    )
    val errors = invalid.left.toOption.getOrElse(fail("invalid limits passed"))
    assertEquals(
      errors.map(_.resource),
      Vector(
        SourceSqlBytes,
        GeneratedSqlBytes,
        StageSqlBytes,
        AllSqlBytes,
        CanonicalJsonDocumentBytes
      )
    )
    assert(errors.forall(_.code.endsWith("invalid-limit")))
  }

  private def limits(
      usage: PostgreSqlEnumerationCumulativeByteUsage,
      source: Long = -1L,
      generated: Long = -1L,
      stage: Long = -1L,
      allSql: Long = -1L,
      documents: Long = -1L
  ): PostgreSqlEnumerationCumulativeByteLimits =
    PostgreSqlEnumerationCumulativeByteLimits
      .create(
        maxSourceSqlBytes = if (source >= 0L) source else usage.sourceSqlBytes,
        maxGeneratedSqlBytes =
          if (generated >= 0L) generated else usage.generatedSqlBytes,
        maxStageSqlBytes = if (stage >= 0L) stage else usage.stageSqlBytes,
        maxAllSqlBytes = if (allSql >= 0L) allSql else usage.allSqlBytes,
        maxCanonicalJsonDocumentBytes =
          if (documents >= 0L) documents
          else usage.canonicalJsonDocumentBytes
      )
      .fold(errors => fail(errors.mkString("; ")), identity)

  private def projectionDocuments(
      projection: PostgreSqlEnumerationCampaignV2Projection
  ): Vector[PostgreSqlEnumerationCampaignV2JsonDocument] =
    namedProjectionDocuments(projection).map(_._2)

  private def namedProjectionDocuments(
      projection: PostgreSqlEnumerationCampaignV2Projection
  ): Vector[(String, PostgreSqlEnumerationCampaignV2JsonDocument)] =
    Vector(
      "source-sql" -> projection.sourceSql,
      "semantic-search-core" -> projection.semanticSearchCore,
      "producer-search-contract" -> projection.producerSearchContract,
      "producer-search-transcript" -> projection.producerSearchTranscript
    ) ++ projection.canonicalDecompositions.zipWithIndex.map {
      case (document, index) =>
        f"canonical-decomposition/rewrite-${index + 1}%04d" -> document
    } ++ Vector("feature-registry" -> projection.featureRegistry) ++
      projection.featureValueBundles.map(bundle =>
        s"feature-values/${bundle.actionId}" -> bundle.document
      ) ++ projection.actions.flatMap(action =>
        Vector(
          s"action-sql/${action.actionId}" -> action.renderedSql,
          s"ordered-stages/${action.actionId}" -> action.orderedStages
        )
      )

  private def produce(planLimit: Int): PostgreSqlEnumerationBatchArtifact = {
    val fixture = CalcitePlannerFixture(CalciteTestSchemas.standard.tables)
    val planner = CalciteSqlPlanner
      .fromFunction(sql => Right(fixture.plan(sql).root))
      .fold(problem => fail(problem.message), identity)
    val rewriter = PostgreSqlQueryRewriter
      .create(planner)
      .fold(problem => fail(problem.message), identity)
    val request = PostgreSqlEnumerationBatchRequest
      .create(triangleSql, planLimit = planLimit)
      .fold(problems => fail(problems.map(_.code).mkString("; ")), identity)
    rewriter
      .produceEnumerationBatch(request)
      .artifact
      .getOrElse(fail("enumeration batch did not produce an artifact"))
  }
}

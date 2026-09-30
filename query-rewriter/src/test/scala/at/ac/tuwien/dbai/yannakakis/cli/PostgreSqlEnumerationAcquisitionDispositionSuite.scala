package at.ac.tuwien.dbai.yannakakis.cli

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.{
  CandidatePlanDecision,
  CandidateResourceLimits
}
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.BagFactorPlanningLimits
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.PostgreSqlRenderingLimits
import at.ac.tuwien.dbai.yannakakis.algorithm.soft.SoftResourceLimits
import at.ac.tuwien.dbai.yannakakis.api.{
  DiagnosticSeverity,
  RewriteDiagnostic,
  RewriteReasonCode
}
import at.ac.tuwien.dbai.yannakakis.api.postgresql.{
  PostgreSqlEnumerationBatchRequest,
  PostgreSqlEnumerationBatchResult,
  PostgreSqlEnumerationBatchStatus,
  PostgreSqlQueryRewriter,
  PostgreSqlRewritePhase
}
import at.ac.tuwien.dbai.yannakakis.calcite.{
  CalcitePlannerFixture,
  CalciteTestSchemas
}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.CalciteSqlPlanner

import PostgreSqlEnumerationAcquisitionDisposition._
import PostgreSqlEnumerationAcquisitionProducerContractViolation._
import PostgreSqlEnumerationAcquisitionResourceOrigin._
import PostgreSqlEnumerationAcquisitionTerminalClass._
import PostgreSqlEnumerationCampaignV2CanonicalJson._
import PostgreSqlEnumerationSearchReplayV2TestSupport._

final class PostgreSqlEnumerationAcquisitionDispositionSuite
    extends munit.FunSuite {

  test("found and exact fallback retain the existing eligible boundary") {
    val foundResult = produce(width = 2, k = 1)
    val fallbackResult = produce(width = 1, k = 3)

    val found = classify(foundResult)
    val fallback = classify(fallbackResult)

    found match {
      case Eligible(
            value: PostgreSqlEnumerationCampaignV2EligibleResult.Found
          ) =>
        assertEquals(value.artifact, foundResult.artifact.get)
      case other => fail(s"expected eligible found result, received $other")
    }
    fallback match {
      case Eligible(
            value: PostgreSqlEnumerationCampaignV2EligibleResult.NativeFallback
          ) =>
        assertEquals(value.result, fallbackResult)
        assertEquals(value.reason.code, "no-decomposition")
      case other => fail(s"expected eligible fallback result, received $other")
    }
  }

  test("invalid, unsupported, and other not-applicable results are distinct") {
    val invalidResult = produce(sql = "   ", width = 2, k = 1)
    val unsupportedResult = produce(sql = UnsupportedSql, width = 2, k = 1)
    val otherResult = unsupportedResult.copy(
      diagnostics = Vector(
        RewriteDiagnostic(
          RewriteReasonCode.RewriteNotBeneficial,
          "closed non-applicability fixture",
          DiagnosticSeverity.Warning,
          Map("reason" -> "fixture")
        )
      )
    )

    val invalid = terminal(invalidResult)
    val unsupported = terminal(unsupportedResult)
    val other = terminal(otherResult)

    assertEquals(invalid.terminalClass, InvalidQuery)
    assertEquals(unsupported.terminalClass, Unsupported)
    assertEquals(other.terminalClass, OtherNotApplicable)
    assertEquals(
      unsupported.diagnostics.map(_.code),
      unsupportedResult.diagnostics.map(_.code)
    )
    assert(
      Vector(invalid, unsupported, other).forall(_.resourceOrigins.isEmpty)
    )
  }

  test("all five exact producer resource origins classify independently") {
    val soft = produceWithLimits(
      softLimits = SoftResourceLimits
        .create(maxCovers = 1L)
        .fold(problems => fail(problems.mkString("; ")), identity)
    )
    val candidate = produceWithLimits(
      candidateLimits = CandidateResourceLimits
        .create(maxBlocks = 1L)
        .fold(problems => fail(problems.mkString("; ")), identity)
    )
    val bagFactor = produceWithLimits(
      bagFactorLimits = BagFactorPlanningLimits
        .create(maxOwnershipChecks = 1L)
        .fold(problems => fail(problems.mkString("; ")), identity)
    )
    val rendering = produceWithLimits(
      renderingLimits = PostgreSqlRenderingLimits
        .create(maxExpandedScans = 1L)
        .fold(problems => fail(problems.mkString("; ")), identity)
    )
    val transport = transportFailureFrom(bagFactor)

    val cases = Vector(
      soft -> SoftBagGeneration,
      candidate -> CandidateSearch,
      bagFactor -> BagFactorPlanning,
      rendering -> PostgreSqlRendering,
      transport -> SqlTransport
    )

    cases.foreach { case (result, expectedOrigin) =>
      assertEquals(result.status, PostgreSqlEnumerationBatchStatus.Failed)
      val disposition = terminal(result)
      assertEquals(
        disposition.terminalClass,
        ResourceLimited,
        expectedOrigin.id
      )
      assertEquals(
        disposition.resourceOrigins,
        Vector(expectedOrigin),
        expectedOrigin.id
      )
    }
  }

  test("mixed and lookalike resource diagnostics remain other-failed") {
    val candidate = produceWithLimits(
      candidateLimits = CandidateResourceLimits
        .create(maxBlocks = 1L)
        .fold(problems => fail(problems.mkString("; ")), identity)
    )
    val exact = candidate.diagnostics.head
    val lookalike = candidate.copy(
      diagnostics = Vector(
        exact.copy(context = exact.context + ("unexpected" -> "value"))
      )
    )
    val mixed = candidate.copy(
      diagnostics = candidate.diagnostics :+ RewriteDiagnostic(
        RewriteReasonCode.InternalError,
        "independent failure",
        DiagnosticSeverity.Error,
        Map("stage" -> "candidate-search")
      )
    )

    Vector(lookalike, mixed).foreach { result =>
      val disposition = terminal(result)
      assertEquals(disposition.terminalClass, OtherFailed)
      assertEquals(disposition.resourceOrigins, Vector.empty)
    }
  }

  test("retained resource tuples bind one canonical reported candidate") {
    val bagFactor = produceWithLimits(
      bagFactorLimits = BagFactorPlanningLimits
        .create(maxOwnershipChecks = 1L)
        .fold(problems => fail(problems.mkString("; ")), identity)
    )
    val exact = bagFactor.diagnostics.head
    val rendering = produceWithLimits(
      renderingLimits = PostgreSqlRenderingLimits
        .create(maxExpandedScans = 1L)
        .fold(problems => fail(problems.mkString("; ")), identity)
    )
    val found = bagFactor.pipelineEvidence.candidateDecision.collect {
      case value: CandidatePlanDecision.Found => value
    }.getOrElse(fail("bag-factor fixture did not retain candidates"))
    val unrelatedInput = produce(width = 1, k = 3).pipelineEvidence.candidateInput
      .getOrElse(fail("unrelated fixture did not retain candidate input"))
    val alternateArtifact = produce(width = 2, k = 2).artifact
      .getOrElse(fail("alternate fixture was not produced"))
    val alternatePlan = alternateArtifact.batch.search.plans
      .lift(1)
      .getOrElse(fail("alternate fixture retained fewer than two candidates"))
    val alternateBagFactorPlan =
      alternateArtifact.batch.candidates(1).bagFactorPlan
    val corruptedCertificatePlan = found.plans.head.copy(
      certificate = found.plans.head.certificate.copy(
        bagCount = found.plans.head.certificate.bagCount + 1
      )
    )
    val hostile = Vector(
      bagFactor.copy(diagnostics = Vector(exact, exact)),
      bagFactor.copy(
        diagnostics = Vector(
          exact.copy(context = exact.context.updated("candidateRank", "+1"))
        )
      ),
      bagFactor.copy(
        diagnostics = Vector(
          exact.copy(
            context = exact.context.updated(
              "candidateId",
              "sha256:" + ("0" * 64)
            )
          )
        )
      ),
      bagFactor.copy(
        pipelineEvidence = bagFactor.pipelineEvidence.copy(
          candidateDecision = Some(
            found.copy(requestedLimit = bagFactor.request.planLimit + 1)
          )
        )
      ),
      bagFactor.copy(
        pipelineEvidence = bagFactor.pipelineEvidence.copy(
          candidateDecision = Some(
            found.copy(plans = found.plans :+ found.plans.head)
          )
        )
      ),
      bagFactor.copy(
        pipelineEvidence = bagFactor.pipelineEvidence.copy(
          candidateDecision = Some(found.copy(plans = null))
        )
      ),
      bagFactor.copy(
        pipelineEvidence = bagFactor.pipelineEvidence.copy(
          candidateDecision = Some(
            found.copy(plans = Vector(corruptedCertificatePlan))
          )
        )
      ),
      bagFactor.copy(
        pipelineEvidence = bagFactor.pipelineEvidence.copy(
          candidateDecision = Some(
            found.copy(plans = found.plans :+ alternatePlan)
          )
        )
      ),
      bagFactor.copy(
        pipelineEvidence = bagFactor.pipelineEvidence.copy(
          candidateInput = Some(unrelatedInput)
        )
      ),
      rendering.copy(
        pipelineEvidence = rendering.pipelineEvidence.copy(
          bagFactorPlan = Some(alternateBagFactorPlan)
        )
      )
    )

    hostile.foreach { result =>
      val disposition = terminal(result)
      assertEquals(disposition.terminalClass, OtherFailed)
      assertEquals(disposition.resourceOrigins, Vector.empty)
    }
  }

  test("terminal status, phase, and invalid-query metadata are reachable") {
    val invalid = produce(sql = "   ", width = 2, k = 1)
    val fabricatedNotApplicable = invalid.copy(
      status = PostgreSqlEnumerationBatchStatus.NotApplicable,
      diagnostics = Vector(
        RewriteDiagnostic(
          RewriteReasonCode.UnsupportedQueryShape,
          "fabricated early non-applicability",
          DiagnosticSeverity.Warning
        )
      )
    )
    assertViolation(
      fabricatedNotApplicable,
      authoring(invalid),
      MalformedTerminalResult
    )

    val invalidException = invalid.copy(
      diagnostics = invalid.diagnostics.map(diagnostic =>
        diagnostic.copy(context = diagnostic.context + ("exceptionClass" -> ""))
      )
    )
    assertViolation(
      invalidException,
      authoring(invalid),
      MalformedTerminalResult
    )
  }

  test("terminal diagnostic projection preserves producer order") {
    val unsupported = produce(sql = UnsupportedSql, width = 2, k = 1)
    val ordered = unsupported.copy(
      diagnostics = Vector(
        RewriteDiagnostic(
          RewriteReasonCode.UnsupportedPredicate,
          "second alphabetically but first in the producer",
          DiagnosticSeverity.Warning
        ),
        RewriteDiagnostic(
          RewriteReasonCode.UnsupportedAggregate,
          "first alphabetically but second in the producer",
          DiagnosticSeverity.Warning
        )
      )
    )

    val disposition = terminal(ordered)
    assertEquals(
      disposition.diagnostics.map(_.code),
      ordered.diagnostics.map(_.code)
    )
    val receipt = PostgreSqlEnumerationAcquisitionTerminalReceiptJson
      .build(disposition)
    val receiptCodes = ujson
      .read(receipt.utf8)("diagnostics")
      .arr
      .map(_("code").str)
      .toVector
    assertEquals(receiptCodes, ordered.diagnostics.map(_.code.id))
  }

  test("authoring, produced, fallback, and terminal mismatches fail closed") {
    val found = produce(width = 2, k = 1)
    val fallback = produce(width = 1, k = 3)
    val invalid = produce(sql = "   ", width = 2, k = 1)

    assertViolation(
      found,
      authoring(
        found,
        producerEntrypoint =
          "at.ac.tuwien.dbai.yannakakis.cli.PostgreSqlEnumerationCampaignV2AcquisitionCli"
      ),
      AuthoringEntrypointMismatch
    )
    assertViolation(
      found,
      authoring(produce(width = 2, k = 2)),
      AuthoringOrSearchContractMismatch
    )

    val producedWithForbiddenReportState = found.copy(
      pipelineEvidence = found.pipelineEvidence.copy(
        bagFactorPlan = Some(
          found.artifact.get.batch.candidates.head.bagFactorPlan
        )
      )
    )
    assertViolation(
      producedWithForbiddenReportState,
      authoring(found),
      RejectedProduced
    )

    val malformedFallback = fallback.copy(
      diagnostics = fallback.diagnostics :+ fallback.diagnostics.head.copy(
        message = "duplicate fallback signal"
      )
    )
    assertViolation(
      malformedFallback,
      authoring(fallback),
      RejectedNativeFallback
    )

    val invalidWithWrongCode = invalid.copy(
      diagnostics = Vector(
        RewriteDiagnostic(
          RewriteReasonCode.InternalError,
          "not a parser or validator rejection",
          DiagnosticSeverity.Error,
          Map("stage" -> "parsing")
        )
      )
    )
    assertViolation(
      invalidWithWrongCode,
      authoring(invalid),
      MalformedTerminalResult
    )
  }

  test("terminal report prefixes reject unreachable retained evidence") {
    val invalid = produce(sql = "   ", width = 2, k = 1)
    val found = produce(width = 2, k = 1)
    val unreachable = invalid.copy(
      pipelineEvidence = invalid.pipelineEvidence.copy(
        analysis = found.pipelineEvidence.analysis,
        hypergraph = found.pipelineEvidence.hypergraph,
        softGeneration = found.pipelineEvidence.softGeneration,
        admissibleGeneration = found.pipelineEvidence.admissibleGeneration,
        candidateInput = found.pipelineEvidence.candidateInput,
        candidateDecision = found.pipelineEvidence.candidateDecision
      )
    )

    assertViolation(
      unreachable,
      authoring(invalid),
      MalformedTerminalResult
    )
  }

  test("terminal receipt is canonical, sealed, closed, and outcome free") {
    val result = produce(sql = UnsupportedSql, width = 2, k = 1)
    val disposition = terminal(result)
    val receipt = PostgreSqlEnumerationAcquisitionTerminalReceiptJson
      .build(disposition)
    val parsed = ujson.read(receipt.utf8)

    assertEquals(
      disposition.authoring.sha256,
      "e8c49040df9d17870ca0e9492eae87562ea04083d4fd3b3ad7894aadcdfd01fe"
    )
    assertEquals(
      disposition.producerSearchContract.sha256,
      "fffbb8344d1c178c6b6532fe45c573438de05c4a512c69f1193a567f9879c8aa"
    )
    assertEquals(
      receipt.sha256,
      "a3d50c986ecbcbc2fd0f71d167670d08c25efcf78a12672f7b91092d3389dc6e"
    )
    assertEquals(
      parsed("receipt_seal_sha256").str,
      "feb55e64c3978be01d479a6cc2c7e989c9b3a1fca5bb475479eb7436a4978901"
    )
    assertEquals(receipt.bytes.count(_ == '\n'.toByte), 1)
    assertEquals(canonicalDocument(parsed), receipt)
    assertEquals(
      parsed("schema_version").str,
      PostgreSqlEnumerationAcquisitionTerminalReceiptJson.SchemaVersion
    )
    assertEquals(parsed("terminal_class").str, "unsupported")
    assertEquals(parsed("claims_same_one_pass_result").bool, true)
    assertEquals(parsed("claims_cryptographic_same_result").bool, false)
    assertEquals(parsed("claims_complete_registered_denominator").bool, false)
    assertEquals(parsed("claims_external_durable_chronology").bool, false)
    assertEquals(
      parsed("claims_no_unrecorded_acquisition_attempts").bool,
      false
    )
    assertEquals(parsed("disposition").str, "terminal-unpackageable")
    assertEquals(parsed("execution_outcomes_in_scope").bool, false)
    assertEquals(parsed("production_policy").bool, false)
    assertEquals(parsed("reportable").bool, false)
    assertEquals(
      parsed("resource_origin_scheme").str,
      "exact-producer-phase-code-severity-context-shape-and-classifier-bindings-v1"
    )
    assertEquals(
      parsed("producer_search_contract_sha256").str,
      disposition.producerSearchContract.sha256
    )

    val body = canonicalDocument(
      ujson.Obj.from(
        parsed.obj.iterator.filterNot(_._1 == "receipt_seal_sha256")
      )
    )
    assertEquals(
      parsed("receipt_seal_sha256").str,
      PostgreSqlEnumerationAcquisitionTerminalReceiptJson.sealSha256(body)
    )

    val forbiddenKeys = Set(
      "message",
      "context",
      "detail",
      "sql",
      "generated_sql",
      "outcome",
      "timing",
      "path",
      "exception"
    )
    assertEquals(
      allObjectKeys(parsed).intersect(forbiddenKeys),
      Set.empty[String]
    )
    assert(!receipt.utf8.contains("triangle_r"))
  }

  test("diagnostic prose and resource detail cannot affect receipt bytes") {
    val candidate = produceWithLimits(
      candidateLimits = CandidateResourceLimits
        .create(maxBlocks = 1L)
        .fold(problems => fail(problems.mkString("; ")), identity)
    )
    val marker = "credential=/private/path prose must stay outside receipt"
    val changed = candidate.copy(
      diagnostics = candidate.diagnostics.map(diagnostic =>
        diagnostic.copy(
          message = marker,
          context = diagnostic.context.updated("detail", marker)
        )
      )
    )

    val first = PostgreSqlEnumerationAcquisitionTerminalReceiptJson
      .build(terminal(candidate))
    val second = PostgreSqlEnumerationAcquisitionTerminalReceiptJson
      .build(terminal(changed))

    assertEquals(second, first)
    assert(!second.utf8.contains(marker))
    assert(!second.utf8.contains("credential"))
    assert(!second.utf8.contains("private/path"))

    val emptyDetail = terminal(
      candidate.copy(
        diagnostics = candidate.diagnostics.map(diagnostic =>
          diagnostic.copy(context = diagnostic.context.updated("detail", ""))
        )
      )
    )
    assertEquals(emptyDetail.terminalClass, OtherFailed)
    assertEquals(emptyDetail.resourceOrigins, Vector.empty)

    val bagFactor = produceWithLimits(
      bagFactorLimits = BagFactorPlanningLimits
        .create(maxOwnershipChecks = 1L)
        .fold(problems => fail(problems.mkString("; ")), identity)
    )
    val exact = bagFactor.diagnostics.head
    val twoFullDiagnostics = bagFactor.copy(
      diagnostics = Vector(
        exact,
        exact.copy(
          message = marker,
          context = exact.context.updated("detail", marker)
        )
      )
    )
    val projected = terminal(twoFullDiagnostics)
    assertEquals(projected.terminalClass, ResourceLimited)
    assertEquals(projected.resourceOrigins, Vector(BagFactorPlanning))
    val projectedDiagnostics = ujson
      .read(
        PostgreSqlEnumerationAcquisitionTerminalReceiptJson
          .build(projected)
          .utf8
      )("diagnostics")
      .arr
      .toVector
    assertEquals(projectedDiagnostics.size, 2)
    assertEquals(projectedDiagnostics.distinct.size, 1)
  }

  private def classify(
      result: PostgreSqlEnumerationBatchResult,
      request: PostgreSqlEnumerationCampaignV2AuthoringRequest = null
  ): PostgreSqlEnumerationAcquisitionDisposition = {
    val exact = if (request == null) authoring(result) else request
    PostgreSqlEnumerationAcquisitionDispositionClassifier
      .classify(exact, result)
      .fold(problem => fail(problem.toString), identity)
  }

  private def terminal(
      result: PostgreSqlEnumerationBatchResult
  ): Unpackageable =
    classify(result) match {
      case value: Unpackageable => value
      case other                => fail(s"expected terminal result, received $other")
    }

  private def assertViolation(
      result: PostgreSqlEnumerationBatchResult,
      request: PostgreSqlEnumerationCampaignV2AuthoringRequest,
      expected: PostgreSqlEnumerationAcquisitionProducerContractViolation
  ): Unit = {
    val failure = PostgreSqlEnumerationAcquisitionDispositionClassifier
      .classify(request, result)
      .left
      .toOption
      .getOrElse(fail("hostile producer state was accepted"))
    assertEquals(failure.violation, expected)
  }

  private def produceWithLimits(
      softLimits: SoftResourceLimits = SoftResourceLimits.default,
      candidateLimits: CandidateResourceLimits = CandidateResourceLimits.default,
      bagFactorLimits: BagFactorPlanningLimits = BagFactorPlanningLimits.default,
      renderingLimits: PostgreSqlRenderingLimits = PostgreSqlRenderingLimits.default
  ): PostgreSqlEnumerationBatchResult = {
    val fixture = CalcitePlannerFixture(CalciteTestSchemas.standard.tables)
    val planner = CalciteSqlPlanner
      .fromFunction(value => Right(fixture.plan(value).root))
      .fold(problem => fail(problem.message), identity)
    val rewriter = PostgreSqlQueryRewriter
      .create(
        planner,
        softLimits = softLimits,
        candidateLimits = candidateLimits,
        bagFactorLimits = bagFactorLimits,
        renderingLimits = renderingLimits
      )
      .fold(problem => fail(problem.toString), identity)
    val request = PostgreSqlEnumerationBatchRequest
      .create(TriangleSql, widthBound = 2, planLimit = 1)
      .fold(
        problems => fail(problems.map(_.code).mkString("; ")),
        identity
      )
    rewriter.produceEnumerationBatch(request)
  }

  private def transportFailureFrom(
      candidateSearchCompleted: PostgreSqlEnumerationBatchResult
  ): PostgreSqlEnumerationBatchResult = {
    assertEquals(
      candidateSearchCompleted.completedPhase,
      PostgreSqlRewritePhase.CandidateSearchCompleted
    )
    val context = candidateSearchCompleted.diagnostics.head.context
    candidateSearchCompleted.copy(
      diagnostics = Vector(
        RewriteDiagnostic(
          RewriteReasonCode.RenderingFailed,
          "retained SQL exceeded the transport envelope",
          DiagnosticSeverity.Error,
          Map(
            "stage" -> "enumeration-batch-transport",
            "candidateRank" -> context("candidateRank"),
            "candidateId" -> context("candidateId"),
            "assetId" -> "rendered-sql",
            "reason" -> "utf8-byte-limit-exceeded"
          )
        )
      )
    )
  }

  private def canonicalDocument(
      value: ujson.Value
  ): PostgreSqlEnumerationCampaignV2JsonDocument =
    document(canonicalValue(value))

  private def canonicalValue(value: ujson.Value): Value =
    value match {
      case ujson.Null       => NullValue
      case ujson.Str(text)  => string(text)
      case ujson.True       => bool(true)
      case ujson.False      => bool(false)
      case numberValue: ujson.Num =>
        val exact = BigDecimal(numberValue.value).toBigIntExact.getOrElse(
          fail(s"receipt contains a non-integral number: $numberValue")
        )
        number(exact)
      case arrayValue: ujson.Arr =>
        array(arrayValue.value.iterator.map(canonicalValue).toVector)
      case objectValue: ujson.Obj =>
        obj(
          objectValue.value.iterator
            .map { case (key, item) => key -> canonicalValue(item) }
            .toVector
        )
      case other => fail(s"receipt contains an unsupported JSON value: $other")
    }

  private def allObjectKeys(value: ujson.Value): Set[String] =
    value match {
      case objectValue: ujson.Obj =>
        objectValue.value.iterator.foldLeft(Set.empty[String]) {
          case (keys, (key, child)) => keys + key ++ allObjectKeys(child)
        }
      case arrayValue: ujson.Arr =>
        arrayValue.value.iterator.foldLeft(Set.empty[String])((keys, child) =>
          keys ++ allObjectKeys(child)
        )
      case _ => Set.empty
    }
}

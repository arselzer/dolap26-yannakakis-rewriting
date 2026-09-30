package at.ac.tuwien.dbai.yannakakis.api.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.{
  CandidatePlanId,
  RankedCandidatePlan
}
import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode
import at.ac.tuwien.dbai.yannakakis.core.EdgeId

class PostgreSqlPhysicalPortfolioEnumerationContractSuite
    extends munit.FunSuite {
  import PostgreSqlFactorAwareContractTestSupport._
  import PostgreSqlPhysicalPortfolioEnumerationFailure._
  import PostgreSqlPhysicalPortfolioEnumerationPhase._
  import PostgreSqlPhysicalPortfolioResource._

  private lazy val prepared = q5
  private lazy val exactHundred = enumerate(prepared.found.plans)
  private lazy val twoPlans: Vector[RankedCandidatePlan] = {
    val sentinel = prepared.found.plans.find(hasAlternateCover).getOrElse {
      fail("Q5 exact-100 fixture has no alternate-cover candidate")
    }
    val covered = prepared.found.plans.find(_.candidateId != sentinel.candidateId).getOrElse {
      fail("Q5 exact-100 fixture has no second candidate")
    }
    Vector(sentinel.copy(rank = 1), covered.copy(rank = 2))
  }
  private lazy val twoCandidate = enumerate(twoPlans)

  test("default limits and every checked lower or upper boundary are frozen") {
    val defaults = PostgreSqlPhysicalPortfolioLimits.default
    assertEquals(defaults.maxStructuralCandidates, 100)
    assertEquals(defaults.maxRootsPerCandidate, 64)
    assertEquals(defaults.maxCoversPerBag, 64)
    assertEquals(defaults.maxCoverStatesPerCandidate, 256)
    assertEquals(defaults.maxVariantsPerCandidate, 256)
    assertEquals(defaults.maxRawVariantsPerSession, 256)
    assertEquals(defaults.maxUniqueWorkloadsPerSession, 256)
    assertEquals(PostgreSqlPhysicalPortfolioLimits.MinRawVariantsPerSession, 221)

    val lower = limits(
      maxRootsPerCandidate = 1,
      maxCoversPerBag = 1,
      maxCoverStatesPerCandidate = 2,
      maxVariantsPerCandidate = 7,
      maxRawVariantsPerSession = 221,
      maxUniqueWorkloadsPerSession = 221
    )
    assertEquals(lower.maxRootsPerCandidate, 1)
    assertEquals(lower.maxCoversPerBag, 1)
    assertEquals(lower.maxCoverStatesPerCandidate, 2)
    assertEquals(lower.maxVariantsPerCandidate, 7)
    assertEquals(lower.maxRawVariantsPerSession, 221)

    val invalid = Vector(
      PostgreSqlPhysicalPortfolioLimits.create(maxStructuralCandidates = 99),
      PostgreSqlPhysicalPortfolioLimits.create(maxStructuralCandidates = 101),
      PostgreSqlPhysicalPortfolioLimits.create(maxRootsPerCandidate = 0),
      PostgreSqlPhysicalPortfolioLimits.create(maxRootsPerCandidate = 65),
      PostgreSqlPhysicalPortfolioLimits.create(maxCoversPerBag = 0),
      PostgreSqlPhysicalPortfolioLimits.create(maxCoversPerBag = 65),
      PostgreSqlPhysicalPortfolioLimits.create(maxCoverStatesPerCandidate = 1),
      PostgreSqlPhysicalPortfolioLimits.create(maxCoverStatesPerCandidate = 257),
      PostgreSqlPhysicalPortfolioLimits.create(maxVariantsPerCandidate = 6),
      PostgreSqlPhysicalPortfolioLimits.create(maxVariantsPerCandidate = 257),
      PostgreSqlPhysicalPortfolioLimits.create(
        maxRawVariantsPerSession = 220,
        maxUniqueWorkloadsPerSession = 220
      ),
      PostgreSqlPhysicalPortfolioLimits.create(maxRawVariantsPerSession = 257),
      PostgreSqlPhysicalPortfolioLimits.create(maxUniqueWorkloadsPerSession = 255)
    )
    assert(invalid.forall(_.isLeft), clues(invalid))
    assertEquals(
      PostgreSqlPhysicalPortfolioLimits.validate(null).map(_.code),
      Vector("physical-portfolio.limit.invalid.limits")
    )
  }

  test("exact structural K=100 receives anchors, global axis sentinels, per-candidate alternatives, and Q16 before later waves") {
    val value = exactHundred
    val candidates = value.candidates
    val entries = value.entries
    val anchors = entries.take(100)

    assertEquals(prepared.found.plans.size, 100)
    assertEquals(candidates.size, 100)
    assertEquals(entries.size, 256)
    assertEquals(anchors.map(_.phase).distinct, Vector(Anchor))
    assertEquals(anchors.map(_.variant.structuralRank), (1 to 100).toVector)
    assert(anchors.forall(_.variant.localOrdinal == 0))
    assertEquals(value.crossProductReserveTarget, 16)
    assertEquals(entries.count(_.phase == CrossProductReserve), 16)

    val primaryPrefixCount = entries.count(entry =>
      Set(Anchor, AxisSentinel, CandidateCoverage)(entry.phase)
    )
    assert(primaryPrefixCount <= 205, clues(primaryPrefixCount))
    assert(primaryPrefixCount + value.crossProductReserveTarget <= 221)
    assertEquals(
      entries.map(_.enumerationOrdinal),
      entries.indices.toVector
    )
    assertEquals(
      entries.map(_.variant.physicalVariantId).distinct.size,
      entries.size
    )
    assert(
      entries.groupBy(_.variant.candidateId).values.forall(_.size >= 2),
      clues(
        "per-candidate-counts",
        entries.groupBy(_.variant.candidateId).view.mapValues(_.size).toMap.toString
      )
    )
    assert(!value.globalWaveSearchExhausted)
  }

  test("sentinels use the lowest applicable structural rank and candidate coverage uses the frozen rank rotation") {
    val value = exactHundred
    val evidenceById = value.candidates.map(item => item.candidateId -> item).toMap
    val sentinels = value.entries.filter(_.phase == AxisSentinel)
    val expectedAxes = PostgreSqlPhysicalPortfolioAxis.Values.filter(axis =>
      value.candidates.exists(_.applicableAxes.contains(axis))
    )
    assertEquals(sentinels.flatMap(_.lane), expectedAxes)

    sentinels.foreach { entry =>
      val axis = entry.lane.getOrElse(fail("sentinel omitted its axis"))
      val expectedRank = value.candidates.find(_.applicableAxes.contains(axis)).map(_.structuralRank)
      assertEquals(Some(entry.variant.structuralRank), expectedRank)
    }

    value.entries.filter(_.phase == CandidateCoverage).foreach { entry =>
      val evidence = evidenceById(entry.variant.candidateId)
      val offset = (evidence.structuralRank - 1) % evidence.applicableAxes.size
      assertEquals(entry.lane, Some(evidence.applicableAxes(offset)))
    }
  }

  test("a two-candidate prefix exercises all six phases, exact Q16, and local heap order") {
    val value = twoCandidate
    assertEquals(
      value.entries.map(_.phase).distinct,
      Vector(
        Anchor,
        AxisSentinel,
        CandidateCoverage,
        CrossProductReserve,
        RemainingPrimaryAxis,
        ResidualHeap
      )
    )
    assertEquals(value.crossProductReserveTarget, 16)
    assertEquals(value.entries.count(_.phase == CrossProductReserve), 16)

    Vector(CrossProductReserve, ResidualHeap).foreach { phase =>
      value.entries
        .filter(_.phase == phase)
        .groupBy(_.variant.candidateId)
        .values
        .foreach { entries =>
          val keys = entries.sortBy(_.variant.localOrdinal).map(_.heapKey)
          assert(
            keys.zip(keys.drop(1)).forall { case (left, right) =>
              compareHeapKeys(left, right) <= 0
            },
            clues(phase, keys)
          )
        }
    }
  }

  test("persisted candidate evidence and heap coordinates exactly bind the structural plans and selected configurations") {
    val value = twoCandidate
    val planById = twoPlans.map(plan => plan.candidateId -> plan).toMap

    value.candidates.foreach { evidence =>
      val plan = planById(evidence.candidateId)
      val bags = plan.decomposition.bags.sortBy(_.id)
      val coverCounts = bags.map { bag =>
        prepared.input.candidateBag(bag.vertices).getOrElse {
          fail(s"missing candidate bag ${bag.id.value}")
        }.physicalCovers.map(_.edgeIds).distinct.size
      }
      val exactCovers = coverCounts.foldLeft(BigInt(1))(_ * _)
      val expectedAxes = PostgreSqlPhysicalPortfolioAxis.Values.filter {
        case PostgreSqlPhysicalPortfolioAxis.AlternateRoot => bags.size > 1
        case PostgreSqlPhysicalPortfolioAxis.AlternateCover => exactCovers > 1
        case _ => true
      }
      assertEquals(evidence.exactRootCount, BigInt(bags.size))
      assertEquals(evidence.exactCoverAssignmentCount, exactCovers)
      assertEquals(
        evidence.exactLogicalVariantCount,
        BigInt(bags.size) * exactCovers *
          YannakakisReductionMode.Values.size *
          PostgreSqlCteMaterializationDirective.Values.size
      )
      assertEquals(evidence.applicableAxes, expectedAxes)
      assert(evidence.retainedCoverStateCount <= value.limits.maxCoverStatesPerCandidate)
      assert(evidence.retainedLocalVariantCount <= value.limits.maxVariantsPerCandidate)
      assert(evidence.retainedCrossProductStateCount <= evidence.retainedLocalVariantCount)
      assertEquals(evidence.coverStateSearchExhausted, BigInt(evidence.retainedCoverStateCount) == exactCovers)
    }

    value.entries.foreach { entry =>
      val plan = planById(entry.variant.candidateId)
      val bags = plan.decomposition.bags.sortBy(_.id)
      val roots = plan.decomposition.root +: bags.map(_.id).filterNot(_ == plan.decomposition.root)
      val selectedCoverIndices = bags.map { bag =>
        val admissible = prepared.input.candidateBag(bag.vertices).get.physicalCovers
          .map(_.edgeIds)
          .distinct
          .sortWith((left, right) => compareEdgeVectors(left, right) < 0)
        val ordered = bag.cover +: admissible.filterNot(_ == bag.cover)
        val selected = entry.variant.configuration.coverAssignments
          .find(_.bagId == bag.id)
          .getOrElse(fail(s"variant omitted bag ${bag.id.value}"))
          .edgeIds
        ordered.indexOf(selected)
      }
      assert(selectedCoverIndices.forall(_ >= 0), clues(entry.variant.physicalVariantId))
      assertEquals(entry.heapKey.rootOrdinal, roots.indexOf(entry.variant.configuration.executionRoot))
      assertEquals(entry.heapKey.coverIndexVector, selectedCoverIndices)
      assertEquals(entry.heapKey.modeOrdinal, YannakakisReductionMode.Values.indexOf(entry.variant.reductionMode))
      assertEquals(
        entry.heapKey.directiveOrdinal,
        PostgreSqlCteMaterializationDirective.Values.indexOf(entry.variant.materialization)
      )
      assertEquals(
        entry.heapKey.deviationMagnitude,
        BigInt(entry.heapKey.rootOrdinal) +
          entry.heapKey.coverIndexVector.foldLeft(BigInt(0))(_ + _) +
          BigInt(entry.heapKey.modeOrdinal) +
          BigInt(entry.heapKey.directiveOrdinal)
      )
      assertEquals(
        entry.variant.physicalVariantId,
        PostgreSqlPhysicalVariantId.fromV2(
          entry.variant.candidateId,
          entry.variant.configuration,
          entry.variant.reductionMode,
          entry.variant.materialization
        )
      )
    }
  }

  test("lower prefix envelopes stay fair and derive the cross-product quota from retained local states") {
    val lower = limits(
      maxCoverStatesPerCandidate = 2,
      maxVariantsPerCandidate = 7,
      maxRawVariantsPerSession = 221,
      maxUniqueWorkloadsPerSession = 221
    )
    val value = enumerate(prepared.found.plans, lower)

    assert(value.entries.size <= 221)
    assertEquals(value.entries.take(100).map(_.phase).distinct, Vector(Anchor))
    assert(value.entries.groupBy(_.variant.candidateId).values.forall(_.size >= 2))
    assertEquals(
      value.crossProductReserveTarget,
      math.min(
        PostgreSqlExplainFactorAwarePhysicalPortfolioV1.CrossProductReserve,
        value.candidates.map(_.retainedCrossProductStateCount).sum
      )
    )
    assertEquals(
      value.entries.count(_.phase == CrossProductReserve),
      value.crossProductReserveTarget
    )
  }

  test("enumeration rejects missing, malformed, oversized, and per-candidate over-limit inputs with typed failures") {
    assertEquals(
      PostgreSqlPhysicalPortfolioEnumerator.enumerate(null, Vector.empty).left.toOption.map(_.code),
      Some("physical-portfolio.input.missing")
    )
    assertEquals(
      PostgreSqlPhysicalPortfolioEnumerator.enumerate(prepared.input, null).left.toOption.map(_.code),
      Some("physical-portfolio.input.missing")
    )
    assertEquals(
      PostgreSqlPhysicalPortfolioEnumerator.enumerate(prepared.input, Vector.empty).left.toOption.map(_.code),
      Some("physical-portfolio.candidate-set.invalid")
    )

    val rankDrift = prepared.found.plans.take(2).updated(
      1,
      prepared.found.plans(1).copy(rank = 3)
    )
    assertEquals(
      PostgreSqlPhysicalPortfolioEnumerator.enumerate(prepared.input, rankDrift).left.toOption.map(_.code),
      Some("physical-portfolio.candidate-set.invalid")
    )
    val identityDrift = prepared.found.plans.take(1).map(
      _.copy(candidateId = CandidatePlanId.parse("sha256:" + "f" * 64).fold(problem => fail(problem.message), identity))
    )
    assertEquals(
      PostgreSqlPhysicalPortfolioEnumerator.enumerate(prepared.input, identityDrift).left.toOption.map(_.code),
      Some("physical-portfolio.candidate-set.invalid")
    )

    val hundredAndOne = prepared.found.plans :+
      triangle.found.plans.head.copy(rank = 101)
    val oversized = PostgreSqlPhysicalPortfolioEnumerator
      .enumerate(prepared.input, hundredAndOne)
      .left
      .toOption
      .getOrElse(fail("101 structural candidates unexpectedly enumerated"))
    assertEquals(oversized.code, "physical-portfolio.resource.exceeded.structural-candidates")
    assertEquals(
      oversized.asInstanceOf[ResourceLimitExceeded].resource,
      StructuralCandidates
    )

    val rootFailure = PostgreSqlPhysicalPortfolioEnumerator
      .enumerate(
        prepared.input,
        prepared.found.plans.take(1),
        limits(maxRootsPerCandidate = 1)
      )
      .left
      .toOption
      .getOrElse(fail("root cap unexpectedly accepted"))
    assertEquals(rootFailure.asInstanceOf[ResourceLimitExceeded].resource, RootsPerCandidate)

    val trianglePrepared = triangle
    val coverFailure = PostgreSqlPhysicalPortfolioEnumerator
      .enumerate(
        trianglePrepared.input,
        trianglePrepared.found.plans.take(1),
        limits(maxCoversPerBag = 1)
      )
      .left
      .toOption
      .getOrElse(fail("cover cap unexpectedly accepted"))
    assertEquals(coverFailure.asInstanceOf[ResourceLimitExceeded].resource, CoversPerBag)
  }

  test("portfolio enumeration is a pure deterministic replay") {
    assertEquals(enumerate(twoPlans), twoCandidate)
  }

  private def enumerate(
      plans: Vector[RankedCandidatePlan],
      limits: PostgreSqlPhysicalPortfolioLimits = PostgreSqlPhysicalPortfolioLimits.default
  ): PostgreSqlPhysicalPortfolioEnumeration =
    PostgreSqlPhysicalPortfolioEnumerator
      .enumerate(prepared.input, plans, limits)
      .fold(problem => fail(problem.message), identity)

  private def hasAlternateCover(plan: RankedCandidatePlan): Boolean =
    plan.decomposition.bags.exists { bag =>
      prepared.input.candidateBag(bag.vertices).exists(
        _.physicalCovers.map(_.edgeIds).distinct.size > 1
      )
    }

  private def limits(
      maxStructuralCandidates: Int = 100,
      maxRootsPerCandidate: Int = 64,
      maxCoversPerBag: Int = 64,
      maxCoverStatesPerCandidate: Int = 256,
      maxVariantsPerCandidate: Int = 256,
      maxRawVariantsPerSession: Int = 256,
      maxUniqueWorkloadsPerSession: Int = 256
  ): PostgreSqlPhysicalPortfolioLimits =
    PostgreSqlPhysicalPortfolioLimits
      .create(
        maxStructuralCandidates,
        maxRootsPerCandidate,
        maxCoversPerBag,
        maxCoverStatesPerCandidate,
        maxVariantsPerCandidate,
        maxRawVariantsPerSession,
        maxUniqueWorkloadsPerSession
      )
      .fold(errors => fail(errors.map(_.message).mkString("; ")), identity)

  private def compareHeapKeys(
      left: PostgreSqlPhysicalVariantHeapKey,
      right: PostgreSqlPhysicalVariantHeapKey
  ): Int = {
    val magnitude = left.deviationMagnitude.compare(right.deviationMagnitude)
    if (magnitude != 0) magnitude
    else {
      val root = Integer.compare(left.rootOrdinal, right.rootOrdinal)
      if (root != 0) root
      else {
        val cover = compareIntVectors(left.coverIndexVector, right.coverIndexVector)
        if (cover != 0) cover
        else {
          val mode = Integer.compare(left.modeOrdinal, right.modeOrdinal)
          if (mode != 0) mode
          else {
            val directive = Integer.compare(left.directiveOrdinal, right.directiveOrdinal)
            if (directive != 0) directive
            else left.physicalVariantId.value.compareTo(right.physicalVariantId.value)
          }
        }
      }
    }
  }

  private def compareIntVectors(left: Vector[Int], right: Vector[Int]): Int = {
    left.zip(right).collectFirst {
      case (l, r) if l != r => Integer.compare(l, r)
    }.getOrElse(Integer.compare(left.size, right.size))
  }

  private def compareEdgeVectors(left: Vector[EdgeId], right: Vector[EdgeId]): Int = {
    left.zip(right).collectFirst {
      case (l, r) if l != r => EdgeId.ordering.compare(l, r)
    }.getOrElse(Integer.compare(left.size, right.size))
  }
}

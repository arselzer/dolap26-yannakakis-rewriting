package at.ac.tuwien.dbai.yannakakis.api.postgresql

class PostgreSqlStaticShapeScoreSuite extends munit.FunSuite {
  test("static-shape ordering is the frozen six-field lexicographic order") {
    assertEquals(
      PostgreSqlStaticShapeScore.OrderedFields,
      Vector(
        "expanded_scan_count",
        "materialized_distinct_support_count",
        "cte_count",
        "rendered_term_count",
        "sql_character_count",
        "structural_rank"
      )
    )

    val maximum = Long.MaxValue
    val witnesses = Vector(
      "expanded scans" -> (
        score(0L, maximum, maximum, maximum, maximum, Int.MaxValue),
        score(1L, 0L, 0L, 0L, 0L, 1)
      ),
      "materialized support" -> (
        score(4L, 0L, maximum, maximum, maximum, Int.MaxValue),
        score(4L, 1L, 0L, 0L, 0L, 1)
      ),
      "CTEs" -> (
        score(4L, 2L, 0L, maximum, maximum, Int.MaxValue),
        score(4L, 2L, 1L, 0L, 0L, 1)
      ),
      "rendered terms" -> (
        score(4L, 2L, 3L, 0L, maximum, Int.MaxValue),
        score(4L, 2L, 3L, 1L, 0L, 1)
      ),
      "SQL characters" -> (
        score(4L, 2L, 3L, 5L, 0L, Int.MaxValue),
        score(4L, 2L, 3L, 5L, 1L, 1)
      ),
      "structural rank" -> (
        score(4L, 2L, 3L, 5L, 8L, 1),
        score(4L, 2L, 3L, 5L, 8L, 2)
      )
    )

    witnesses.foreach { case (field, (better, worse)) =>
      assert(
        PostgreSqlStaticShapeScore.ordering.lt(better, worse),
        clues(field, better, worse)
      )
      assert(
        PostgreSqlStaticShapeScore.ordering.gt(worse, better),
        clues(field, better, worse)
      )
    }
    val equal = score(4L, 2L, 3L, 5L, 8L, 9)
    assertEquals(PostgreSqlStaticShapeScore.ordering.compare(equal, equal), 0)
  }

  test("static-shape scores reject every invalid dimension") {
    val invalid = Vector[() => PostgreSqlStaticShapeScore](
      () => score(-1L, 0L, 0L, 0L, 0L, 1),
      () => score(0L, -1L, 0L, 0L, 0L, 1),
      () => score(0L, 0L, -1L, 0L, 0L, 1),
      () => score(0L, 0L, 0L, -1L, 0L, 1),
      () => score(0L, 0L, 0L, 0L, -1L, 1),
      () => score(0L, 0L, 0L, 0L, 0L, 0)
    )

    invalid.foreach(build => intercept[IllegalArgumentException](build()))
    score(Long.MaxValue, Long.MaxValue, Long.MaxValue, Long.MaxValue, Long.MaxValue, Int.MaxValue)
  }

  test("selection limit validation is complete and declaration-ordered") {
    val exact = PostgreSqlCandidateSelectionLimits
      .create(
        maxEvaluatedCandidates = 1L,
        maxCumulativeExpandedScans = 1L,
        maxCumulativeCtes = 1L,
        maxCumulativeRenderedTerms = 1L,
        maxCumulativeSqlCharacters = 1L
      )
      .fold(errors => fail(errors.mkString("; ")), identity)
    assertEquals(exact.maxEvaluatedCandidates, 1L)
    assertEquals(exact.maxCumulativeExpandedScans, 1L)
    assertEquals(exact.maxCumulativeCtes, 1L)
    assertEquals(exact.maxCumulativeRenderedTerms, 1L)
    assertEquals(exact.maxCumulativeSqlCharacters, 1L)

    val errors = PostgreSqlCandidateSelectionLimits
      .create(
        maxEvaluatedCandidates = 0L,
        maxCumulativeExpandedScans = -1L,
        maxCumulativeCtes = 0L,
        maxCumulativeRenderedTerms = -1L,
        maxCumulativeSqlCharacters = 0L
      )
      .left
      .toOption
      .getOrElse(fail("invalid limits were accepted"))
    assertEquals(
      errors.map(_.resource.code),
      Vector(
        "evaluated-candidates",
        "cumulative-expanded-scans",
        "cumulative-ctes",
        "cumulative-rendered-terms",
        "cumulative-sql-characters"
      )
    )
  }

  private def score(
      scans: Long,
      support: Long,
      ctes: Long,
      terms: Long,
      characters: Long,
      rank: Int
  ): PostgreSqlStaticShapeScore =
    PostgreSqlStaticShapeScore(
      expandedScanCount = scans,
      materializedDistinctSupportCount = support,
      cteCount = ctes,
      renderedTermCount = terms,
      sqlCharacterCount = characters,
      structuralRank = rank
    )
}

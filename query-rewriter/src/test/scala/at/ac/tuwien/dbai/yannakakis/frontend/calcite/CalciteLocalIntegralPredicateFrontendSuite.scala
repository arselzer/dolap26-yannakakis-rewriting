package at.ac.tuwien.dbai.yannakakis.frontend.calcite

import at.ac.tuwien.dbai.yannakakis.api.RewriteReasonCode
import org.apache.calcite.rel.`type`.{RelDataType, RelDataTypeFactory}
import org.apache.calcite.schema.impl.AbstractTable
import org.apache.calcite.sql.`type`.SqlTypeName
import org.apache.calcite.tools.Frameworks

class CalciteLocalIntegralPredicateFrontendSuite extends munit.FunSuite {
  private val schema = {
    val root = Frameworks.createRootSchema(true)
    root.add(
      "role_edge",
      new AbstractTable {
        override def getRowType(factory: RelDataTypeFactory): RelDataType = {
          val nullableSmallint = factory.createTypeWithNullability(
            factory.createSqlType(SqlTypeName.SMALLINT),
            true
          )
          factory
            .builder()
            .add("left_key", SqlTypeName.INTEGER)
            .add("right_key", SqlTypeName.INTEGER)
            .add("edge_role", nullableSmallint)
            .add("big_role", SqlTypeName.BIGINT)
            .add("label", SqlTypeName.VARCHAR, 32)
            .add("approx_role", SqlTypeName.REAL)
            .build()
        }
      }
    )
    root
  }

  private val planner = CalciteSqlPlanner
    .forSchema(schema, CalciteSqlPlanner.PostgreSqlParserConfig)
    .fold(problem => fail(problem.toString), identity)

  private val baseJoin =
    "FROM role_edge AS e0 JOIN role_edge AS e1 " +
      "ON e0.right_key = e1.left_key"

  private def planned(sql: String): CalciteSqlPlannedQuery =
    CalciteSqlPlanner
      .planWithSourceEvidence(planner, sql)
      .fold(problem => fail(problem.toString), identity)

  private def certified(sql: String): CalciteSqlPlannedQuery = {
    val result = planned(sql)
    result.sourceEvidence match {
      case _: CalciteSqlSourceEvidence.CertifiedNonAggregateLocalIntegralPredicates =>
        result
      case other =>
        fail(s"expected certified local integral predicates, found $other")
    }
  }

  test("direct SMALLINT literal predicates retain value, type, order, and occurrence") {
    val result = certified(
      "SELECT e0.left_key, e1.right_key " +
        "FROM role_edge AS e0 JOIN role_edge AS e1 " +
        "ON e0.right_key = e1.left_key AND e1.edge_role = 1 " +
        "WHERE e0.edge_role = 0"
    )
    val certificate = result.sourceEvidence match {
      case CalciteSqlSourceEvidence.CertifiedNonAggregateLocalIntegralPredicates(
            value
          ) => value
      case other => fail(s"unexpected source evidence $other")
    }

    assertEquals(certificate.totalPredicateCount, 3)
    assertEquals(certificate.occurrences.map(_.ordinal), Vector(0, 1))
    assertEquals(certificate.occurrences.map(_.alias), Vector("e0", "e1"))
    assertEquals(
      certificate.predicates.map(binding =>
        (
          binding.predicateOrdinal,
          binding.occurrenceOrdinal,
          binding.alias,
          binding.columnName,
          binding.literalValue,
          binding.literalOnLeft
        )
      ),
      Vector(
        (1, 1, "e1", "edge_role", 1L, false),
        (2, 0, "e0", "edge_role", 0L, false)
      )
    )

    val report = CalciteQueryAnalyzer.analyze(result)
    assert(report.isSupported, clues(report.diagnostics))
    assertEquals(
      report.query.sourceProof.localIntegralPredicateCertificate,
      Some(certificate)
    )
    assertEquals(
      report.query.predicates.map(_.classification.id),
      Vector("inner-equality", "local-filter", "local-filter")
    )
    val localExpression = report.query.predicates(1).expression
      .asInstanceOf[ScalarExpression.Call]
    localExpression.operands.head match {
      case ScalarExpression.ExactIntegralWideningCast(
            column: ScalarExpression.Column,
            "SMALLINT",
            "INTEGER",
            _,
            "INTEGER",
            true
          ) => assertEquals(column.ref.relationOccurrenceId.value, 1)
      case other => fail(s"expected certified SMALLINT widening, found $other")
    }
    localExpression.operands(1) match {
      case ScalarExpression.ExactIntegralLiteral(
            1L,
            "INTEGER",
            _,
            "INTEGER NOT NULL"
          ) => ()
      case other => fail(s"expected exact INTEGER literal, found $other")
    }
  }

  test("symmetric literal equality is admitted only with explicit orientation evidence") {
    val result = certified(
      s"SELECT e0.left_key, e1.right_key $baseJoin " +
        "AND 1 = e1.edge_role WHERE 0 = e0.edge_role"
    )
    val certificate = result.sourceEvidence
      .asInstanceOf[
        CalciteSqlSourceEvidence.CertifiedNonAggregateLocalIntegralPredicates
      ]
      .certificate
    assert(certificate.predicates.forall(_.literalOnLeft))

    val report = CalciteQueryAnalyzer.analyze(result)
    assert(report.isSupported, clues(report.diagnostics))
    assertEquals(
      report.query.predicates.drop(1).map(_.classification.id),
      Vector("local-filter", "local-filter")
    )
  }

  test("SMALLINT INTEGER and BIGINT operands retain exact coercion and nullability") {
    val cases = Vector(
      (
        "e1.edge_role = 1",
        "SMALLINT",
        "INTEGER",
        "INTEGER",
        "INTEGER",
        true,
        "BOOLEAN"
      ),
      (
        "e1.left_key = 1",
        "INTEGER",
        "INTEGER",
        "INTEGER",
        "INTEGER",
        false,
        "BOOLEAN NOT NULL"
      ),
      (
        "e1.big_role = 1",
        "BIGINT",
        "BIGINT",
        "BIGINT",
        "BIGINT",
        false,
        "BOOLEAN NOT NULL"
      )
    )

    cases.foreach {
      case (
            predicate,
            columnSource,
            columnTarget,
            literalSource,
            literalTarget,
            expectedNullable,
            expectedDataType
          ) =>
        val report = CalciteQueryAnalyzer.analyze(
          certified(
            s"SELECT e0.left_key, e1.right_key $baseJoin AND $predicate"
          )
        )
        assert(report.isSupported, clues(predicate, report.diagnostics))
        val equality = report.query.predicates.last.expression
          .asInstanceOf[ScalarExpression.Call]

        def operandTypes(
            expression: ScalarExpression
        ): (String, String) = expression match {
          case column: ScalarExpression.Column =>
            val source = report.query.relations
              .flatMap(_.columns)
              .find(_.ref == column.ref)
              .map(_.sqlTypeName)
              .getOrElse(fail(s"missing metadata for ${column.ref}"))
            source -> source
          case literal: ScalarExpression.ExactIntegralLiteral =>
            literal.sqlTypeName -> literal.sqlTypeName
          case ScalarExpression.ExactIntegralWideningCast(
                operand,
                source,
                target,
                _,
                _,
                _
              ) =>
            assert(
              operand.isInstanceOf[ScalarExpression.Column] ||
                operand.isInstanceOf[ScalarExpression.ExactIntegralLiteral]
            )
            source -> target
          case other => fail(s"unexpected integral operand $other")
        }

        assertEquals(operandTypes(equality.operands.head), columnSource -> columnTarget)
        assertEquals(operandTypes(equality.operands(1)), literalSource -> literalTarget)
        assertEquals(equality.nullable, expectedNullable)
        assertEquals(equality.dataType, expectedDataType)
    }
  }

  test("relational literal shape without raw source evidence fails closed") {
    val result = certified(
      s"SELECT e0.left_key, e1.right_key $baseJoin " +
        "AND e1.edge_role = 1"
    )
    val report = CalciteQueryAnalyzer.analyze(result.root)

    assert(!report.isSupported)
    assertEquals(report.query.sourceProof, QuerySourceProof.Uncertified)
    assert(
      report.diagnostics.exists(diagnostic =>
        diagnostic.code == RewriteReasonCode.UnsupportedPredicate &&
          diagnostic.context.get("sourceCertificate").contains("missing")
      ),
      clues(report.diagnostics)
    )

    val customPlanner = CalciteSqlPlanner
      .fromFunction(_ => Right(result.root))
      .fold(problem => fail(problem.toString), identity)
    val custom = CalciteSqlPlanner
      .planWithSourceEvidence(
        customPlanner,
        s"SELECT e0.left_key, e1.right_key $baseJoin " +
          "AND e1.edge_role = 1"
      )
      .fold(problem => fail(problem.toString), identity)
    assertEquals(custom.sourceEvidence, CalciteSqlSourceEvidence.Uncertified)
    val customReport = CalciteQueryAnalyzer.analyze(custom)
    assert(!customReport.isSupported)
    assert(
      customReport.diagnostics.exists(_.code == RewriteReasonCode.UnsupportedPredicate),
      clues(customReport.diagnostics)
    )
  }

  test("explicit source CAST stays rejected even when Calcite folds it away") {
    val result = planned(
      s"SELECT e0.left_key, e1.right_key $baseJoin " +
        "AND e1.edge_role = CAST(1 AS SMALLINT)"
    )
    assertEquals(result.sourceEvidence, CalciteSqlSourceEvidence.NonAggregate)

    val report = CalciteQueryAnalyzer.analyze(result)
    assert(!report.isSupported)
    assert(
      report.query.predicates.last.expression.predicateLosslesslyRetained,
      clues(report.query.predicates.last.expression)
    )
    assert(
      report.diagnostics.exists(diagnostic =>
        diagnostic.code == RewriteReasonCode.UnsupportedPredicate &&
          diagnostic.context.get("sourceCertificate").contains("missing")
      ),
      clues(report.diagnostics)
    )
  }

  test("certificate count order value occurrence and column tampering fail closed") {
    val result = certified(
      "SELECT e0.left_key, e1.right_key " +
        "FROM role_edge AS e0 JOIN role_edge AS e1 " +
        "ON e0.right_key = e1.left_key AND e1.edge_role = 1 " +
        "WHERE e0.edge_role = 0"
    )
    val certificate = result.sourceEvidence
      .asInstanceOf[
        CalciteSqlSourceEvidence.CertifiedNonAggregateLocalIntegralPredicates
      ]
      .certificate

    val mutations = Vector(
      certificate.copy(totalPredicateCount = certificate.totalPredicateCount + 1),
      certificate.copy(predicates = certificate.predicates.reverse),
      certificate.copy(
        predicates = certificate.predicates.updated(
          0,
          certificate.predicates.head.copy(literalValue = 2L)
        )
      ),
      certificate.copy(
        predicates = certificate.predicates.updated(
          0,
          certificate.predicates.head.copy(
            occurrenceOrdinal = 0,
            alias = "e0"
          )
        )
      ),
      certificate.copy(
        predicates = certificate.predicates.updated(
          0,
          certificate.predicates.head.copy(columnName = "big_role")
        )
      ),
      certificate.copy(
        occurrences = certificate.occurrences.updated(
          0,
          certificate.occurrences.head.copy(qualifiedName = Vector("other"))
        )
      )
    )

    mutations.foreach { mutation =>
      val tampered = result.copy(
        sourceEvidence = CalciteSqlSourceEvidence
          .CertifiedNonAggregateLocalIntegralPredicates(mutation)
      )
      val report = CalciteQueryAnalyzer.analyze(tampered)
      assert(!report.isSupported, clues(mutation, report.diagnostics))
      assertEquals(report.query.sourceProof, QuerySourceProof.Uncertified)
      assert(
        report.diagnostics.exists(_.code == RewriteReasonCode.UnsupportedPredicate),
        clues(mutation, report.diagnostics)
      )
    }

    intercept[IllegalArgumentException] {
      certificate.predicates.head.copy(
        literalPrecision = certificate.predicates.head.literalPrecision + 1
      )
    }
  }

  test("casts, NULLs, non-integral values, expressions, and non-equality fail closed") {
    val rejected = Vector(
      s"SELECT e0.left_key $baseJoin AND e1.edge_role = CAST(1 AS SMALLINT)",
      s"SELECT e0.left_key $baseJoin AND CAST(e1.edge_role AS INTEGER) = 1",
      s"SELECT e0.left_key $baseJoin AND e1.edge_role = NULL",
      s"SELECT e0.left_key $baseJoin AND e1.edge_role = -1",
      s"SELECT e0.left_key $baseJoin AND e1.edge_role = 1.0",
      s"SELECT e0.left_key $baseJoin AND e1.big_role = 9223372036854775808",
      s"SELECT e0.left_key $baseJoin AND e1.label = '1'",
      s"SELECT e0.left_key $baseJoin AND e1.approx_role = 1",
      s"SELECT e0.left_key $baseJoin AND e1.edge_role + 0 = 1",
      s"SELECT e0.left_key $baseJoin AND e1.edge_role > 1",
      s"SELECT e0.left_key $baseJoin AND e0.edge_role + e1.edge_role = 1"
    )

    rejected.foreach(assertNeverSupported)
  }

  test("valid signed and non-canonical integral spellings are unsupported, not invalid") {
    Vector("-1", "01", "00").foreach { literal =>
      val sql = s"SELECT e0.left_key $baseJoin AND e1.edge_role = $literal"
      val result = CalciteSqlPlanner
        .planWithSourceEvidence(planner, sql)
        .fold(
          problem => fail(s"valid SQL was misclassified at ${problem.stage.id}: $sql"),
          identity
        )
      val report = CalciteQueryAnalyzer.analyze(result)

      assert(!report.isSupported, clues(sql, report.diagnostics))
      assertEquals(report.query.sourceProof, QuerySourceProof.Uncertified)
      assert(
        report.diagnostics.exists(_.code == RewriteReasonCode.UnsupportedPredicate),
        clues(sql, report.diagnostics)
      )
    }
  }

  test("typed integral equality is not admitted as a projection expression") {
    val report = CalciteQueryAnalyzer.analyze(
      planned(
        s"SELECT (e0.edge_role = 1) AS matched $baseJoin"
      )
    )

    assert(!report.isSupported)
    assert(
      report.diagnostics.exists(_.code == RewriteReasonCode.UnsupportedQueryShape),
      clues(report.diagnostics)
    )
  }

  test("aggregate analysis never receives the non-aggregate literal source proof") {
    val result = planned(
      "SELECT COUNT(*) " + baseJoin + " WHERE e0.edge_role = 0"
    )
    assert(
      result.sourceEvidence
        .isInstanceOf[CalciteSqlSourceEvidence.CertifiedGlobalCountStar],
      clues(result.sourceEvidence)
    )

    val report = CalciteQueryAnalyzer.analyze(result)
    assert(!report.isSupported)
    assertEquals(report.query.sourceProof, QuerySourceProof.Uncertified)
    assert(
      report.diagnostics.exists(_.code == RewriteReasonCode.UnsupportedPredicate),
      clues(report.diagnostics)
    )
  }

  private def assertNeverSupported(sql: String): Unit =
    CalciteSqlPlanner.planWithSourceEvidence(planner, sql) match {
      case Left(_) => ()
      case Right(result) =>
        val report = CalciteQueryAnalyzer.analyze(result)
        assert(!report.isSupported, clues(sql, report.diagnostics))
    }
}

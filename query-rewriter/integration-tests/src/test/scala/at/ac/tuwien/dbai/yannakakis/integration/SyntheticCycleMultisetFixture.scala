package at.ac.tuwien.dbai.yannakakis.integration

import java.sql.{Connection, PreparedStatement, Types}

import scala.util.Using

/**
  * Deterministic correctness-scale cycle data matching the documented corpus
  * formulas. Sparse data has two planted cycles and dangling decoys; dense
  * data has a complete two-key block at every cycle vertex.
  */
private[integration] object SyntheticCycleMultisetFixture {
  sealed trait Shape extends Product with Serializable {
    def id: String
  }
  case object Sparse extends Shape { override val id: String = "sparse" }
  case object Dense extends Shape { override val id: String = "dense" }

  final case class Topology(
      id: String,
      vertexCount: Int,
      edges: Vector[(Int, Int)]
  ) {
    def edgeCount: Int = edges.size
  }

  final case class Dataset(
      id: String,
      shape: Shape,
      topology: Topology,
      expectedWitnessMultiplicity: Long,
      expectedDistinctWitnesses: Int,
      expectedRowsPerRelation: Long
  )

  private final case class Row(
      rowId: Long,
      leftKey: Option[Long],
      rightKey: Option[Long],
      payload: Option[String]
  )

  val C4: Topology = Topology(
    "c4",
    vertexCount = 4,
    edges = Vector(0 -> 1, 1 -> 2, 2 -> 3, 3 -> 0)
  )
  val C6: Topology = Topology(
    "c6",
    vertexCount = 6,
    edges = Vector(0 -> 1, 1 -> 2, 2 -> 3, 3 -> 4, 4 -> 5, 5 -> 0)
  )
  val BowTie: Topology = Topology(
    "bow_tie",
    vertexCount = 5,
    edges = Vector(0 -> 1, 1 -> 2, 2 -> 0, 0 -> 3, 3 -> 4, 4 -> 0)
  )

  val SparseC4: Dataset =
    Dataset(
      "sparse_c4",
      Sparse,
      C4,
      expectedWitnessMultiplicity = 32L,
      expectedDistinctWitnesses = 2,
      expectedRowsPerRelation = 8L
    )
  val DenseC4: Dataset =
    Dataset(
      "dense_c4",
      Dense,
      C4,
      expectedWitnessMultiplicity = 256L,
      expectedDistinctWitnesses = 16,
      expectedRowsPerRelation = 12L
    )
  val SparseC6: Dataset =
    Dataset(
      "sparse_c6",
      Sparse,
      C6,
      expectedWitnessMultiplicity = 128L,
      expectedDistinctWitnesses = 2,
      expectedRowsPerRelation = 8L
    )
  val DenseC6: Dataset =
    Dataset(
      "dense_c6",
      Dense,
      C6,
      expectedWitnessMultiplicity = 4096L,
      expectedDistinctWitnesses = 64,
      expectedRowsPerRelation = 12L
    )
  val SparseBowTie: Dataset =
    Dataset(
      "sparse_bow_tie",
      Sparse,
      BowTie,
      expectedWitnessMultiplicity = 128L,
      expectedDistinctWitnesses = 2,
      expectedRowsPerRelation = 8L
    )
  val DenseBowTie: Dataset =
    Dataset(
      "dense_bow_tie",
      Dense,
      BowTie,
      expectedWitnessMultiplicity = 2048L,
      expectedDistinctWitnesses = 32,
      expectedRowsPerRelation = 12L
    )

  val All: Vector[Dataset] =
    Vector(
      SparseC4,
      DenseC4,
      SparseC6,
      DenseC6,
      SparseBowTie,
      DenseBowTie
    )

  def tableName(dataset: Dataset, edgeIndex: Int): String =
    s"synthetic_${dataset.id}_e$edgeIndex"

  def enumerationCore(dataset: Dataset): String = {
    val projections = (0 until dataset.topology.edgeCount).flatMap { edgeIndex =>
      Vector(
        s"  \"e$edgeIndex\".\"row_id\" AS \"e${edgeIndex}_row_id\"",
        s"  \"e$edgeIndex\".\"left_key\" AS \"e${edgeIndex}_left_key\"",
        s"  \"e$edgeIndex\".\"right_key\" AS \"e${edgeIndex}_right_key\"",
        s"  \"e$edgeIndex\".\"payload\" AS \"e${edgeIndex}_payload\""
      )
    }.mkString(",\n")

    val representatives = scala.collection.mutable.Map[Int, (Int, String)](
      dataset.topology.edges.head._1 -> (0 -> "left_key"),
      dataset.topology.edges.head._2 -> (0 -> "right_key")
    )
    val joins = (1 until dataset.topology.edgeCount).map { edgeIndex =>
      val (leftVertex, rightVertex) = dataset.topology.edges(edgeIndex)
      val endpoints = Vector(leftVertex -> "left_key", rightVertex -> "right_key")
      val equalities = endpoints.flatMap { case (vertex, column) =>
        representatives.get(vertex).map { case (knownEdge, knownColumn) =>
          s"\"e$knownEdge\".\"$knownColumn\" = \"e$edgeIndex\".\"$column\""
        }
      }
      require(equalities.nonEmpty, s"edge $edgeIndex must attach to the prefix")
      endpoints.foreach { case (vertex, column) =>
        representatives.getOrElseUpdate(vertex, edgeIndex -> column)
      }
      val predicate = equalities.mkString("\n AND ")
      s"""JOIN "pg_temp"."${tableName(dataset, edgeIndex)}" AS "e$edgeIndex"
         |  ON $predicate""".stripMargin
    }.mkString("\n")

    s"""/* ${dataset.id}: independently authored synthetic correctness core. */
       |SELECT
       |$projections
       |FROM "pg_temp"."${tableName(dataset, 0)}" AS "e0"
       |$joins
       |""".stripMargin
  }

  def install(connection: Connection): Unit = {
    Using.resource(connection.createStatement()) { statement =>
      All.foreach { dataset =>
        (0 until dataset.topology.edgeCount).foreach { edgeIndex =>
          statement.execute(
            s"""CREATE TEMPORARY TABLE "${tableName(dataset, edgeIndex)}" (
               |  "row_id" BIGINT NOT NULL,
               |  "left_key" BIGINT,
               |  "right_key" BIGINT,
               |  "payload" TEXT
               |)""".stripMargin
          )
        }
      }
    }

    All.foreach { dataset =>
      (0 until dataset.topology.edgeCount).foreach { edgeIndex =>
        val sql =
          s"""INSERT INTO "${tableName(dataset, edgeIndex)}"
             |  ("row_id", "left_key", "right_key", "payload")
             |VALUES (?, ?, ?, ?)""".stripMargin
        Using.resource(connection.prepareStatement(sql)) { insert =>
          rows(dataset, edgeIndex).foreach(add(insert, _))
          insert.executeBatch()
        }
      }
    }
  }

  private def rows(dataset: Dataset, edgeIndex: Int): Vector[Row] = {
    val (leftVertex, rightVertex) = dataset.topology.edges(edgeIndex)
    val productive = dataset.shape match {
      case Sparse =>
        (0 until 2).toVector.flatMap { planted =>
          val row = Row(
            rowId = 1_000_000L + planted * 100L + edgeIndex,
            leftKey = Some(100_000_000L + planted * 100L + leftVertex),
            rightKey = Some(100_000_000L + planted * 100L + rightVertex),
            payload =
              if (planted == 0) None
              else Some(s"planted-$planted-edge-$edgeIndex")
          )
          Vector(row, row)
        }
      case Dense =>
        (0 until 2).toVector.flatMap { leftOrdinal =>
          (0 until 2).toVector.flatMap { rightOrdinal =>
            val row = Row(
              rowId =
                3_000_000L + edgeIndex * 10_000L + leftOrdinal * 100L + rightOrdinal,
              leftKey = Some(
                200_000_000L + leftVertex * 10_000L + leftOrdinal
              ),
              rightKey = Some(
                200_000_000L + rightVertex * 10_000L + rightOrdinal
              ),
              payload =
                if (leftOrdinal == 0 && rightOrdinal == 0) None
                else
                  Some(
                    s"block-0-$leftOrdinal-$rightOrdinal-edge-$edgeIndex"
                  )
            )
            Vector(row, row)
          }
        }
    }

    productive ++ Vector(
      Row(
        5_000_000L + edgeIndex * 10_000L,
        Some(500_000_000L + edgeIndex * 10_000L),
        Some(600_000_000L + edgeIndex * 10_000L),
        Some(s"dangling-$edgeIndex-0")
      ),
      Row(
        5_000_001L + edgeIndex * 10_000L,
        Some(500_000_001L + edgeIndex * 10_000L),
        Some(600_000_001L + edgeIndex * 10_000L),
        Some(s"dangling-$edgeIndex-1")
      ),
      Row(
        7_000_000L + edgeIndex * 10_000L,
        None,
        Some(700_000_000L + edgeIndex),
        Some(s"null-left-$edgeIndex")
      ),
      Row(
        7_000_001L + edgeIndex * 10_000L,
        Some(710_000_000L + edgeIndex),
        None,
        None
      )
    )
  }

  private def add(statement: PreparedStatement, row: Row): Unit = {
    statement.setLong(1, row.rowId)
    setNullableLong(statement, 2, row.leftKey)
    setNullableLong(statement, 3, row.rightKey)
    row.payload match {
      case Some(value) => statement.setString(4, value)
      case None        => statement.setNull(4, Types.VARCHAR)
    }
    statement.addBatch()
  }

  private def setNullableLong(
      statement: PreparedStatement,
      index: Int,
      value: Option[Long]
  ): Unit = value match {
    case Some(present) => statement.setLong(index, present)
    case None          => statement.setNull(index, Types.BIGINT)
  }
}

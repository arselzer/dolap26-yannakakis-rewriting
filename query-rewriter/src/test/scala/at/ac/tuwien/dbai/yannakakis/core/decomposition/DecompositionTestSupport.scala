package at.ac.tuwien.dbai.yannakakis.core.decomposition

import at.ac.tuwien.dbai.yannakakis.core.CoreModelTestSupport.{
  bagId,
  column,
  edgeId,
  model => coreModel,
  relation,
  variable
}
import at.ac.tuwien.dbai.yannakakis.core.{Hyperedge, QueryHypergraph}

private[decomposition] object DecompositionTestSupport {
  def decompositionModel[A](
      value: Either[Vector[DecompositionValidationError], A]
  ): A =
    value.fold(
      errors =>
        throw new AssertionError(errors.map(_.message).mkString("; ")),
      identity
    )

  def edge(id: Int, variables: Int*): Hyperedge = {
    val occurrence = relation(id)
    coreModel(
      Hyperedge.create(
        edgeId(id),
        occurrence,
        variables.zipWithIndex.map { case (variableValue, ordinal) =>
          column(occurrence, ordinal) -> variable(variableValue)
        }
      )
    )
  }

  def graph(edges: Hyperedge*): QueryHypergraph =
    coreModel(QueryHypergraph.create(edges))

  def bag(
      id: Int,
      vertices: Iterable[Int],
      cover: Iterable[Int]
  ): CoveredBag =
    decompositionModel(
      CoveredBag.create(
        bagId(id),
        vertices.map(variable),
        cover.map(edgeId)
      )
    )

  def treeEdge(first: Int, second: Int): BagTreeEdge =
    decompositionModel(BagTreeEdge.create(bagId(first), bagId(second)))

  def decomposition(
      root: Int,
      bags: Iterable[CoveredBag],
      treeEdges: Iterable[BagTreeEdge]
  ): CoveredDecomposition =
    decompositionModel(
      CoveredDecomposition.create(bagId(root), bags, treeEdges)
    )

  def errors(
      hypergraph: QueryHypergraph,
      decomposition: CoveredDecomposition
  ): Vector[DecompositionValidationError] =
    DecompositionValidator
      .validate(hypergraph, decomposition)
      .swap
      .toOption
      .getOrElse(Vector.empty)
}

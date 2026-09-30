package at.ac.tuwien.dbai.yannakakis.core.decomposition

private[decomposition] object CanonicalOrdering {
  def compareVectors[A](
      left: Vector[A],
      right: Vector[A]
  )(implicit ordering: Ordering[A]): Int = {
    val commonLength = math.min(left.length, right.length)
    var index = 0
    while (index < commonLength) {
      val comparison = ordering.compare(left(index), right(index))
      if (comparison != 0) return comparison
      index += 1
    }
    Integer.compare(left.length, right.length)
  }
}

package at.ac.tuwien.dbai.yannakakis.algorithm.soft

import at.ac.tuwien.dbai.yannakakis.algorithm.soft.SoftFailure.InvalidResourceLimit
import at.ac.tuwien.dbai.yannakakis.algorithm.soft.SoftResource._

/** Hard limits for exhaustive, fixed-width candidate generation. */
final class SoftResourceLimits private (
    val maxCovers: Long,
    val maxCoverPairs: Long,
    val maxComponents: Long,
    val maxDerivations: Long,
    val maxCandidates: Long,
    val maxCandidateCoverChecks: Long
) extends Serializable {
  override def equals(other: Any): Boolean = other match {
    case that: SoftResourceLimits =>
      maxCovers == that.maxCovers &&
        maxCoverPairs == that.maxCoverPairs &&
        maxComponents == that.maxComponents &&
        maxDerivations == that.maxDerivations &&
        maxCandidates == that.maxCandidates &&
        maxCandidateCoverChecks == that.maxCandidateCoverChecks
    case _ => false
  }

  override def hashCode(): Int =
    (
      maxCovers,
      maxCoverPairs,
      maxComponents,
      maxDerivations,
      maxCandidates,
      maxCandidateCoverChecks
    ).hashCode()

  override def toString: String =
    s"SoftResourceLimits($maxCovers,$maxCoverPairs,$maxComponents," +
      s"$maxDerivations,$maxCandidates,$maxCandidateCoverChecks)"
}

object SoftResourceLimits {
  val default: SoftResourceLimits = new SoftResourceLimits(
    maxCovers = 100000L,
    maxCoverPairs = 100000000L,
    maxComponents = 1000000L,
    maxDerivations = 5000000L,
    maxCandidates = 1000000L,
    maxCandidateCoverChecks = 10000000L
  )

  def create(
      maxCovers: Long = default.maxCovers,
      maxCoverPairs: Long = default.maxCoverPairs,
      maxComponents: Long = default.maxComponents,
      maxDerivations: Long = default.maxDerivations,
      maxCandidates: Long = default.maxCandidates,
      maxCandidateCoverChecks: Long = default.maxCandidateCoverChecks
  ): Either[Vector[InvalidResourceLimit], SoftResourceLimits] = {
    val supplied = Vector(
      Covers -> maxCovers,
      CoverPairs -> maxCoverPairs,
      Components -> maxComponents,
      Derivations -> maxDerivations,
      Candidates -> maxCandidates,
      CandidateCoverChecks -> maxCandidateCoverChecks
    )
    val errors = supplied.collect {
      case (resource, value) if value <= 0L =>
        InvalidResourceLimit(resource, value)
    }

    if (errors.nonEmpty) Left(errors)
    else
      Right(
        new SoftResourceLimits(
          maxCovers,
          maxCoverPairs,
          maxComponents,
          maxDerivations,
          maxCandidates,
          maxCandidateCoverChecks
        )
      )
  }
}

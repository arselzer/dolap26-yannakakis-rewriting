package at.ac.tuwien.dbai.yannakakis.algorithm.candidate

/** Mutable accounting local to one pure top-K enumeration call. */
private[candidate] final class CandidateTopKBudget(
    limits: CandidateResourceLimits
) {
  private var retainedPartialPlans = 0L
  private var combinationStates = 0L

  def retainedPartialPlanCount: Long = retainedPartialPlans
  def combinationStateCount: Long = combinationStates

  def retainPartialPlans(count: Int): Either[CandidateFailure, Unit] = {
    val required = BigInt(retainedPartialPlans) + count
    if (required > BigInt(limits.maxRetainedPartialPlans)) {
      Left(
        CandidateFailure.ResourceLimitExceeded(
          CandidateResource.RetainedPartialPlans,
          required,
          limits.maxRetainedPartialPlans
        )
      )
    } else {
      retainedPartialPlans = required.toLong
      Right(())
    }
  }

  /** Accounts one distinct coordinate before admitting it to a visited set. */
  def admitCombinationState(): Either[CandidateFailure, Unit] = {
    val required = BigInt(combinationStates) + 1
    if (required > BigInt(limits.maxCombinationStates)) {
      Left(
        CandidateFailure.ResourceLimitExceeded(
          CandidateResource.CombinationStates,
          required,
          limits.maxCombinationStates
        )
      )
    } else {
      combinationStates += 1L
      Right(())
    }
  }
}

package at.ac.tuwien.dbai.yannakakis.api

/** Request-wide choice of the canonical Yannakakis reduction passes. */
sealed trait YannakakisReductionMode extends Product with Serializable {
  def id: String
}

sealed trait YannakakisReductionModeError extends Product with Serializable {
  def code: String
  def message: String
}

object YannakakisReductionModeError {
  case object MissingValue extends YannakakisReductionModeError {
    override val code: String = "reduction-mode.missing"
    override val message: String = "Yannakakis reduction mode must not be null"
  }

  final case class UnsupportedValue(value: String)
      extends YannakakisReductionModeError {
    override val code: String = "reduction-mode.unsupported"
    override val message: String =
      s"Unsupported Yannakakis reduction mode: $value"
  }
}

object YannakakisReductionMode {
  case object FullTwoPass extends YannakakisReductionMode {
    override val id: String = "full"
  }

  case object BottomUpOnly extends YannakakisReductionMode {
    override val id: String = "bottom-up"
  }

  case object NoReduction extends YannakakisReductionMode {
    override val id: String = "none"
  }

  val Default: YannakakisReductionMode = FullTwoPass
  val Values: Vector[YannakakisReductionMode] =
    Vector(FullTwoPass, BottomUpOnly, NoReduction)

  /** Parses an untrusted value without trimming or case normalization. */
  def parse(
      value: String
  ): Either[YannakakisReductionModeError, YannakakisReductionMode] = {
    if (value == null) Left(YannakakisReductionModeError.MissingValue)
    else
      Values.find(_.id == value) match {
        case Some(mode) => Right(mode)
        case None =>
          Left(YannakakisReductionModeError.UnsupportedValue(value))
      }
  }
}

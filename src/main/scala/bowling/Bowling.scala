package bowling

import bowling.Frame.toFrames

object Bowling {
  def roll(pins: List[Int]): Game = pinsValidated(pins) match {
    case Left(e) :: _ => Game(Left(e))
    case _ => Game(Right(toFrames(pins)))
  }

  private def pinsValidated(pins: List[Int]): List[Either[Error, Int]] = {
    val validatedPins = pins.map {
      case p if p < 0 => Left(new Error(s"Negative number is forbidden bowl: $p"))
      case p if p > 10 => Left(new Error(s"Impossible to knock down more than 10 pins: $p"))
      case p => Right(p)
    }

    validatedPins.find(_.isLeft) match {
      case None => validatedPins
      case Some(error) => List(error)
    }
  }
}

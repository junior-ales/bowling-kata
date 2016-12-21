package bowling

import bowling.Frame.toFrames

object Bowling {
  def roll(pins: List[Int]): Game = {
    try {
      Game(Right(toFrames(pins)))
    } catch {
      case e: Error => Game(Left(e))
    }
  }
}

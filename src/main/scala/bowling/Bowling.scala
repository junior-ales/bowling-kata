package bowling

import bowling.Frame.toFrames

object Bowling {
  def roll(pins: List[Int]): Game = Game(toFrames(pins))
}

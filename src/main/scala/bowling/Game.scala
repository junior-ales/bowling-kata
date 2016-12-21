package bowling

import bowling.Frame.Last

sealed trait Game {
  type BowlingResult = List[(Frame, Int)]

  def result: BowlingResult = {
    def calcBowlingResult(_fs: List[Frame], res: BowlingResult): BowlingResult = _fs match {
      case Nil => res
      case f :: fs => calcBowlingResult(fs, (f, f.score(fs)) :: res)
    }

    calcBowlingResult(this.frames, List())
  }

  def frames: List[Frame]

  def score: Int = result.foldLeft(0)((acc, result) => acc + result._2)
}

object Game {

  final case class Started(frames: List[Frame]) extends Game

  final case class GameOver(frames: List[Frame]) extends Game

  final case class InvalidGame(error: Error) extends Game {
    def frames: List[Frame] = List()
  }

  case object NotStarted extends Game {
    override def frames = List()
  }

  def apply(frames: Either[Error, List[Frame]]): Game = frames match {
    case Right(fs) => createValidGame(fs)
    case Left(error) => InvalidGame(error)
  }

  private def createValidGame(frames: List[Frame]): Game = frames.lastOption match {
    case None => NotStarted
    case Some(_) if frames.length < 10 => Started(frames)
    case Some(f: Last) if f.finished => GameOver(frames)
    case _ => Started(frames)
  }
}

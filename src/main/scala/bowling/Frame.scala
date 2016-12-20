package bowling

sealed trait Frame {
  def bowl1: Int

  def score(nextFrames: List[Frame] = List()): Int
}

object Frame {
  final case class OnGoing(bowl1: Int) extends Frame {
    def score(nextFrames: List[Frame]): Int = this.bowl1
  }

  final case class Open(bowl1: Int, bowl2: Int) extends Frame {
    def score(nextFrames: List[Frame]): Int = this.bowl1 + this.bowl2
  }

  final case class Last(bowl1: Int, bowl2: Option[Int] = None, fillBall: Option[Int] = None) extends Frame {
    def finished: Boolean = (bowl2, fillBall) match {
      case (Some(_), Some(_)) => true
      case (Some(b2), None) => bowl1 + b2 < 10
      case _ => false
    }

    def score(nextFrames: List[Frame]): Int = bowl1 + bowl2.getOrElse(0) + fillBall.getOrElse(0)
  }

  final case class Spare(bowl1: Int, bowl2: Int) extends Frame {
    def score(nextFrames: List[Frame]): Int =
      nextFrames.headOption.getOrElse(NoFrame).bowl1 + this.bowl1 + this.bowl2
  }

  case object Strike extends Frame {
    def bowl1: Int = 10

    def score(nextFrames: List[Frame]): Int = (nextFrames match {
      case Spare(b1, b2) :: _ => b1 + b2
      case Open(b1, b2) :: _ => b1 + b2
      case OnGoing(b) :: _ => b
      case Last(b1, optionalBowl2, _) :: _ => b1 + optionalBowl2.getOrElse(0)
      case Strike :: fs => Strike.bowl1 + fs.headOption.getOrElse(NoFrame).bowl1
      case _ => 0
    }) + this.bowl1
  }

  case object NoFrame extends Frame {
    def bowl1: Int = 0

    def score(nextFrames: List[Frame]): Int = this.bowl1
  }

  def apply(bowl: Int): Frame =
    if (bowl == 10) Strike
    else OnGoing(bowl)

  def apply(bowl1: Int, bowl2: Int): Frame = Frame(OnGoing(bowl1), bowl2)

  def apply(bowl1: Int, bowl2: Int, bowl3: Int): Frame = Last(bowl1, Some(bowl2), Some(bowl3))

  private def apply(frame: OnGoing, bowl: Int): Frame = frame match {
    case OnGoing(score) if score + bowl == 10 => Spare(score, bowl)
    case OnGoing(score) => Open(score, bowl)
  }

  def toFrames(bowls: List[Int]): List[Frame] = {
    if (bowls.isEmpty) Nil
    else bowls.foldLeft(List.empty[Frame])(mergeFrames)
  }

  private def mergeFrames(frames: List[Frame], bowl: Int): List[Frame] = frames.lastOption match {
    case Some(f) if frames.length == 10 => frames.init :+ lastFrame(f, bowl)
    case Some(f: OnGoing) => frames.init :+ Frame(f, bowl)
    case _  => frames :+ Frame(bowl)
  }

  private def lastFrame(frame: Frame, bowl: Int): Last = frame match {
    case NoFrame => Last(bowl)
    case OnGoing(b) => Last(b, Some(bowl))
    case Spare(b1, b2) => Last(b1, Some(b2), Some(bowl))
    case Strike => Last(10, Some(bowl))
    case Last(b1, None, None) => Last(b1, Some(bowl))
    case Last(b1, Some(b2), None) => Last(b1, Some(b2), Some(bowl))
    case _ => throw new Error(s"forbidden: no more bowls allowed")
  }

}

package bowling

sealed trait Frame {
  def bowl1: Int

  def score(nextFrames: List[Frame]): Int
}

object Frame {

  final case class OnGoing(bowl1: Int) extends Frame {
    def score(nextFrames: List[Frame]): Int = nextFrames match {
      case Nil => this.bowl1
      case _ => throw new Error("OnGoing frame + other frames are forbidden")
    }
  }

  final case class Open(bowl1: Int, bowl2: Int) extends Frame {
    def score(nextFrames: List[Frame]): Int = bowl1 + bowl2
  }

  final case class Spare(bowl1: Int, bowl2: Int) extends Frame {
    def score(nextFrames: List[Frame]): Int =
      nextFrames.headOption.getOrElse(NoFrame).bowl1 + this.bowl1 + this.bowl2
  }

  case object Strike extends Frame {
    def bowl1: Int = 10

    def score(nextFrames: List[Frame]): Int = (nextFrames match {
      case List() => 0
      case NoFrame :: _ => 0
      case Strike :: fs => Strike.bowl1 + fs.headOption.getOrElse(NoFrame).bowl1
      case Spare(b1, b2) :: _ => b1 + b2
      case Open(b1, b2) :: _ => b1 + b2
      case OnGoing(b) :: _ => b
    }) + this.bowl1
  }

  case object NoFrame extends Frame {
    def bowl1: Int = 0

    def score(nextFrames: List[Frame]): Int = this.bowl1
  }

  def apply(bowl: Int): Frame =
    if (bowl == 10) Strike else OnGoing(bowl)

  def apply(bowl1: Int, bowl2: Int): Frame = Frame(OnGoing(bowl1), bowl2)

  def apply(frame: OnGoing, bowl: Int): Frame = frame match {
    case OnGoing(score) if score + bowl == 10 => Spare(score, bowl)
    case OnGoing(score) if score + bowl < 10 => Open(score, bowl)
    //case _ if bowl == 10 => throw new Error("forbidden ongoing + 10")
  }

  private def mergeFrames(frame: Frame, bowl: Int): List[Frame] = frame match {
    case f: OnGoing => List(Frame(f, bowl))
    case _ => List(frame, Frame(bowl))
  }

  def toFrames(bowls: List[Int]): List[Frame] = {
    def _toFrames(_bowls: List[Int], acc: List[Frame]): List[Frame] = _bowls match {
      case Nil => acc
      case b :: bs => _toFrames(bs, acc.init ::: mergeFrames(acc.last, b))
    }

    bowls match {
      case Nil => Nil
      case b :: bs => _toFrames(bs, List(Frame(b)))
    }
  }
}

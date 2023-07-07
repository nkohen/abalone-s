
case class Board(marbleStates: Vector[MarbleState], history: Vector[Move] = Vector.empty, blackOff: Int = 0, whiteOff: Int = 0) {
  def whiteMarbles: Vector[Pos] = Pos.all.filter(pos => marbleStates(pos.index) == White)
  def blackMarbles: Vector[Pos] = Pos.all.filter(pos => marbleStates(pos.index) == Black)

  def applyMove(move: Move): Board = {
    val newMarbleStates = marbleStates.zipWithIndex.map { case (prevState, index) =>
      val pos = Pos(index)
      move.movingMarbles.find { case (_, p2) => p2 == pos } match {
        case Some((p1, _)) => marbleStates(p1.index)
        case None => move.movingMarbles.find { case (p1, _) => p1 == pos } match {
          case Some(_) => Empty
          case None => prevState
        }
      }
    }

    val newHistory = history.appended(move)

    val killedColorOpt = move.killed.map(pos => marbleStates(pos.index))
    val newBlackOff = blackOff + killedColorOpt.count(_ == Black)
    val newWhiteOff = whiteOff + killedColorOpt.count(_ == White)

    Board(newMarbleStates, newHistory, newBlackOff, newWhiteOff)
  }
}

object Board {

  val BelgianDaisy: Board = Board(Vector(
    Black, Black, Empty, White, White,
    Black, Black, Black, White, White, White,
    Empty, Black, Black, Empty, White, White, Empty,
    Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty,
    Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty,
    Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty,
    Empty, White, White, Empty, Black, Black, Empty,
    White, White, White, Black, Black, Black,
    White, White, Empty, Black, Black
  ))

  val Standard: Board = Board(Vector(
    Black, Black, Black, Black, Black,
    Black, Black, Black, Black, Black, Black,
    Empty, Empty, Black, Black, Black, Empty, Empty,
    Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty,
    Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty,
    Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty,
    Empty, Empty, White, White, White, Empty, Empty,
    White, White, White, White, White, White,
    White, White, White, White, White
  ))
}

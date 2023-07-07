
case class Situation(board: Board, blackToPlay: Boolean) {

  def whiteToPlay: Boolean = !blackToPlay
  def playerMarbleState: MarbleState = if (blackToPlay) Black else White

  /** Finds all moves involving pos moving to a new position previously empty or inhabited by an enemy. */
  private def constructMoves(pos: Pos, direction: MoveDirection): Vector[Move] = {
    pos.inDirection(direction) match {
      case None || Some(Pos.OFF) => Vector.empty
      case Some(newPos) =>
        val prevStateOfNewPos = board.marbleStates(newPos.index)
        if (prevStateOfNewPos == Empty) {
          // Single marble push
          val singleMarblePush = Vector(Move(Map((pos, newPos))))

          // 2 slide moves
          val twoMarbleSlideMoves = MoveDirection.all
            .filterNot(d => d == direction || d == direction.opposite)
            .map(pos.inDirection)
            .filter {
              case None || Some(Pos.OFF) => false
              case Some(_) => true
            }.map(_.get)
            .filter(neighbor => board.marbleStates(neighbor.index) == playerMarbleState)
            .filter { ally =>
              ally.inDirection(direction) match {
                case None || Some(Pos.OFF) => false
                case Some(Pos(index)) => board.marbleStates(index) == Empty
              }
            }
            .map { ally =>
              Move(Map((pos, newPos), (ally, ally.inDirection(direction).get)))
            }

          // 3 slide moves
          val threeMarbleSlideMoves = twoMarbleSlideMoves.flatMap { twoSlide =>
            // Example Order: pos3 pos2 pos1 pos4
            val pos2 = twoSlide.movingMarbles.keys.filterNot(_ == pos).head
            val direction1 = MoveDirection.relativeDirection(pos, pos2).get
            val direction2 = direction1.opposite
            val pos3 = pos2.inDirection(direction1)
            val pos4 = pos.inDirection(direction2)
            Vector(pos3, pos4).filter {
              case None || Some(Pos.OFF) => false
              case Some(_) => true
            }.map(_.get)
              .filter(neighbor => board.marbleStates(neighbor.index) == playerMarbleState)
              .filter { ally =>
                ally.inDirection(direction) match {
                  case None || Some(Pos.OFF) => false
                  case Some(Pos(index)) => board.marbleStates(index) == Empty
                }
              }
              .map { ally =>
                Move(Map((pos, newPos), (pos2, pos2.inDirection(direction).get), (ally, ally.inDirection(direction).get)))
              }
          }

          // 2, 3 push moves
          val pushMoves = {
            pos.inDirection(direction.opposite) match {
              case None || Some(Pos.OFF) => Vector.empty
              case Some(pos2) =>
                if (board.marbleStates(pos2.index) == playerMarbleState) {
                  val twoPushMove = Move(Map((pos, newPos), (pos2, pos)))
                  pos2.inDirection(direction.opposite) match {
                    case None || Some(Pos.OFF) => Vector(twoPushMove)
                    case Some(pos3) =>
                      if (board.marbleStates(pos3.index) == playerMarbleState) {
                        val threePushMove = Move(Map((pos, newPos), (pos2, pos), (pos3, pos2)))
                        Vector(twoPushMove, threePushMove)
                      } else Vector(twoPushMove)
                  }
                } else Vector.empty
            }
          }

          singleMarblePush ++ pushMoves ++ twoMarbleSlideMoves ++ threeMarbleSlideMoves
        } else if (prevStateOfNewPos != playerMarbleState) {
          // Pushing enemy
          val beyondEnemyPos = newPos.inDirection(direction).get
          if (beyondEnemyPos != Pos.OFF && board.marbleStates(beyondEnemyPos.index) == playerMarbleState) {
            Vector.empty
          } else {
            val beyondBeyondEnemyPosOpt = beyondEnemyPos.inDirection(direction)
            if (beyondBeyondEnemyPosOpt.isDefined &&
              beyondBeyondEnemyPosOpt.get != Pos.OFF &&
              board.marbleStates(beyondBeyondEnemyPosOpt.get.index) != Empty) {
              Vector.empty
            } else {
              val enemies = if (beyondEnemyPos == Pos.OFF || board.marbleStates(beyondEnemyPos.index) == Empty) Vector(newPos)
              else Vector(newPos, beyondEnemyPos)

              val allyPos = pos.inDirection(direction.opposite).get
              if (allyPos == Pos.OFF || board.marbleStates(allyPos.index) != playerMarbleState) Vector.empty
              else {
                val allyPos2Opt = allyPos.inDirection(direction.opposite).flatMap {
                  case Pos.OFF => None
                  case p => Some(p)
                }
                val allies = if (allyPos2Opt.isDefined && board.marbleStates(allyPos2Opt.get.index) == playerMarbleState) {
                  Vector(allyPos, allyPos2Opt.get)
                } else Vector(allyPos)

                if (allies.length < enemies.length) Vector.empty
                else if (allies.length == enemies.length) {
                  if (allies.length == 1) { // 2 push 1
                    Vector(Move(Map((allies.head, pos), (pos, newPos), (newPos, beyondEnemyPos))))
                  } else { // 3 push 2
                    Vector(Move(Map((allies.last, allies.head), (allies.head, pos), (pos, newPos), (newPos, beyondEnemyPos), (beyondEnemyPos, beyondBeyondEnemyPosOpt.get))))
                  }
                } else { // 2 or 3 push 1
                  Vector(
                    Move(Map((allies.head, pos), (pos, newPos), (newPos, beyondEnemyPos))),
                    Move(Map((allies.last, allies.head), (allies.head, pos), (pos, newPos), (newPos, beyondEnemyPos)))
                  )
                }
              }
            }
          }
        } else Vector.empty
    }
  }

  lazy val legalMoves: Vector[Move] = {
    Pos.all.filter(pos => board.marbleStates(pos.index) == playerMarbleState).flatMap { pos =>
      MoveDirection.all.flatMap(constructMoves(pos, _))
    }.distinct
  }

  def makeMove(move: Move): Option[Situation] = {
    if (legalMoves.contains(move)) {
      Some(Situation(board.applyMove(move), !blackToPlay))
    } else None
  }
}

object Situation {

  val BelgianDaisy: Situation = Situation(Board.BelgianDaisy, blackToPlay = true)
  val Standard: Situation = Situation(Board.Standard, blackToPlay = true)
}
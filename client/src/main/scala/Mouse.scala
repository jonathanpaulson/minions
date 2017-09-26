package minionsgame.jsclient

import scala.util.{Try, Success, Failure}

import minionsgame.core._
import RichImplicits._

//A thing on the board that could possibly mouse-overed, clicked, on, selected, etc.
sealed trait MouseTarget {
  def findPiece(board: BoardState): Option[Piece] = {
    this match {
      case MousePiece(spec) => board.findPiece(spec)
      case MouseTile(_) => None
      case MouseTech(_) => None
      case MouseReinforcement(_,_) => None
    }
  }
}
case class MousePiece(pieceSpec: PieceSpec) extends MouseTarget
case class MouseTile(loc: Loc) extends MouseTarget
case class MouseTech(techIdx: Int) extends MouseTarget
case class MouseReinforcement(pieceName: PieceName, side:Side) extends MouseTarget

//Current state of the mouse, helper object
object MouseState {

}

//Current state of the mouse
case class MouseState() {

  //The current thing that was clicked on and is selected
  var selected: Option[MouseTarget] = None
  //The current target moused-over
  var hovered : Option[MouseTarget] = None
  //The current location of the mouse
  var hoverLoc: Option[Loc] = None
  //When clicking and dragging for a movement, the path of that movement
  var path : List[Loc] = Nil

  def clear() = {
    hoverLoc = None
    hovered = None
    selected = None
    path = Nil
  }

  def clearSelect() = {
    selected = None
    path = Nil
  }
  private def clearPath() = {
    path = Nil
  }

  //Update path to be a shortest path from [selected] to [hoverLoc] that
  //shares the longest prefix with the current [path].
  private def updatePath(board: Board): Unit = {
    val bState = board.curState

    val selectedPiece = selected.flatMap { target => target.findPiece(bState) }
    selectedPiece match {
      case None => clearPath()
      case Some(selectedPiece) =>
        val hoverPiece = hovered.flatMap { target => target.findPiece(bState) }
        hoverLoc match {
          case None => clearPath()
          case Some(hoverLoc) =>
            val newLegalMove = {
              hoverPiece match {
                case None =>
                  //Ordinary move to location
                  bState.findLegalMove(selectedPiece,pathBias=path) { loc => loc == hoverLoc }
                case Some(hoverPiece) =>
                  //Merge into friendly swarm
                  if(hoverPiece.side == selectedPiece.side)
                    bState.findLegalMove(selectedPiece,pathBias=path) { loc => loc == hoverLoc }
                  //Attack enemy piece
                  else {
                    val spStats = selectedPiece.curStats(bState)
                    val hpStats = hoverPiece.curStats(bState)
                    if(!bState.canAttack(spStats,attackerHasMoved=false,selectedPiece.actState,hpStats))
                      None
                    else {
                      bState.findLegalMove(selectedPiece,pathBias=path) { loc =>
                        bState.tiles.topology.distance(loc, hoverLoc) <= spStats.attackRange &&
                        (loc == selectedPiece.loc || !spStats.isLumbering)
                      }
                    }
                  }
              }
            }
            newLegalMove match {
              case None => ()
              case Some(newPath) =>
                path = newPath
            }
        }
    }
  }

  private def movementsOfPath(piece: Piece): Option[PlayerAction] = {
    if(path.length <= 1) None
    else Some(Movements(List(Movement(piece.spec, path.toVector))))
  }

  private def getLocAndDelta(pixelLoc: PixelLoc, board: Board, flipDisplay: Boolean): (Loc,HexVec) = {
    val hexLoc = HexLoc.ofPixel(pixelLoc, Drawing.gridSize)
    hexLoc.round(flipDisplay,board.curState)
  }

  private def getPiece(loc: Loc, hexDelta: HexVec, board: Board, requireSide: Option[Side]): Option[Piece] = {
    val bState = board.curState
    if(!bState.pieces.inBounds(loc))
      None
    else {
      val piece = {
        bState.pieces(loc) match {
          case Nil => None
          case p :: Nil => Some(p)
          case p1 :: p2 :: Nil =>
            hexDelta.closestCorner() match {
              case 0 | 4 | 5 => Some(p1)
              case 1 | 2 | 3 => Some(p2)
            }
          case p1 :: p2 :: p3 :: Nil =>
            hexDelta.hexant() match {
              case 4 | 5 => Some(p1)
              case 2 | 3 => Some(p2)
              case 0 | 1 => Some(p3)
              case _ => assertUnreachable()
            }
          case _ => None
        }
      }
      requireSide match {
        case None => piece
        case Some(side) =>
          piece.filter { piece => piece.side == side }
      }
    }
  }

  private def getTarget(pixelLoc: PixelLoc, game: Game, board: Board, flipDisplay: Boolean, side: Option[Side], requireSide: Option[Side]): Option[MouseTarget] = {
    val (loc,hexDelta) = getLocAndDelta(pixelLoc,board,flipDisplay)
    side match {
      case None => None
      case Some(side) =>
        val bState = board.curState
        if(side != bState.side)
          None
        else {
          getPiece(loc,hexDelta,board,requireSide) match {
            case Some(piece) => Some(MousePiece(piece.spec))
            case None =>
              if(bState.inBounds(loc))
                Some(MouseTile(loc))
              else {
                ReinforcementsUI.getSelectedPiece(side,flipDisplay,bState,loc) match {
                  case Some(pieceName) => Some(MouseReinforcement(pieceName,side))
                  case None =>
                    TechUI.getSelectedTechIdx(game,loc) match {
                      case Some(techIdx) => Some(MouseTech(techIdx))
                      case None =>
                        if(loc == EndTurnUI.loc) {
                          //TODO - add a new MouseTarget for this, add appropriate
                          //queries and responses to protocol.
                          //End of turn button should do nothing client side, instead
                          //it should simply signal the server, and if all boards
                          //have ended turn, OR the time limit has passed, the turn
                          //ends
                          //TODO don't forget to have the server auto-tech and
                          //to have it auto-pick spells for everyone if those
                          //were not done that turn.
                          None
                        }
                        else
                          None
                    }
                }
              }
          }
        }
    }
  }

  def handleMouseDown(pixelLoc: PixelLoc, game: Game, board: Board, flipDisplay: Boolean, side: Option[Side]) : Unit = {
    selected = getTarget(pixelLoc,game,board,flipDisplay,side,requireSide=side)
    clearPath()
  }


  def handleMouseUp(pixelLoc: PixelLoc, game: Game, board: Board, flipDisplay: Boolean)
    (makeActionId: () => String)
    (doGameAction:GameAction => Unit)
    (doBoardAction:BoardAction => Unit): Unit = {
    val (loc,hexDelta) = getLocAndDelta(pixelLoc,board,flipDisplay)

    //Branch based on what we selected on the mouseDown
    selected match {
      case None | Some(MouseTile(_)) => ()
      case Some(MouseTech(techIdx)) =>
        val techState = game.techLine(techIdx)
        techState.level(game.curSide) match {
          case TechLocked | TechUnlocked =>
            doGameAction(PerformTech(game.curSide,techIdx))
          case TechAcquired =>
            techState.tech match {
              case PieceTech(pieceName) =>
                doBoardAction(DoGeneralBoardAction(BuyReinforcement(pieceName),makeActionId()))
            }
        }
      case Some(MouseReinforcement(pieceName,_)) =>

        doBoardAction(PlayerActions(List(Spawn(loc,pieceName)),makeActionId()))
      case Some(MousePiece(selectedSpec)) =>
        board.curState.findPiece(selectedSpec) match {
          case None => ()
          case Some(piece) =>
            //Move based on the path, if we have a nontrivial path
            val movement: Option[PlayerAction] = movementsOfPath(piece)
            //Attack if enemy piece under the mouseUp
            val attack: Option[PlayerAction] = {
              getPiece(loc,hexDelta,board,requireSide=None) match {
                case None => None
                case Some(other) =>
                  if(other.side == piece.side) None
                  else Some(Attack(piece.spec, other.spec))
              }
            }
            val actions = List(movement,attack).flatten
            if(actions.length > 0)
              doBoardAction(PlayerActions(actions,makeActionId()))
        }
    }
    clearSelect()
  }


  def handleMouseMove(pixelLoc: PixelLoc, game: Game, board: Board, flipDisplay: Boolean, side: Option[Side]) : Unit = {
    val (loc,_) = getLocAndDelta(pixelLoc,board,flipDisplay)
    hoverLoc = Some(loc)
    hovered = getTarget(pixelLoc,game,board,flipDisplay,side,requireSide=None)
    updatePath(board)
  }

}

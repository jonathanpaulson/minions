package minionsgame.jsclient

import scala.util.{Try, Success, Failure}

import minionsgame.core._
import RichImplicits._

//A thing on the board that could possibly mouse-overed, clicked, on, selected, etc.
sealed trait MouseTarget {
  def findPiece(board: BoardState): Option[Piece] = {
    this match {
      case MouseNone => None
      case MousePiece(spec) => board.findPiece(spec)
      case MouseTile(_) => None
      case MouseTech(_) => None
      case MouseReinforcement(_,_) => None
      case MouseDeadPiece(_) => None
      case MouseEndTurn => None
      case MouseResignBoard => None
    }
  }
}
case object MouseNone extends MouseTarget
case class MousePiece(pieceSpec: PieceSpec) extends MouseTarget
case class MouseTile(loc: Loc) extends MouseTarget
case class MouseTech(techIdx: Int) extends MouseTarget
case class MouseReinforcement(pieceName: PieceName, side:Side) extends MouseTarget
case class MouseDeadPiece(pieceSpec: PieceSpec) extends MouseTarget
case object MouseEndTurn extends MouseTarget
case object MouseResignBoard extends MouseTarget

//Different modes the mouse can be in for selecting different things
sealed trait MouseMode {
  //Handlers in this mouse mode
  def handleMouseDown(curTarget: MouseTarget, curLoc: Loc, game: Game, board: BoardState): Unit
  def handleMouseMove(dragTarget: MouseTarget, curTarget: MouseTarget, curLoc: Loc, game: Game, board: BoardState): Unit
  def handleMouseUp(dragTarget: MouseTarget, curTarget: MouseTarget, curLoc: Loc, game: Game, board: BoardState, boardIdx: Int, ctrlPressed: Boolean): Unit
}

//---------------------------------------------------------------------------------------
//Current state of the mouse.

case class MouseState(val ourSide: Option[Side], val flipDisplay: Boolean, val client: Client)
{
  //The current target moused-over
  var hovered : MouseTarget = MouseNone
  //The current location of the mouse
  var hoverLoc: Option[Loc] = None
  //The target of the last mousedown, if the mouse is still held down.
  var dragTarget: MouseTarget = MouseNone

  //Last pixel location of the mouse, for refreshing the state in case the board changed
  var lastPixelLoc: Option[PixelLoc] = None

  //Current mode that the mouse is operating in.
  var mode: MouseMode = NormalMouseMode(this)

  def clear() = {
    hoverLoc = None
    hovered = MouseNone
    dragTarget = MouseNone
    lastPixelLoc = None
    mode = NormalMouseMode(this)
  }

  def clearSelect() = {
    dragTarget = MouseNone
    mode = NormalMouseMode(this)
  }

  def refresh(game: Game, board: BoardState) = {
    lastPixelLoc.foreach { pixelLoc =>
      handleMouseMove(pixelLoc,game,board)
    }
  }

  private def getLocAndDelta(pixelLoc: PixelLoc, board: BoardState): (Loc,HexVec) = {
    val hexLoc = HexLoc.ofPixel(pixelLoc, Drawing.gridSize)
    hexLoc.round(flipDisplay,board)
  }

  private def getPiece(loc: Loc, hexDelta: HexVec, board: BoardState): Option[Piece] = {
    if(!board.pieces.inBounds(loc))
      None
    else {
      board.pieces(loc) match {
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
  }

  private def getTarget(pixelLoc: PixelLoc, game: Game, board: BoardState): MouseTarget = {
    val (loc,hexDelta) = getLocAndDelta(pixelLoc,board)
    getPiece(loc,hexDelta,board) match {
      case Some(piece) => MousePiece(piece.spec)
      case None =>
        if(board.inBounds(loc))
          MouseTile(loc)
        else {
          UI.Reinforcements.getSelectedPiece(game.curSide,flipDisplay,board,loc) match {
            case Some(pieceName) => MouseReinforcement(pieceName,game.curSide)
            case None =>
              UI.DeadPieces.getSelectedSpec(board,loc) match {
                case Some(pieceSpec) => MouseDeadPiece(pieceSpec)
                case None =>
                  UI.Tech.getSelectedTechIdx(game,loc) match {
                    case Some(techIdx) => MouseTech(techIdx)
                    case None =>
                      if(loc == UI.EndTurn.loc)
                        MouseEndTurn
                      else if(loc == UI.ResignBoard.loc)
                        MouseResignBoard
                      else
                        MouseNone
                  }
              }
          }
        }
    }
  }

  def handleMouseDown(pixelLoc: PixelLoc, game: Game, board: BoardState) : Unit = {
    lastPixelLoc = Some(pixelLoc)
    ourSide match {
      case None => ()
      case Some(ourSide) =>
        if(ourSide == board.side) {
          val curTarget = getTarget(pixelLoc,game,board)
          val (curLoc,_) = getLocAndDelta(pixelLoc,board)
          dragTarget = curTarget

          mode.handleMouseDown(curTarget,curLoc,game,board)
        }
    }
  }

  def handleMouseUp(pixelLoc: PixelLoc, game: Game, board: BoardState, boardIdx: Int, ctrlPressed: Boolean): Unit = {
    lastPixelLoc = Some(pixelLoc)
    ourSide match {
      case None => ()
      case Some(ourSide) =>
        if(ourSide == board.side) {
          val curTarget = getTarget(pixelLoc,game,board)
          val (curLoc,_) = getLocAndDelta(pixelLoc,board)
          mode.handleMouseUp(dragTarget,curTarget,curLoc,game,board,boardIdx,ctrlPressed)
          dragTarget = MouseNone
        }
    }
  }

  def handleMouseMove(pixelLoc: PixelLoc, game: Game, board: BoardState) : Unit = {
    lastPixelLoc = Some(pixelLoc)

    val curTarget = getTarget(pixelLoc,game,board)
    val (curLoc,_) = getLocAndDelta(pixelLoc,board)
    hoverLoc = Some(curLoc)
    hovered = curTarget

    ourSide match {
      case None => ()
      case Some(ourSide) =>
        if(ourSide == board.side) {
          mode.handleMouseMove(dragTarget,curTarget,curLoc,game,board)
        }
    }
  }

}

//---------------------------------------------------------------------------------------
//Normal mouse mode - click on things to do their usual actions, click and drag on
//pieces to move and attack.

case class NormalMouseMode(mouseState: MouseState) extends MouseMode {

  //Tolerance in seconds for double click
  val doubleClickTolerance = 0.40

  //When clicking and dragging for a movement, the path of that movement
  var path : List[Loc] = Nil

  //Double click implementation
  //First mousedown -> (target,time,false)
  //Second mousedown -> (target,time,true)
  //Second mouseup -> double click effect happens
  var doubleClickState: Option[(MouseTarget,Double,Boolean)] = None

  private def clearPath() = {
    path = Nil
  }

  private def movementActionsOfPath(piece: Piece): Option[PlayerAction] = {
    if(path.length <= 1) None
    else Some(Movements(List(Movement(piece.spec, path.toVector))))
  }

  //Current time in seconds
  private def getNow(): Double = {
    (new scala.scalajs.js.Date()).getTime() / 1000.0
  }

  private def doubleClickTimeOkay(prevTime: Double): Boolean = {
    prevTime + doubleClickTolerance >= getNow()
  }

  def makeActionId(): String = {
    mouseState.client.makeActionId()
  }

  def handleMouseDown(curTarget: MouseTarget, curLoc: Loc, game: Game, board: BoardState): Unit = {
    val _ = (curLoc,game,board)
    doubleClickState = doubleClickState match {
      case None => Some((curTarget,getNow(),false))
      case Some((prevTarget,prevTime,_)) =>
        if(curTarget == prevTarget && doubleClickTimeOkay(prevTime)) Some((curTarget,prevTime,true))
        else Some((curTarget,getNow(),false))
    }

    clearPath()
  }

  def handleMouseMove(dragTarget: MouseTarget, curTarget: MouseTarget, curLoc: Loc, game: Game, board: BoardState): Unit = {
    //Clear double click if mouse changes targets
    doubleClickState match {
      case None => ()
      case Some((prevTarget,_,_)) =>
        if(curTarget != prevTarget)
          doubleClickState = None
    }

    //Update path to be a shortest path from [dragTarget] to [curLoc] that
    //shares the longest prefix with the current [path].
    val dragPiece = dragTarget.findPiece(board)
    dragPiece match {
      case None => clearPath()
      case Some(dragPiece) =>
        if(dragPiece.side != board.side)
          clearPath()
        else {
          val newLegalMove = {
            val targetPiece = curTarget.findPiece(board)
            targetPiece match {
              case None =>
                //Ordinary move to location
                board.findLegalMove(dragPiece,pathBias=path) { loc => loc == curLoc }
              case Some(targetPiece) =>
                //Merge into friendly swarm
                if(targetPiece.side == dragPiece.side)
                  board.findLegalMove(dragPiece,pathBias=path) { loc => loc == curLoc }
                //Attack enemy piece
                else {
                  val dpStats = dragPiece.curStats(board)
                  val tpStats = targetPiece.curStats(board)
                  if(!board.canAttack(dpStats,attackerHasMoved=false,dragPiece.actState,tpStats))
                    None
                  else {
                    board.findLegalMove(dragPiece,pathBias=path) { loc =>
                      board.topology.distance(loc, curLoc) <= dpStats.attackRange &&
                      (loc == dragPiece.loc || !dpStats.isLumbering)
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

  def handleMouseUp(dragTarget: MouseTarget, curTarget: MouseTarget, curLoc: Loc, game: Game, board: BoardState, boardIdx: Int, ctrlPressed: Boolean): Unit = {
    val didDoubleClick = {
      doubleClickState match {
        case None => false
        case Some((prevTarget,prevTime,secondDown)) =>
          prevTarget == curTarget && doubleClickTimeOkay(prevTime) && secondDown
      }
    }

    //Branch based on what we selected on the mouseDown
    dragTarget match {
      case MouseNone | MouseTile(_) => ()

      case MouseEndTurn =>
        //Require mouse down and up on the same target
        if(curTarget == dragTarget)
          mouseState.client.doGameAction(SetBoardDone(boardIdx,!game.isBoardDone(boardIdx)))

      case MouseResignBoard =>
        //Require mouse down and up on the same target
        if(curTarget == dragTarget) {
          mouseState.client.showResignConfirm()
        }

      case MouseTech(techIdx) =>
        //Require mouse down and up on the same target
        if(curTarget == dragTarget) {
          val techState = game.techLine(techIdx)
          if(ctrlPressed) {
            (techState.level(game.curSide), techState.startingLevelThisTurn(game.curSide)) match {
              case (TechAcquired,TechAcquired) =>
                techState.tech match {
                  case PieceTech(pieceName) =>
                    mouseState.client.doActionOnCurBoard(BuyReinforcementUndo(pieceName,makeActionId()))
                }
              case _ =>
                mouseState.client.doGameAction(UndoTech(game.curSide,techIdx))
            }
          }
          else {
            techState.level(game.curSide) match {
              case TechLocked | TechUnlocked =>
                mouseState.client.doGameAction(PerformTech(game.curSide,techIdx))
              case TechAcquired =>
                techState.tech match {
                  case PieceTech(pieceName) =>
                    mouseState.client.doActionOnCurBoard(DoGeneralBoardAction(BuyReinforcement(pieceName),makeActionId()))
                }
            }
          }
        }

      case MouseDeadPiece(pieceSpec) =>
        if(ctrlPressed) {
          //Require mouse down and up on the same target
          if(curTarget == dragTarget) {
            mouseState.client.doActionOnCurBoard(LocalPieceUndo(pieceSpec,makeActionId()))
          }
        }

      case MouseReinforcement(pieceName,_) =>
        if(ctrlPressed) {
          //Require mouse down and up on the same target
          if(curTarget == dragTarget) {
            val pieceSpec = board.unsummonedThisTurn.reverse.findMap { case (pieceSpec,name,_) =>
              if(pieceName == name) Some(pieceSpec) else None
            }
            pieceSpec.foreach { pieceSpec =>
              mouseState.client.doActionOnCurBoard(LocalPieceUndo(pieceSpec,makeActionId()))
            }
          }
        }
        else {
          mouseState.client.doActionOnCurBoard(PlayerActions(List(Spawn(curLoc,pieceName)),makeActionId()))
        }

      case MousePiece(dragSpec) =>
        if(ctrlPressed) {
          //Require mouse down and up on the same target
          if(curTarget == dragTarget) {
            mouseState.client.doActionOnCurBoard(LocalPieceUndo(dragSpec,makeActionId()))
          }
        }
        else {
          board.findPiece(dragSpec) match {
            case None => ()
            case Some(piece) =>
              if(piece.side == game.curSide) {
                //Double-clicking on a piece activates its ability
                if(didDoubleClick) {
                  val pieceStats = piece.curStats(board)
                  val abilityNames = pieceStats.abilities.keys.toArray
                  if(abilityNames.length > 0) {
                    //TODO disambiguate which action if there's more than one?
                    val abilityName = abilityNames(0)
                    val ability = pieceStats.abilities(abilityName)
                    val spec = piece.spec
                    ability match {
                      case SuicideAbility | BlinkAbility | (_:SelfEnchantAbility) =>
                        val abilityActions = List(ActivateAbility(spec,abilityName,SpellOrAbilityTargets.none))
                        mouseState.client.doActionOnCurBoard(PlayerActions(abilityActions,makeActionId()))
                      case (_:TargetedAbility) =>
                        mouseState.mode = SelectTargetMouseMode(mouseState) { (target:MouseTarget) =>
                          target.findPiece(board) match {
                            case None => ()
                            case Some(targetPiece) =>
                              val abilityActions = List(ActivateAbility(spec,abilityName,SpellOrAbilityTargets.singlePiece(targetPiece.spec)))
                              mouseState.client.doActionOnCurBoard(PlayerActions(abilityActions,makeActionId()))
                          }
                        }
                    }
                  }
                }
                //Otherwise, normal click-and-drag
                else {
                  //Attack if enemy piece under the mouseUp
                  val attack: Option[PlayerAction] = {
                    curTarget.findPiece(board) match {
                      case None => None
                      case Some(other) =>
                        if(other.side == piece.side) None
                        else Some(Attack(piece.spec, other.spec))
                    }
                  }
                  //Move based on the path, if we have a nontrivial path
                  val movementFromPath: Option[PlayerAction] = movementActionsOfPath(piece)
                  //If the path is empty and the target is a tile one hex away from the piece's current location
                  //then attempt a move so that we can report an illegal move error as help
                  val movement = {
                    if(attack.isEmpty && movementFromPath.isEmpty) {
                      if(board.topology.distance(piece.loc,curLoc) == 1)
                        Some(Movements(List(Movement(piece.spec, Vector(piece.loc,curLoc)))))
                      else movementFromPath
                    }
                    else movementFromPath
                  }

                  val actions = List(movement,attack).flatten
                  if(actions.length > 0)
                    mouseState.client.doActionOnCurBoard(PlayerActions(actions,makeActionId()))
                }
              }
          }
        }
    }

    path = Nil

    if(didDoubleClick)
      doubleClickState = None
  }

}

//---------------------------------------------------------------------------------------
//Mouse mode for selecting a target of spell or ability

case class SelectTargetMouseMode(mouseState: MouseState)(f:MouseTarget => Unit) extends MouseMode {

  def handleMouseDown(curTarget: MouseTarget, curLoc: Loc, game: Game, board: BoardState): Unit = {
    val _ = (curTarget,curLoc,game,board)
  }

  def handleMouseMove(dragTarget: MouseTarget, curTarget: MouseTarget, curLoc: Loc, game: Game, board: BoardState): Unit = {
    val _ = (dragTarget,curTarget,curLoc,game,board)
  }

  def handleMouseUp(dragTarget: MouseTarget, curTarget: MouseTarget, curLoc: Loc, game: Game, board: BoardState, boardIdx: Int, ctrlPressed: Boolean): Unit = {
    val _ = (curLoc,game,board,boardIdx,ctrlPressed)

    //Restore normal mode
    mouseState.mode = NormalMouseMode(mouseState)

    //If the mouse up and mouse down are on the same location, we're good
    if(curTarget == dragTarget)
      f(curTarget)
  }
}

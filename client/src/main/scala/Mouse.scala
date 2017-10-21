package minionsgame.jsclient

import scala.util.{Try, Success, Failure}

import minionsgame.core._
import RichImplicits._

//A thing on the board that could possibly mouse-overed, clicked, on, selected, etc.
sealed trait MouseTarget {
  def findPiece(board: BoardState): Option[Piece] = {
    this match {
      case MouseNone => None
      case MouseSpellChoice(_,_) => None
      case MouseSpellHand(_,_,_) => None
      case MouseSpellPlayed(_,_,_,_) => None
      case MousePiece(spec,_) => board.findPiece(spec)
      case MouseTile(_) => None
      case MouseTech(_,_) => None
      case MouseReinforcement(_,_,_) => None
      case MouseDeadPiece(_,_) => None
      case MouseExtraTechAndSpell(_) => None
      case MouseEndTurn(_) => None
      case MouseNextBoard => None
      case MousePrevBoard => None
      case MouseResignBoard(_) => None
    }
  }
  def getLoc(): Option[Loc] = {
    this match {
      case MouseNone => None
      case MouseSpellChoice(_,loc) => Some(loc)
      case MouseSpellHand(_,_,loc) => Some(loc)
      case MouseSpellPlayed(_,_,_,loc) => Some(loc)
      case MousePiece(_,loc) => Some(loc)
      case MouseTile(loc) => Some(loc)
      case MouseTech(_,loc) => Some(loc)
      case MouseReinforcement(_,_,loc) => Some(loc)
      case MouseDeadPiece(_,loc) => Some(loc)
      case MouseExtraTechAndSpell(loc) => Some(loc)
      case MouseEndTurn(loc) => Some(loc)
      case MouseNextBoard => None
      case MousePrevBoard => None
      case MouseResignBoard(loc) => Some(loc)
    }
  }
}
case object MouseNone extends MouseTarget
case class MouseSpellChoice(idx: Int, loc: Loc) extends MouseTarget
case class MouseSpellHand(spellId: SpellId, side: Side, loc: Loc) extends MouseTarget
case class MouseSpellPlayed(spellId: SpellId, side: Side, targets: Option[SpellOrAbilityTargets], loc: Loc) extends MouseTarget
case class MousePiece(pieceSpec: PieceSpec, loc: Loc) extends MouseTarget
case class MouseTile(loc: Loc) extends MouseTarget
case class MouseTech(techIdx: Int, loc: Loc) extends MouseTarget
case class MouseReinforcement(pieceName: PieceName, side:Side, loc: Loc) extends MouseTarget
case class MouseDeadPiece(pieceSpec: PieceSpec, loc: Loc) extends MouseTarget
case class MouseExtraTechAndSpell(loc: Loc) extends MouseTarget
case class MouseEndTurn(loc: Loc) extends MouseTarget
case object MouseNextBoard extends MouseTarget
case object MousePrevBoard extends MouseTarget
case class MouseResignBoard(loc: Loc) extends MouseTarget

//Different modes the mouse can be in for selecting different things
sealed trait MouseMode {
  //Handlers in this mouse mode
  def handleMouseDown(curTarget: MouseTarget, game: Game, board: BoardState, undo: Boolean): Unit
  def handleMouseMove(dragTarget: MouseTarget, curTarget: MouseTarget, game: Game, board: BoardState): Unit
  def handleMouseUp(dragTarget: MouseTarget, curTarget: MouseTarget, game: Game, board: BoardState, boardIdx: Int, undo: Boolean): Unit
}

//---------------------------------------------------------------------------------------
//Current state of the mouse.

case class MouseState(val ourSide: Option[Side], val ui: UI, val client: Client)
{
  //The current target moused-over
  var hovered : MouseTarget = MouseNone
  //The target of the last mousedown, if the mouse is still held down.
  var dragTarget: MouseTarget = MouseNone

  //Last pixel location of the mouse, for refreshing the state in case the board changed
  var lastPixelLoc: Option[PixelLoc] = None

  // Are we in the middle of undoing something?
  var undoing: Boolean = false

  //Current mode that the mouse is operating in.
  var mode: MouseMode = NormalMouseMode(this)

  def clear() = {
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

  private def getTarget(pixelLoc: PixelLoc, game: Game, board: BoardState): MouseTarget = {
    val hexLoc = HexLoc.ofPixel(pixelLoc, Drawing.gridSize)
    for(component <- ui.clickableComponents) {
      val target = component.getMouseTarget(game,board,hexLoc)
      if(target != MouseNone)
        return target
    }
    MouseNone
  }

  def handleMouseDown(pixelLoc: PixelLoc, game: Game, board: BoardState, undo: Boolean) : Unit = {
    undoing = undo
    lastPixelLoc = Some(pixelLoc)
    ourSide match {
      case None => ()
      case Some(ourSide) =>
        if(ourSide == board.side && !client.gotFatalError) {
          val curTarget = getTarget(pixelLoc,game,board)
          dragTarget = curTarget

          mode.handleMouseDown(curTarget,game,board, undo)
        }
    }
  }

  def handleMouseUp(pixelLoc: PixelLoc, game: Game, board: BoardState, boardIdx: Int, undo: Boolean): Unit = {
    undoing = false
    lastPixelLoc = Some(pixelLoc)
    ourSide match {
      case None => ()
      case Some(ourSide) =>
        if(ourSide == board.side && !client.gotFatalError) {
          val curTarget = getTarget(pixelLoc,game,board)
          mode.handleMouseUp(dragTarget,curTarget,game,board,boardIdx,undo)
          dragTarget = MouseNone
        }
    }
  }

  def handleMouseMove(pixelLoc: PixelLoc, game: Game, board: BoardState) : Unit = {
    lastPixelLoc = Some(pixelLoc)

    val curTarget = getTarget(pixelLoc,game,board)
    hovered = curTarget

    ourSide match {
      case None => ()
      case Some(ourSide) =>
        if(ourSide == board.side && !client.gotFatalError) {
          mode.handleMouseMove(dragTarget,curTarget,game,board)
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

  //When clicking and dragging for a movement, the path of that movement, and the piece movements associated with it
  var path : List[Loc] = Nil
  var pathMovements : List[(PieceSpec,List[Loc])] = Nil

  //Double click implementation
  //First mousedown -> (target,time,false)
  //Second mousedown -> (target,time,true)
  //Second mouseup -> double click effect happens
  var doubleClickState: Option[(MouseTarget,Double,Boolean)] = None

  private def clearPath() = {
    path = Nil
    pathMovements = Nil
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

  def handleMouseDown(curTarget: MouseTarget, game: Game, board: BoardState, undo: Boolean): Unit = {
    val _ = (game,board,undo)
    doubleClickState = doubleClickState match {
      case None => Some((curTarget,getNow(),false))
      case Some((prevTarget,prevTime,_)) =>
        if(curTarget == prevTarget && doubleClickTimeOkay(prevTime)) Some((curTarget,prevTime,true))
        else Some((curTarget,getNow(),false))
    }

    clearPath()
  }

  def handleMouseMove(dragTarget: MouseTarget, curTarget: MouseTarget, game: Game, board: BoardState): Unit = {
    //Clear double click if mouse changes targets
    doubleClickState match {
      case None => ()
      case Some((prevTarget,_,_)) =>
        if(curTarget != prevTarget)
          doubleClickState = None
    }

    //Update path to be a shortest path from [dragTarget] to [curLoc] that
    //shares the longest prefix with the current [path].
    //(actually, not always shortest, see the case below where not mergable)
    val dragPiece = dragTarget.findPiece(board)
    val curLoc = curTarget.getLoc()
    (dragPiece, curLoc) match {
      case (None,_) | (_,None) => clearPath()
      case (Some(dragPiece), Some(curLoc)) =>
        if(dragPiece.side != board.side)
          clearPath()
        else {
          val newLegalMove = {
            //Truncate path to current loc, or extend by current loc, to use as the bias
            //for the next path
            val pathBias = {
              val idx = path.indexOf(curLoc)
              if(idx != -1) path.take(idx+1)
              else path :+ curLoc
            }

            val targetPiece = curTarget.findPiece(board)
            targetPiece match {
              case None =>
                //Ordinary move to location
                board.findLegalMove(dragPiece,pathBias=pathBias,allowNonMinimalBias=false) { case (loc,_) => loc == curLoc }
              case Some(targetPiece) =>
                //Merge into friendly swarm
                if(targetPiece.side == dragPiece.side) {
                  //If not mergable, then the user might be trying to swap, so preserve path as much as possible
                  //even if the path becomes nonminimal
                  val dpStats = dragPiece.curStats(board)
                  val tpStats = targetPiece.curStats(board)
                  val allowNonMinimalBias = {
                    !board.canSwarmTogether(dpStats,tpStats) || dpStats.swarmMax < board.pieces(curLoc).length + 1
                  }
                  board.findLegalMove(dragPiece,pathBias=pathBias,allowNonMinimalBias) { case (loc,_) => loc == curLoc }
                }
                //Attack enemy piece
                else {
                  val dpStats = dragPiece.curStats(board)
                  val tpStats = targetPiece.curStats(board)
                  if(!board.canAttack(dpStats,attackerHasMoved=false,dragPiece.actState,tpStats))
                    None
                  else {
                    val attackRange = { if(tpStats.isFlying) dpStats.attackRangeVsFlying else dpStats.attackRange }
                    board.findLegalMove(dragPiece,pathBias=pathBias,allowNonMinimalBias=false) { case (loc,shuffles) =>
                      shuffles.length <= 1 &&
                      board.topology.distance(loc, curLoc) <= attackRange &&
                      (loc == dragPiece.loc || !dpStats.isLumbering)
                    }
                  }
                }
            }
          }
          newLegalMove match {
            case None => ()
            case Some((newPath,newPathMovements)) =>
              path = newPath
              pathMovements = newPathMovements
          }
        }
    }
  }

  //The action to perform on mouseup from dragging the given piece
  def dragPieceMouseUpActions(curTarget: MouseTarget, curLoc: Loc, piece: Piece, board: BoardState): List[PlayerAction] = {
    //Attack if enemy piece under the mouseUp
    val attack: Option[PlayerAction] = {
      curTarget.findPiece(board) match {
        case None => None
        case Some(other) =>
          if(other.side == piece.side) None
          else Some(Attack(piece.spec, other.spec))
      }
    }
    val movementFromPath: Option[PlayerAction] = {
      //Move based on the path, if we have a nontrivial path and either the final location
      //is under the mouse OR we're attacking.
      if(attack.nonEmpty || (path.length >= 1 && path.last == curLoc)) {
        //Move based on path only if attempting to attack
        if(attack.nonEmpty) {
          if(path.length <= 1) None
          else Some(Movements(List(Movement(piece.spec, path.toVector))))
        }
        //Do fancy movement that potentially includes swaps
        else {
          val filteredPathMovements = pathMovements.filter { case (_,path) => path.length > 1 }
          if(filteredPathMovements.isEmpty) None
          else Some(Movements(filteredPathMovements.map { case (pieceSpec,path) => Movement(pieceSpec,path.toVector) }))
        }
      }
      //Use teleporter
      else if(board.tiles(piece.loc).terrain == Teleporter)
        Some(Teleport(piece.spec, piece.loc, curLoc))
      else
        None
    }
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
    List(movement,attack).flatten
  }

  def handleMouseUp(dragTarget: MouseTarget, curTarget: MouseTarget, game: Game, board: BoardState, boardIdx: Int, undo: Boolean): Unit = {
    val didDoubleClick = {
      doubleClickState match {
        case None => false
        case Some((prevTarget,prevTime,secondDown)) =>
          prevTarget == curTarget && doubleClickTimeOkay(prevTime) && secondDown
      }
    }

    //Branch based on what we selected on the mouseDown
    dragTarget match {
      case MouseNone => ()

      case MouseSpellChoice(idx, _) =>
        val (spellId, _) = game.resolveSpellChoice(idx, mouseState.client.ourSide)
        spellId match {
          case None => ()
          case Some(spellId) =>
            if(undo) {
              mouseState.client.doActionOnCurBoard(GainSpellUndo(spellId, makeActionId()))
            } else {
              mouseState.client.doActionOnCurBoard(DoGeneralBoardAction(GainSpell(spellId),makeActionId()))
            }
        }

      case MouseSpellHand(spellId,side,_) =>
        if(side == game.curSide) {
          if(undo) {
            //Require mouse down and up on the same target
            if(curTarget == dragTarget) {
              mouseState.client.doActionOnCurBoard(GainSpellUndo(spellId, makeActionId()))
            }
          } else {
            if(curTarget == dragTarget && didDoubleClick) {
                mouseState.client.doActionOnCurBoard(PlayerActions(List(DiscardSpell(spellId)),makeActionId()))
            } else {
              curTarget.getLoc().foreach { loc =>
                val target0 =
                  curTarget.findPiece(board) match {
                    case None => PieceSpec.none
                    case Some(piece) => piece.spec
                  }
                val targets = SpellOrAbilityTargets(target0, PieceSpec.none, loc, Loc(-1, -1))
                mouseState.client.doActionOnCurBoard(PlayerActions(List(PlaySpell(spellId, targets)),makeActionId()))
              }
            }
          }
        }

      case MouseSpellPlayed(spellId,side,_,_) =>
        if(undo && side == game.curSide && curTarget == dragTarget) {
          mouseState.client.doActionOnCurBoard(SpellUndo(spellId, makeActionId()))
        }

      case MouseExtraTechAndSpell(_) =>
        if(curTarget == dragTarget) {
          if(undo) {
            mouseState.client.doGameAction(UnbuyExtraTechAndSpell(game.curSide))
          } else {
            mouseState.client.doGameAction(BuyExtraTechAndSpell(game.curSide))
          }
        }

      case MouseEndTurn(_) =>
        //Require mouse down and up on the same target
        if(curTarget == dragTarget)
          mouseState.client.doGameAction(SetBoardDone(boardIdx,!game.isBoardDone(boardIdx)))

      case MouseResignBoard(_) =>
        //Require mouse down and up on the same target
        if(curTarget == dragTarget) {
          mouseState.client.showResignConfirm()
        }

      case MouseNextBoard =>
        //Require mouse down and up on the same target
        if(curTarget == dragTarget) {
          val client = mouseState.client
          if(client.curBoardIdx < client.numBoards - 1) {
            mouseState.clear()
            client.curBoardIdx += 1
            client.draw()
          }
        }

      case MousePrevBoard =>
        if(curTarget == dragTarget) {
          val client = mouseState.client
          if(client.curBoardIdx > 0) {
            mouseState.clear()
            client.curBoardIdx -= 1
            client.draw()
          }
        }

      case MouseTech(techIdx,_) =>
        //Require mouse down and up on the same target
        if(curTarget == dragTarget) {
          val techState = game.techLine(techIdx)
          if(undo) {
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

      case MouseDeadPiece(pieceSpec,_) =>
        if(undo) {
          //Require mouse down and up on the same target
          if(curTarget == dragTarget) {
            mouseState.client.doActionOnCurBoard(LocalPieceUndo(pieceSpec,makeActionId()))
          }
        }

      case MouseReinforcement(pieceName,side,_) =>
        if(side == game.curSide) {
          if(undo) {
            //Require mouse down and up on the same target
            if(curTarget == dragTarget) {
              val pieceSpec = board.unsummonedThisTurn.reverse.findMap { case (pieceSpec,name,_) =>
                if(pieceName == name) Some(pieceSpec) else None
              }
              pieceSpec match {
                case Some(pieceSpec) =>
                  mouseState.client.doActionOnCurBoard(LocalPieceUndo(pieceSpec,makeActionId()))
                case None =>
                  mouseState.client.doActionOnCurBoard(BuyReinforcementUndo(pieceName,makeActionId()))
              }
            }
          }
          else {
            curTarget.getLoc().foreach { curLoc =>
              mouseState.client.doActionOnCurBoard(PlayerActions(List(Spawn(curLoc,pieceName)),makeActionId()))
            }
          }
        }

      case MouseTile(loc) =>
        if(undo) ()
        else {
          board.tiles(loc).terrain match {
            case Wall | Ground | Water | Graveyard | SorceryNode | Teleporter | StartHex(_) => ()
            case Spawner(_) =>
              mouseState.client.doActionOnCurBoard(PlayerActions(List(ActivateTile(loc)),makeActionId()))
          }
        }

      case MousePiece(dragSpec,_) =>
        if(undo) {
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
                      case SuicideAbility | BlinkAbility | KillAdjacentAbility | (_:SelfEnchantAbility) =>
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
                  curTarget.getLoc().foreach { curLoc =>
                    val actions = dragPieceMouseUpActions(curTarget, curLoc, piece, board)
                    if(actions.length > 0)
                      mouseState.client.doActionOnCurBoard(PlayerActions(actions,makeActionId()))
                  }
                }
              }
          }
        }
    }

    path = Nil
    pathMovements = Nil

    if(didDoubleClick)
      doubleClickState = None
  }

}

//---------------------------------------------------------------------------------------
//Mouse mode for selecting a target of spell or ability

case class SelectTargetMouseMode(mouseState: MouseState)(f:MouseTarget => Unit) extends MouseMode {

  def handleMouseDown(curTarget: MouseTarget, game: Game, board: BoardState, undo: Boolean): Unit = {
    val _ = (curTarget,game,board, undo)
  }

  def handleMouseMove(dragTarget: MouseTarget, curTarget: MouseTarget, game: Game, board: BoardState): Unit = {
    val _ = (dragTarget,curTarget,game,board)
  }

  def handleMouseUp(dragTarget: MouseTarget, curTarget: MouseTarget, game: Game, board: BoardState, boardIdx: Int, undo: Boolean): Unit = {
    val _ = (game,board,boardIdx,undo)

    //Restore normal mode
    mouseState.mode = NormalMouseMode(mouseState)

    //If the mouse up and mouse down are on the same location, we're good
    if(curTarget == dragTarget)
      f(curTarget)
  }
}

package minionsgame.core

import scala.util.{Try,Success,Failure}
import scala.collection.immutable.Vector

import RichImplicits._

/**
  * The second layer of the board implementation. See BoardState.scala for the more primitive layer below.
  * Actions and semantics on this layer are more at the level of the UI, rather than the direct legal actions in the game.
  * In particular, this layer implements undo/redo and action reordering that make the UI friendly:
  *
  * - Spawn PlayerActions are reordered to happen after all other PlayerActions. This allows us to legality-check everything
  *   AS IF all spawns happened at the end of the turn in a distinct "spawn phase" as required by the technical rules of minions,
  *   and yet permits us to have a UI that in practice allows users to interleave spawns and other actions.
  *
  * - Local undos allow a user to specify a piece and undo all actions involving that piece this turn. We do this simply by stripping
  *   out all actions involving the piece, and legality-checking the remaining sequence of actions, tossing out any that don't legality check.
  *
  * In general, we deal with any dependency conflicts from reordering of actions by throwing out actions that became illegal.
  *
  * As a slight detail, GeneralBoardActions require communication with the outside world when doing them or attempting to undo or redo them.
  * Doing or redoing requires claiming a shared resource (like spending mana to buy a unit), undoing requires informing the broader game
  * the resource is available again (like regaining the mana after undoing the purchase).
  * We handle this by exposing functions here to allow users to determine what is about to get undone or redone on an undo or redo,
  * namely prevAction and nextAction.
  * It's up to the user of Board to do the necessary work here.
  *
  */


/** BoardAction:
  * A single UI/user action that can be made on a board.
  * These are the objects that conceptually form a stack that global undo/redo operates on - yes, this means that local undos
  * are treated the same way as a group of PlayerActions from the UI perspective - and the local undo itself can be undone/redone.
  * The actionId values are simply strings intended to disambiguate different actions, so that when we merge and dedup action sequences
  * we can tell which actions were actually distinct.
  */
sealed trait BoardAction
case class PlayerActions(actions: List[PlayerAction], actionId: String) extends BoardAction
case class DoGeneralBoardAction(action: GeneralBoardAction, actionId: String) extends BoardAction

//Undo the most recent List[PlayerAction] involving this piece.
case class LocalPieceUndo(pieceSpec: PieceSpec, actionId: String) extends BoardAction
//Undo the most recent List[PlayerAction] involving this spell.
case class SpellUndo(spellId: Int, actionId: String) extends BoardAction
//Undo the most recent GeneralBoardAction buying this reinforcement.
case class BuyReinforcementUndo(pieceName: PieceName, actionId: String) extends BoardAction


//Pairs board states together with the legal history of actions that generated those states, after reordering of actions.
//A new BoardHistory is created after each Action.
case class BoardHistory(
  //State of the board after applying moving/attacking actions, so far
  val moveAttackState: BoardState,
  //State of the board after applying spawning actions, so far
  val spawnState: BoardState,

  //Moving/attacking actions taken by the side to move this turn
  val moveAttackActionsThisTurn: Vector[List[PlayerAction]],
  //Spawning actions taken by the side to move this turn
  val spawnActionsThisTurn: Vector[List[PlayerAction]],
  //GeneralBoard actions taken by the side to move this turn
  val generalBoardActionsThisTurn: Vector[GeneralBoardAction]
)

object BoardHistory {
  def initial(state: BoardState) = BoardHistory(
    moveAttackState = state.copy(),
    spawnState = state.copy(),
    moveAttackActionsThisTurn = Vector(),
    spawnActionsThisTurn = Vector(),
    generalBoardActionsThisTurn = Vector()
  )
}

case class BoardSummary(
  val initialStateThisTurn: BoardState,
  val actionsThisTurn: Vector[BoardAction]
)

object Board {
  def create(initialState: BoardState): Board = {
    new Board(
      initialState = initialState.copy(),
      initialStateThisTurn = initialState.copy(),
      actionsThisTurn = Vector(),
      history = BoardHistory.initial(initialState),
      actionsPrevTurns = Vector(),
      playerGeneralBoardActionsPrevTurns = Vector()
    )
  }

  def ofSummary(summary: BoardSummary): Board = {
    val board = create(summary.initialStateThisTurn)
    summary.actionsThisTurn.foreach { action =>
      board.doAction(action).get
    }
    board
  }
}

class Board private (
  //The board state at the start of everything
  val initialState: BoardState,
  //The board history at the start of this turn
  var initialStateThisTurn: BoardState,

  //actionsThisTurn is defined in a potentially larger range if undos have happened, as it stores the redo history.
  private var actionsThisTurn: Vector[BoardAction],
  private var history: BoardHistory,

  //TODO these don't take into account board resets properly for being able to reconstruct the game history
  //Accumulates actionsThisTurn at the end of each turn
  private var actionsPrevTurns: Vector[Vector[BoardAction]],
  //Actions over the history of the board over prior turns, at the internal rearranging level, rather than the UI level.
  private var playerGeneralBoardActionsPrevTurns: Vector[(Vector[List[PlayerAction]],Vector[GeneralBoardAction])]
) {
  //Copies the board (but not the Boardstates within, since we never modify those directly)
  def copy(): Board = {
    val newBoard = new Board(
      initialState = initialState,
      initialStateThisTurn = initialStateThisTurn,
      actionsThisTurn = actionsThisTurn,
      history = history,
      actionsPrevTurns = actionsPrevTurns,
      playerGeneralBoardActionsPrevTurns = playerGeneralBoardActionsPrevTurns
    )
    newBoard
  }

  //Users should NOT modify the BoardState returned by this function!
  def curState(): BoardState = {
    history.spawnState
  }

  //Find the set of actions that would be undone by a local undo for pieceSpec
  def findLocalPieceUndoActions(pieceSpec: PieceSpec): Option[List[PlayerAction]] = {
    findLastMatch(history.spawnActionsThisTurn) { playerActions => playerActions.exists { _.involvesPiece(pieceSpec) } } match {
      case Some(actions) => Some(actions)
      case None =>
        findLastMatch(history.moveAttackActionsThisTurn) { playerActions => playerActions.exists { _.involvesPiece(pieceSpec) } }
    }
  }

  def tryLegality(action: BoardAction): Try[Unit] = {
    action match {
      case PlayerActions(actions,_) => curState().tryLegality(actions)
      case DoGeneralBoardAction(action,_) => curState().tryGeneralLegality(action)
      case LocalPieceUndo(pieceSpec,_) =>
        val anyActionInvolvesPiece = {
          history.moveAttackActionsThisTurn.exists { playerActions => playerActions.exists { _.involvesPiece(pieceSpec) } } ||
          history.spawnActionsThisTurn.exists { playerActions => playerActions.exists { _.involvesPiece(pieceSpec) } }
        }
        if(!anyActionInvolvesPiece)
          Failure(new Exception("Cannot undo actions for a piece that hasn't acted"))
        else
          Success(())

      case SpellUndo(spellId,_) =>
        val anyActionInvolvesSpell = {
          history.moveAttackActionsThisTurn.exists { playerActions => playerActions.exists { _.involvesSpell(spellId) } } ||
          history.spawnActionsThisTurn.exists { playerActions => playerActions.exists { _.involvesSpell(spellId) } }
        }
        if(!anyActionInvolvesSpell)
          Failure(new Exception("Cannot undo actions for a spell that wasn't played"))
        else
          Success(())

      case BuyReinforcementUndo(pieceName,_) =>
        if(!Units.pieceMap.contains(pieceName))
          Failure(new Exception("Trying to undo buying reinforcement piece with unknown name: " + pieceName))
        else if(!history.generalBoardActionsThisTurn.exists { generalAction => generalAction.involvesBuyPiece(pieceName) })
          Failure(new Exception("Cannot undo buying a piece that was not bought this turn"))
        else
          Success(())
    }
  }

  def findLastMatch[T](vec: Vector[T])(f: T => Boolean): Option[T] = {
    val index = vec.lastIndexWhere(f)
    if(index < 0) //No match
      None
    else Some(vec(index))
  }

  def dropLastMatch[T](vec: Vector[T])(f: T => Boolean): Vector[T] = {
    val index = vec.lastIndexWhere(f)
    if(index < 0) //No match
      vec
    else vec.take(index) ++ vec.drop(index+1)
  }

  def reapplyLegal[T](vec: Vector[T])(tryAction: T => Try[Unit]): Vector[T] = {
    vec.filter { action =>
      tryAction(action) match {
        case Success(()) => true
        case Failure(_) => false
      }
    }
  }

  //Due to action reordering, might also report some actions illegal that aren't reported as illegal by tryLegality,
  //but this should be extremely rare.
  def doAction(action: BoardAction): Try[Unit] = {
    tryLegality(action) match {
      case Failure(err) => Failure(err)
      case Success(()) => Try {
        val newHistory = action match {
          case PlayerActions(playerActions,_) =>
            val newMoveAttackState = history.moveAttackState.copy()

            //Try all the move/attack actions that are legal now in a row. Delay other actions to the spawn phase.
            var moveAttackActionsRev: List[PlayerAction] = List()
            var delayedToSpawnRev: List[PlayerAction] = List()
            playerActions.foreach { playerAction =>
              playerAction match {
                case Movements(_) | Attack(_,_) =>
                  //If move/attacks fail, then they're flat-out illegal
                  newMoveAttackState.doAction(playerAction).get
                  moveAttackActionsRev = playerAction :: moveAttackActionsRev
                case Spawn(_,_) =>
                  delayedToSpawnRev = playerAction :: delayedToSpawnRev
                case ActivateAbility(_,_,_) | PlaySpell(_,_) | DiscardSpell(_) =>
                  //When spells or abilities fail, it may be because they are targeting units only placed during spawn
                  newMoveAttackState.doAction(playerAction) match {
                    case Success(()) => moveAttackActionsRev = playerAction :: moveAttackActionsRev
                    case Failure(_) => delayedToSpawnRev = playerAction :: delayedToSpawnRev
                  }
              }
            }

            //Reapply all the spawn actions so far
            val newSpawnState = newMoveAttackState.copy()
            reapplyLegal(history.spawnActionsThisTurn) { playerActions =>
              newSpawnState.doActions(playerActions)
            }

            //And now apply all the deferred actions
            val spawnActions = delayedToSpawnRev.reverse
            newSpawnState.doActions(spawnActions).get

            BoardHistory(
              moveAttackState = newMoveAttackState,
              spawnState = newSpawnState,
              moveAttackActionsThisTurn = history.moveAttackActionsThisTurn :+ moveAttackActionsRev.reverse,
              spawnActionsThisTurn = history.spawnActionsThisTurn :+ spawnActions,
              generalBoardActionsThisTurn = history.generalBoardActionsThisTurn
            )
          case DoGeneralBoardAction(generalBoardAction,_) =>
            val newMoveAttackState = history.moveAttackState.copy()
            val newSpawnState = history.spawnState.copy()
            newMoveAttackState.doGeneralBoardAction(generalBoardAction)
            newSpawnState.doGeneralBoardAction(generalBoardAction)
            BoardHistory(
              moveAttackState = newMoveAttackState,
              spawnState = newSpawnState,
              moveAttackActionsThisTurn = history.moveAttackActionsThisTurn,
              spawnActionsThisTurn = history.spawnActionsThisTurn,
              generalBoardActionsThisTurn = history.generalBoardActionsThisTurn :+ generalBoardAction
            )
          case LocalPieceUndo(pieceSpec,_) =>
            val newMoveAttackState = initialStateThisTurn.copy()
            //Reapply all generalBoard actions
            //Note that this relies on the invariant mentioned at the top of BoardState.scala - that reordering generalBoard actions
            //to come before player actions never changes the legality of the total move sequence or the final result of that sequence.
            history.generalBoardActionsThisTurn.foreach { generalBoardAction =>
              newMoveAttackState.doGeneralBoardAction(generalBoardAction)
            }

            //Drop the most recent action involving the piece
            val keptSpawnActionsThisTurn =
              dropLastMatch(history.spawnActionsThisTurn) { playerActions => playerActions.exists { _.involvesPiece(pieceSpec) } }
            val keptMoveAttackActionsThisTurn = {
              if(keptSpawnActionsThisTurn.length != history.spawnActionsThisTurn.length)
                history.moveAttackActionsThisTurn
              else
                dropLastMatch(history.moveAttackActionsThisTurn) { playerActions => playerActions.exists { _.involvesPiece(pieceSpec) } }
            }

            //And then reapply the actions, dropping any illegal ones.
            val newMoveAttackActionsThisTurn =
              reapplyLegal(keptMoveAttackActionsThisTurn) { playerActions => newMoveAttackState.doActions(playerActions) }
            val newSpawnState = newMoveAttackState.copy()
            val newSpawnActionsThisTurn =
              reapplyLegal(keptSpawnActionsThisTurn) { playerActions => newSpawnState.doActions(playerActions) }
            BoardHistory(
              moveAttackState = newMoveAttackState,
              spawnState = newSpawnState,
              moveAttackActionsThisTurn = newMoveAttackActionsThisTurn,
              spawnActionsThisTurn = newSpawnActionsThisTurn,
              generalBoardActionsThisTurn = history.generalBoardActionsThisTurn
            )

          case SpellUndo(spellId,_) =>
            val newMoveAttackState = initialStateThisTurn.copy()
            //Reapply all generalBoard actions
            //Note that this relies on the invariant mentioned at the top of BoardState.scala - that reordering generalBoard actions
            //to come before player actions never changes the legality of the total move sequence or the final result of that sequence.
            history.generalBoardActionsThisTurn.foreach { generalBoardAction =>
              newMoveAttackState.doGeneralBoardAction(generalBoardAction)
            }

            //Drop the most recent action involving the spell
            val keptSpawnActionsThisTurn =
              dropLastMatch(history.spawnActionsThisTurn) { playerActions => playerActions.exists { _.involvesSpell(spellId) } }
            val keptMoveAttackActionsThisTurn = {
              if(keptSpawnActionsThisTurn.length != history.spawnActionsThisTurn.length)
                history.moveAttackActionsThisTurn
              else
                dropLastMatch(history.moveAttackActionsThisTurn) { playerActions => playerActions.exists { _.involvesSpell(spellId) } }
            }

            //And then reapply the actions, dropping any illegal ones.
            val newMoveAttackActionsThisTurn =
              reapplyLegal(keptMoveAttackActionsThisTurn) { playerActions => newMoveAttackState.doActions(playerActions) }
            val newSpawnState = newMoveAttackState.copy()
            val newSpawnActionsThisTurn =
              reapplyLegal(keptSpawnActionsThisTurn) { playerActions => newSpawnState.doActions(playerActions) }
            BoardHistory(
              moveAttackState = newMoveAttackState,
              spawnState = newSpawnState,
              moveAttackActionsThisTurn = newMoveAttackActionsThisTurn,
              spawnActionsThisTurn = newSpawnActionsThisTurn,
              generalBoardActionsThisTurn = history.generalBoardActionsThisTurn
            )

          case BuyReinforcementUndo(pieceName,_) =>
            val newMoveAttackState = initialStateThisTurn.copy()
            //Reapply all generalBoard actions, after filtering
            val keptGeneralActionsThisTurn =
              dropLastMatch(history.generalBoardActionsThisTurn) { generalBoardAction =>
                generalBoardAction.involvesBuyPiece(pieceName)
              }

            //General actions cannot be made illegal via one another
            keptGeneralActionsThisTurn.foreach { generalBoardAction =>
              newMoveAttackState.doGeneralBoardAction(generalBoardAction)
            }

            val newMoveAttackActionsThisTurn =
              reapplyLegal(history.moveAttackActionsThisTurn) { playerActions => newMoveAttackState.doActions(playerActions) }
            val newSpawnState = newMoveAttackState.copy()
            val newSpawnActionsThisTurn =
              reapplyLegal(history.spawnActionsThisTurn) { playerActions => newSpawnState.doActions(playerActions) }

            BoardHistory(
              moveAttackState = newMoveAttackState,
              spawnState = newSpawnState,
              moveAttackActionsThisTurn = newMoveAttackActionsThisTurn,
              spawnActionsThisTurn = newSpawnActionsThisTurn,
              generalBoardActionsThisTurn = keptGeneralActionsThisTurn
            )
        }

        actionsThisTurn = actionsThisTurn :+ action
        history = newHistory
        ()
      }
    }
  }

  //End the current turn and begin the next turn.
  def endTurn(): Unit = {
    initialStateThisTurn = history.spawnState.copy()
    initialStateThisTurn.endTurn()
    actionsPrevTurns = actionsPrevTurns :+ actionsThisTurn
    actionsThisTurn = Vector()
    history = BoardHistory.initial(initialStateThisTurn)

    val playerGeneralBoardActions = (
      history.moveAttackActionsThisTurn ++ history.spawnActionsThisTurn,
      history.generalBoardActionsThisTurn
    )
    playerGeneralBoardActionsPrevTurns = playerGeneralBoardActionsPrevTurns :+ playerGeneralBoardActions
  }

  //Reset the board to the starting position
  def resetBoard(necroNames: SideArray[PieceName]): Unit = {
    initialStateThisTurn = history.spawnState.copy()
    initialStateThisTurn.resetBoard(necroNames)
    //TODO loses history information for recording purposes into playerGeneralBoardActionsPrevTurns
    //on endTurn()
    history = BoardHistory.initial(initialStateThisTurn)
  }

  def toSummary(): BoardSummary = {
    BoardSummary(
      initialStateThisTurn.copy(),
      actionsThisTurn
    )
  }
}

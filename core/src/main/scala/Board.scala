import scala.util.{Try,Success,Failure}
import RichImplicits._
import scala.collection.immutable.Vector

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
  * As a slight detail, GeneralActions require communication with the outside world when doing them or attempting to undo or redo them.
  * Doing or redoing requires claiming a shared resource (like spending mana to buy a unit), undoing requires informing the broader game
  * the resource is available again (like regaining the mana after undoing the purchase).
  * We handle this by exposing functions here to allow users to determine what is about to get undone or redone on an undo or redo.
  * It's up to the user of Board to do the necessary work here.
  *
  */


/** Action:
  * A single UI/user action that can be made, undone, and redone.
  * These are the objects that conceptually form a stack that global undo/redo operates on - yes, this means that local undos
  * are treated the same way as a group of PlayerActions from the UI perspective - and the local undo itself can be undone/redone.
  */
sealed trait Action
case class PlayerActions(actions: List[PlayerAction]) extends Action
case class GeneralAction(action: GeneralAction) extends Action
case class LocalUndo(pieceSpec: PieceSpec) extends Action

object Board {
  def create(tiles: Plane[Tile]): Board = {
    val initialState = BoardState.create(tiles)
    new Board(
      initialState = initialState,
      initialStateThisTurn = initialState,
      statesThisTurn = Vector(initialState),
      actionsThisTurn = Vector(),
      curIdx = 0,
      actionsPrevTurns = Vector(),
    )
  }
}

class Board private (
  //The board state at the start of everything
  val initialState: BoardState,
  //The board state at the start of this turn
  var initialStateThisTurn: BoardState,

  //Actions and states so far this turn
  //statesThisTurn is defined in the range [0,curIdx].
  //actionsThisTurn is defined in a potentially larger range if undos have happened, as it stores the redo history.
  //If no redos have happened, actionsThisTurn will be one shorter than statesThisTurn (because the n-1th action results in the nth state).
  var actionsThisTurn: Vector[Action],
  var statesThisTurn: Vector[BoardState],

  //Current index of action (decrements on undo, increments on redo)
  var curIdx: Int,

  //Accumulates actionsThisTurn at the end of each turn
  var actionsPrevTurns: Vector[Vector[Action]]
) {

  def curState(): BoardState = {
    statesThisTurn(curIdx)
  }

  def tryLegality(action: Action): Try[Unit] = {
    action match {
      case PlayerActions(actions) => curState().tryLegality(actions)
      case GeneralAction(_) => Success(())
      case LocalUndo(pieceSpec) =>
        if(curState().pieceExists(pieceSpec))
          Success(())
        else
          Failure(new Exception("Cannot local-undo for a piece that doesn't exist"))
    }
  }

  def doAction(action: Action): Try[Unit] = {
    action match {
      case PlayerActions(actions) =>
        val newState = curState().copy()
        //TODO don't forget spawn reordering
        newState.doActionsStopOnIllegal(actionList) match {
          case Failure(err) => Failure(err)
          case Success(()) =>
            //TODO
        }
      case GeneralAction(_) => //TODO
      case LocalUndo(pieceSpec) =>
        //TODO
    }
  }

}

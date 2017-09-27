package minionsgame.core

import scala.util.{Try,Success,Failure}
import scala.collection.immutable.Vector

import RichImplicits._

/**
  * The board state and various (often mutable) data structures and types relating directly to it,
  * for a single board.
  *
  * This is the base layer of the board implementation - interacting with the board is done via
  * PlayerAction, which represents a basic action performable in the game according to the rules.
  * Note that the model presented here in BoardState is that move/attack/spawn actions are arbitrarily
  * interleavable. Enforcing of a separation of spawn actions into a separate phase at the end of
  * the turn is done by action reordering in Board.scala, the next layer up. This allows the user
  * to input actions in arbitrary order, yet have the result be consistent with having a spawn phase.
  *
  * Also implemented are GeneralBoardActions - gaining spells and buying pieces.
  * These are recorded separately from PlayerActions. This is because they aren't subject to the same
  * undo/redo/reordering logic that player actions are subject to, since they involve interaction with the broader game.
  *
  * Given generalBoard action G and player action P, an invariant that should hold is:
  * - If P followed by G is legal, then G followed by P must also be legal and lead to the same board state.
  * - Also generalBoardActions commute with each other.
  *
  * See Board.scala for the next layer up in the board implementation stack.
  */

/**
  * PieceSpec:
  * Identifies a single piece on the board uniquely on a given turn, and in a way that
  * should be robust to reordering of actions within that turn.
  */
sealed trait PieceSpec
//Piece was present at start of turn and has this id
case class StartedTurnWithID(pieceId: Int) extends PieceSpec
//Piece spawned this turn - (piece's name, spawning location, nth spawn at that location)
case class SpawnedThisTurn(pieceName: PieceName, spawnLoc: Loc, nthAtLoc: Int) extends PieceSpec
with Ordered[SpawnedThisTurn] {
  def compare(that: SpawnedThisTurn): Int =
    Ordering[(PieceName,Int,Int,Int)].compare(
        (pieceName,spawnLoc.x,spawnLoc.y,nthAtLoc),
        (that.pieceName,that.spawnLoc.x,that.spawnLoc.y,that.nthAtLoc)
    )
}

/**
  * PlayerAction:
  * A single action taken by a player. Does not include "GeneralBoard" actions of gaining spells or buying pieces.
  * PlayerActions are immutable and their data should be independent of any of the mutable types
  * on the board. That is, it should be meaningful to ask whether an action would be legal
  * on a different board, or if the order of actions were rearranged, etc.
  *
  * Notes:
  * Movements: movements is a list because we need to support piece swaps, triangular rotations, etc.
  * Attack: self-explanatory
  * Spawn: self-explanatory
  * SpellsAndAbilities: spellsAndAbilities is a list because we need to support playing sorceries and cantrips together.
  */
sealed trait PlayerAction {
  //Does this action directly involve pieceSpec?
  def involvesPiece(pieceSpec: PieceSpec): Boolean = {
    this match {
      case Movements(movements) =>
        movements.exists { case Movement(pSpec, _) => pieceSpec == pSpec }
      case Attack(aSpec,tSpec) => pieceSpec == aSpec || pieceSpec == tSpec
      case Spawn(spawnLoc,spawnName) =>
        pieceSpec match {
          case StartedTurnWithID(_) => false
          //Note that we don't check nthAtLoc - this means local undo will undo all units spawned on that hex with that name.
          case SpawnedThisTurn(pieceName,sLoc,_) => pieceName == spawnName && sLoc == spawnLoc
        }
      case SpellsAndAbilities(spellsAndAbilities) =>
        spellsAndAbilities.exists { played =>
          played.pieceSpecAndKey.exists { case (pSpec,_) => pieceSpec == pSpec } ||
          pieceSpec == played.target0 ||
          pieceSpec == played.target1
        }
    }
  }
}

case class Movements(movements: List[Movement]) extends PlayerAction
case class Attack(attackerSpec: PieceSpec, targetSpec: PieceSpec) extends PlayerAction
case class Spawn(spawnLoc: Loc, pieceName: PieceName) extends PlayerAction
case class SpellsAndAbilities(spellsAndAbilities: List[PlayedSpellOrAbility]) extends PlayerAction

//Note: path should contain both the start and ending location
case class Movement(pieceSpec: PieceSpec, path: Vector[Loc])

//Data for a played spell or piece ability.
//Since different spells and abilities affect different things and have different numbers
//of targets, not all fields may be applicable.
case class PlayedSpellOrAbility(
  val pieceSpecAndKey: Option[(PieceSpec,String)], //Some if it was a piece, (pieceId, key of ability)
  val side: Side,
  val spellId: Option[Int], //Some if it was a spell
  val target0: PieceSpec,
  val target1: PieceSpec,
  val loc0: Loc,
  //Spell ids that were discarded to power this spell or ability, if it was a sorcery
  val discardingId: Option[Int]
)


//TODO think about how to manage the opposing side not being able to see what spells you own...

/** GeneralBoardAction:
  * Actions relating to this board that involve interaction with the broader game (a shared spell pool, a shared mana pool).
  * These are NOT part of the normal action stack.
  *
  * Requirement: spellId should be a unique identifier for a particular spell card. Users of BoardState should ensure that this is the case.
  */
sealed trait GeneralBoardAction {
  //Does this action involve buying the named piece?
  def involvesBuyPiece(pieceName: PieceName): Boolean = {
    this match {
      case BuyReinforcement(name) => pieceName == name
      case GainSpell(_) | RevealSpell(_,_,_) => false
    }
  }
}

case class BuyReinforcement(pieceName: PieceName) extends GeneralBoardAction
case class GainSpell(spellId: Int) extends GeneralBoardAction
//server -> client only
case class RevealSpell(side: Side, spellId: Int, spellName: SpellName) extends GeneralBoardAction

/** Tile:
 *  A single tile on the board
 *  Possibly enchanted due to spells. Later in list -> spells were played later.
 */
case class Tile(
  val terrain: Terrain,
  val modsWithDuration: List[PieceModWithDuration]
)

/**
 * Piece:
 * MUTABLE - be aware when copying, as we rely sharing of mutation to fields between different references to a piece
 * A single piece on the board.
 */
object Piece {
  //This function is used internally by the board implementation.
  //Users should never need to call this function. For setting up initial pieces, see functions like spawnPieceInitial.
  def createInternal(side: Side, pieceName: String, id: Int, loc: Loc, nthAtLoc: Int): Piece = {
    new Piece(
      side = side,
      baseStats = Units.pieceMap(pieceName),
      id = id,
      loc = loc,
      modsWithDuration = List(),
      damage = 0,
      actState = DoneActing,
      hasMoved = false,
      hasAttacked = false,
      spawnedThisTurn = Some(SpawnedThisTurn(pieceName,loc,nthAtLoc))
    )
  }
}
case class Piece private (
  val side: Side,
  val baseStats: PieceStats,
  val id: Int,
  var loc: Loc, //BoardState is responsible for updating this as the piece moves
  //Modifiers from spells, etc, along with the number of turns they will last
  var modsWithDuration: List[PieceModWithDuration],
  //Damage dealt to this piece
  var damage: Int,
  //Indicates what this piece is allowed to do given what it's done
  var actState: ActState,
  //Indicates what this piece actually DID do this turn so far.
  var hasMoved: Boolean,
  var hasAttacked: Boolean,
  //If the piece was newly spawned this turn
  var spawnedThisTurn: Option[SpawnedThisTurn]
) {
  def copy() = {
    new Piece(
      side = side,
      baseStats = baseStats,
      id = id,
      loc = loc,
      modsWithDuration = modsWithDuration,
      damage = damage,
      hasMoved = hasMoved,
      actState = actState,
      hasAttacked = hasAttacked,
      spawnedThisTurn = spawnedThisTurn
    )
  }

  //Taking into account all mods on this piece as well as the mods on the tile the piece is standing on
  def curStats(board: BoardState): PieceStats = {
    (modsWithDuration ++ board.tiles(loc).modsWithDuration).foldLeft(baseStats) {
      (pieceStats,mod) => mod.mod(pieceStats)
    }
  }

  //Get a spec that refers to this piece and can do so stably across undos or actions.
  def spec: PieceSpec = {
    spawnedThisTurn match {
      case None => StartedTurnWithID(id)
      case Some(spawnedThisTurn) => spawnedThisTurn
    }
  }
}

/**
 * BoardState:
 * The full state of one board of the game.
 */
object BoardState {
  def create(terrain: Plane[Terrain]): BoardState = {
    val board = new BoardState(
      tiles = terrain.map { terrain =>
        Tile(terrain = terrain, modsWithDuration = List())
      },
      pieces = Plane.create(terrain.xSize,terrain.ySize,terrain.topology,List()),
      pieceById = Map(),
      nextPieceId = 0,
      piecesSpawnedThisTurn = Map(),
      numPiecesSpawnedThisTurnAt = Map(),
      turnNumber = 0,
      reinforcements = SideArray.create(Map()),
      spellsRevealed = SideArray.create(Map()),
      spellsInHand = SideArray.create(List()),
      side = S0,
      manaThisRound = SideArray.create(0),
      totalMana = SideArray.create(0),
      totalCosts = SideArray.create(0)
    )
    board
  }

  //Some local functions that it's nice to have in scope
  object Imports {
    def failUnless(b: Boolean, message: String) =
      if(!b) throw new Exception(message)
    def failIf(b: Boolean, message: String) =
      if(b) throw new Exception(message)
    def fail(message: String) =
      throw new Exception(message)
    def failed[U](message: String): Try[U] =
      Failure(new Exception(message))
  }
}
import BoardState.Imports._

case class BoardState private (
  //For convenience, we leave these fields exposed rather than making them private and
  //carefully wrapping them in a bunch of getters and setters. But users of BoardState
  //should NOT modify any of these fields, only read them.

  //Tiles of the board
  val tiles: Plane[Tile],
  //List of pieces in each space. Order is irrelevant
  val pieces: Plane[List[Piece]],

  //Map of all pieces currently on the board by pieceId.
  var pieceById: Map[Int,Piece],
  var nextPieceId: Int, //Counter for assigning per-board unique ids to pieces

  //Map of pieces spawned this turn
  var piecesSpawnedThisTurn: Map[SpawnedThisTurn,Piece],
  var numPiecesSpawnedThisTurnAt: Map[Loc,Int],

  //Number of turns completed
  var turnNumber: Int,

  //Count of reinforcement pieces in hand by name
  val reinforcements: SideArray[Map[PieceName,Int]],

  //Map of all spells that have been revealed, by side.
  val spellsRevealed: SideArray[Map[Int,SpellName]],
  //List of all spells in hand, indexed by spellID
  val spellsInHand: SideArray[List[Int]],

  //Current side to move
  var side: Side,

  //Accumulated mana from spires and rebate for costs for units that died, this turn.
  //(Only clears at the beginning of a side's turn)
  val manaThisRound: SideArray[Int],
  //Same, but never clears - summed over the whole board's lifetime.
  val totalMana: SideArray[Int],
  //Total cost of units added to reinforcements of this board over the board's lifetime
  val totalCosts: SideArray[Int]
) {
  val xSize: Int = tiles.xSize
  val ySize: Int = tiles.ySize
  val topology: PlaneTopology = tiles.topology

  def copy(): BoardState = {
    val newBoard = new BoardState(
      tiles = tiles.copy(),
      pieces = pieces.copy(),
      pieceById = Map(), //Set below after construction
      nextPieceId = nextPieceId,
      piecesSpawnedThisTurn = Map(), //Set below after construction
      numPiecesSpawnedThisTurnAt = numPiecesSpawnedThisTurnAt,
      turnNumber = turnNumber,
      reinforcements = reinforcements.copy(),
      spellsRevealed = spellsRevealed.copy(),
      spellsInHand = spellsInHand.copy(),
      side = side,
      manaThisRound = manaThisRound,
      totalMana = totalMana,
      totalCosts = totalCosts
    )
    val newPieceById = pieceById.transform({ (_k, piece) => piece.copy() })
    val newPiecesSpawnedThisTurn = piecesSpawnedThisTurn.transform { (_k, piece) => newPieceById(piece.id) }
    newBoard.pieceById = newPieceById
    newBoard.piecesSpawnedThisTurn = newPiecesSpawnedThisTurn
    newBoard.pieces.transform { pieceList => pieceList.map { piece => newPieceById(piece.id) } }
    newBoard
  }

  //Check whether an action would be legal
  def tryLegality(action: PlayerAction): Try[Unit] = {
    tryLegalitySingle(action)
  }

  //Check whether a sequence of actions would be legal
  def tryLegality(actions: Seq[PlayerAction]): Try[Unit] = {
    val list = actions.toList
    list match {
      case Nil => Success(())
      case action :: Nil => tryLegalitySingle(action)
      case _ =>
        val board = this.copy()
        def loop(list: List[PlayerAction]): Try[Unit] = {
          list match {
            case Nil => Success(())
            case action :: Nil => board.tryLegalitySingle(action)
            case action :: rest =>
              board.doActionSingle(action) match {
                case Success(()) => loop(rest)
                case Failure(err) => Failure(err)
              }
          }
        }
        loop(list)
    }
  }

  //Perform an action
  def doAction(action: PlayerAction): Try[Unit] = {
    doActionSingle(action)
  }

  //Perform a sequence of actions, except if any part of the sequence is illegal, perform none of them.
  def doActions(actions: Seq[PlayerAction]): Try[Unit] = {
    val list = actions.toList
    list match {
      case Nil => Success(())
      case action :: Nil => doActionSingle(action)
      case _ =>
        tryLegality(actions) match {
          case Failure(err) => Failure(err)
          case Success(()) =>
            list.foreach { action => doActionUnsafeAssumeLegal(action) }
            Success(())
        }
    }
  }

  //Perform an action, assuming that it's legal without checking. Could lead to
  //an exception or an invalid board state if it actually isn't legal
  def doActionUnsafeAssumeLegal(action: PlayerAction): Unit = {
    doActionSingleAssumeLegal(action)
  }

  //End the current turn and begin the next turn
  def endTurn(): Unit = {
    //Count and accumulate mana. Wailing units do generate mana
    var newMana = 0
    pieceById.values.foreach { piece =>
      if(piece.side == side)
        newMana += piece.curStats(this).extraMana + (if(tiles(piece.loc).terrain == Graveyard) 1 else 0)
    }

    // TODO jpaulson for dwu: Wailing units that attack die immediately, not at end of turn
    // dwu: Yeah, although actually there's a nasty UI detail here, because they all have multiple attacks.
    // How do you indicate that you're done attacking if you don't spend all your attacks?

    //Wailing units that attacked die
    val attackedWailings = pieceById.iterator.filter { case (_,piece) =>
      piece.curStats(this).isWailing && piece.hasAttacked
    }
    attackedWailings.toList.foreach { case (_,piece) => killPiece(piece) }

    //Heal damage, reset piece state, decay modifiers
    pieceById.values.foreach { piece =>
      refreshPieceForStartOfTurn(piece)
      piece.modsWithDuration = piece.modsWithDuration.flatMap(_.decay)
    }
    //Decay tile modifiers
    tiles.transform { tile =>
      if(tile.modsWithDuration.isEmpty)
        tile
      else
        tile.copy(modsWithDuration = tile.modsWithDuration.flatMap(_.decay))
    }

    manaThisRound(side) += newMana
    totalMana(side) += newMana

    //Flip turn
    side = side.opp
    turnNumber += 1

    //Clear mana for the side to move, and other board state
    manaThisRound(side) = 0
    piecesSpawnedThisTurn = Map()
    numPiecesSpawnedThisTurnAt = Map()
  }

  def tryGeneralLegality(action: GeneralBoardAction): Try[Unit] = {
    action match {
      case BuyReinforcement(pieceName) =>
        if(!Units.pieceMap.contains(pieceName))
          Failure(new Exception("Bought reinforcement piece with unknown name: " + pieceName))
        else
          Success(())
      case GainSpell(_) => Success(())
      case RevealSpell(_,_,_) => Success(())
    }
  }

  //Perform a GeneralBoardAction.
  def doGeneralBoardAction(action: GeneralBoardAction): Unit = {
    action match {
      case BuyReinforcement(pieceName) =>
        if(!Units.pieceMap.contains(pieceName))
          throw new Exception("Bought reinforcement piece with unknown name: " + pieceName)
        addReinforcementInternal(side,pieceName)
        val pieceStats = Units.pieceMap(pieceName)
        totalCosts(side) = totalCosts(side) + pieceStats.cost

      case GainSpell(spellId) =>
        spellsInHand(side) = spellsInHand(side) :+ spellId

      case RevealSpell(side,spellId,spellName) =>
        spellsRevealed(side) = spellsRevealed(side) + (spellId -> spellName)
    }
  }

  //Directly spawn a piece if it possible to do so. Exposed for use to set up initial boards.
  def spawnPieceInitial(side: Side, pieceName: String, loc: Loc): Try[Unit] = {
    trySpawnIsLegal(side, pieceName, loc) match {
      case Failure(err) => Failure(err)
      case Success(()) =>
        val piece = spawnPieceInternal(side,pieceName,loc).get
        refreshPieceForStartOfTurn(piece)
        Success(())
    }
  }

  //Directly add a piece to reinforcements. Exposed for use to set up initial boards.
  def addReinforcementInitial(side: Side, pieceName: String): Unit = {
    addReinforcementInternal(side,pieceName)
  }

  //Is there a piece on the current board matching this spec?
  def pieceExists(spec: PieceSpec): Boolean = {
    findPiece(spec).nonEmpty
  }

  //Find the piece, if any, matching this spec
  def findPiece(spec: PieceSpec): Option[Piece] = {
    spec match {
      case StartedTurnWithID(pieceId) =>
        pieceById.get(pieceId).flatMap { piece =>
          if(piece.spawnedThisTurn.nonEmpty) None
          else Some(piece)
        }
      case (spawnedThisTurn: SpawnedThisTurn) =>
        piecesSpawnedThisTurn.get(spawnedThisTurn)
    }
  }

  private def forEachLegalMoveHelper(piece: Piece, pathBias: List[Loc])(f: (Loc,List[Loc]) => Unit): Unit = {
    val q = scala.collection.mutable.Queue[(Loc, Int, List[Loc])]()
    val seen = scala.collection.mutable.HashSet[Loc]()

    val pieceStats = piece.curStats(this)

    val range = piece.actState match {
      case Moving(stepsUsed) => pieceStats.moveRange - stepsUsed
      case Attacking(_) | Spawning | DoneActing => 0
    }

    //Breadth first floodfill
    q += ((piece.loc, 0, List()))
    while(!q.isEmpty) {
      val (loc,d,revPath) = q.dequeue
      if(tiles.inBounds(loc) && !seen.contains(loc)) {
        seen += loc
        if(canMoveThroughLoc(piece,loc)) {
          val nextRevPath = loc :: revPath
          if(canEndOnLoc(piece.side,piece.spec,pieceStats,loc,List()))
            f(loc,nextRevPath)

          if(d < range) {
            //Enqueue locations that we're biased to prefer first, so that we try those paths first.
            topology.forEachAdj(loc) { y => if(pathBias.contains(y)) q.enqueue((y, d+1, nextRevPath)) }
            topology.forEachAdj(loc) { y => q.enqueue((y, d+1, nextRevPath)) }
          }
        }
      }
    }
  }

  //Find the set of all legal locations that a piece can move to, along with the number of steps to reach the location
  def legalMoves(piece : Piece) : Map[Loc, Int] = {
    val ans = scala.collection.mutable.HashMap[Loc, Int]()
    forEachLegalMoveHelper(piece,List()) { case (loc,revPath) =>
      ans(loc) = revPath.length
    }
    Map() ++ ans
  }

  //Similar to legalMoves but finds a shortest path whose destination satisfies the desired predicate.
  def findLegalMove(piece : Piece, pathBias: List[Loc])(f: Loc => Boolean): Option[List[Loc]] = {
    forEachLegalMoveHelper(piece,pathBias) { case (loc,revPath) =>
      if(f(loc)) {
        return Some(revPath.reverse)
      }
    }
    None
  }

  def canAttack(attackerStats: PieceStats, attackerHasMoved: Boolean, attackerState: ActState, targetStats: PieceStats): Boolean = {
    tryCanAttack(attackerStats, attackerHasMoved, attackerState, targetStats).isSuccess
  }

  def tryCanAttack(attackerStats: PieceStats, attackerHasMoved: Boolean, attackerState: ActState, targetStats: PieceStats): Try[Unit] = {
    if(attackerStats.attackEffect.isEmpty || attackerStats.numAttacks == 0) failed("Piece cannot attack")
    else if(attackerStats.attackRange <= 0) failed("Piece cannot attack")
    else if(attackerStats.isLumbering && attackerHasMoved) failed("Lumbering pieces cannot both move and attack on the same turn")
    else {
      val result = attackerState match {
        case Moving(_) => Success(())
        case Attacking(numAttacks) =>
          //Slightly more specific err message
          if(attackerStats.numAttacks == 1 && numAttacks > 0) failed("Piece already attacked")
          //Fuller check
          else if(numAttacks >= attackerStats.numAttacks) failed("Piece already assigned all of its attacks")
          else Success(())
        case Spawning | DoneActing =>
          failed("Piece has already acted or cannot attack any more this turn")
      }
      result.flatMap { case () =>
        val result2 = attackerStats.attackEffect match {
          case None => failed("Piece cannot attack")
          case Some(Damage(_)) => Success(())
          case Some(Kill) =>
            if(targetStats.isNecromancer) failed("Death attacks cannot hurt necromancers")
            else Success(())
          case Some(Unsummon) =>
            if(targetStats.isPersistent) failed("Target is persistent - cannot be unsummoned")
            else Success(())
          case Some(Enchant(_)) => Success(())
          case Some(TransformInto(_)) =>
            if(targetStats.isNecromancer) failed("Necromancers cannot be transformed")
            else Success(())
        }
        result2.flatMap { case () =>
          if(attackerStats.isWailing && targetStats.isNecromancer) failed("Wailing pieces cannot hurt necromancers")
          else if(!attackerStats.canHurtNecromancer && targetStats.isNecromancer) failed("Piece not allowed to hurt necromancer")
          else Success(())
        }
      }
    }
  }

  def inBounds(loc: Loc): Boolean = {
    tiles.inBounds(loc)
  }

  //HELPER FUNCTIONS -------------------------------------------------------------------------------------
  private def canWalkOnTile(pieceStats: PieceStats, tile: Tile): Boolean = {
    tile.terrain match {
      case Wall => false
      case Ground | Graveyard | Spawner(_,_) => true
      case Water => pieceStats.isFlying
    }
  }
  private def tryCanWalkOnTile(pieceStats: PieceStats, tile: Tile): Try[Unit] = {
    tile.terrain match {
      case Wall => failed("Cannot move or spawn through borders")
      case Ground | Graveyard | Spawner(_,_) => Success(())
      case Water => if(pieceStats.isFlying) Success(()) else failed("Non-flying pieces cannot move or spawn on water")
    }
  }

  private def canMoveThroughLoc(piece: Piece, loc: Loc): Boolean = {
    val pieceStats = piece.curStats(this)
    canWalkOnTile(pieceStats,tiles(loc)) &&
    (pieceStats.isFlying || pieces(loc).forall { other => other.side == piece.side })
  }
  private def tryCanMoveThroughLoc(piece: Piece, loc: Loc): Try[Unit] = {
    val pieceStats = piece.curStats(this)
    tryCanWalkOnTile(pieceStats,tiles(loc)).flatMap { case () =>
      if(pieceStats.isFlying || pieces(loc).forall { other => other.side == piece.side }) Success(())
      else failed("Non-flying pieces cannot move through enemies")
    }
  }

  private def canSwarmTogether(pieceStats: PieceStats, otherStats: PieceStats): Boolean = {
    pieceStats.swarmMax > 1 && otherStats.swarmMax > 1 && pieceStats.name == otherStats.name
  }

  private def canSwarmToLoc(pieceSpec: PieceSpec, pieceStats: PieceStats, loc: Loc, simultaneousMovements: List[Movement]): Boolean = {
    var piecesOnFinalSquare = 1
    var minSwarmMax = pieceStats.swarmMax
    def pieceIfMovingTo(pieceSpec: PieceSpec, path: Vector[Loc], dest: Loc): Option[Piece] =
      if(path.length > 0 && path.last == dest) findPiece(pieceSpec)
      else None
    def okSwarmer(otherStats: PieceStats): Boolean = {
      if(!canSwarmTogether(otherStats,pieceStats)) false
      else {
        piecesOnFinalSquare += 1
        minSwarmMax = Math.min(minSwarmMax,otherStats.swarmMax)
        true
      }
    }
    val ok = {
      pieces(loc).forall { other =>
        if(other.spec == pieceSpec) true
        else if(simultaneousMovements.exists { case Movement(spec,_) => pieceSpec == spec }) true
        else okSwarmer(other.curStats(this))
      } && simultaneousMovements.forall { case Movement(otherSpec,otherPath) =>
          if(pieceSpec == otherSpec) true
          else pieceIfMovingTo(otherSpec,otherPath,loc).forall { other => okSwarmer(other.curStats(this)) }
      }
    }
    ok && piecesOnFinalSquare <= minSwarmMax
  }

  private def canEndOnLoc(side: Side, pieceSpec: PieceSpec, pieceStats: PieceStats, loc: Loc, simultaneousMovements: List[Movement]): Boolean = {
    tiles.inBounds(loc) &&
    canWalkOnTile(pieceStats,tiles(loc)) &&
    pieces(loc).forall { other => other.side == side } &&
    canSwarmToLoc(pieceSpec,pieceStats,loc,simultaneousMovements)
  }
  private def tryCanEndOnLoc(side: Side, pieceSpec: PieceSpec, pieceStats: PieceStats, loc: Loc, simultaneousMovements: List[Movement]): Try[Unit] = {
    if(!tiles.inBounds(loc)) failed("Location not in bounds")
    else {
      tryCanWalkOnTile(pieceStats,tiles(loc)).flatMap { case () =>
        if(!pieces(loc).forall { other => other.side == side }) failed("Piece would end in the same space as enemies")
        else {
          if(!canSwarmToLoc(pieceSpec,pieceStats,loc,simultaneousMovements)) {
            if(pieceStats.swarmMax > 1) failed("Pieces cannot swarm together or too many in the same space")
            else failed("Piece would end in same space as other pieces")
          }
          else Success(())
        }
      }
    }
  }

  private def trySpellTargetLegality(spellName: SpellName, played: PlayedSpellOrAbility): Try[Unit] = {
    Spells.spellMap.get(spellName) match {
      case None => failed("Unknown spell name")
      case Some(spell) =>
        spell match {
          case (spell: TargetedSpell) =>
            findPiece(played.target0) match {
              case None => failed("No target specified for spell")
              case Some(target) =>
                if(!spell.canTarget(side,target)) failed(spell.targetError)
                else Success(())
            }
          case (spell: TileSpell) =>
            if(!tiles.inBounds(played.loc0)) failed("Target location not in bounds")
            else if(!spell.canTarget(side,tiles(played.loc0),pieces(played.loc0))) failed(spell.targetError)
            else Success(())
          case (_: NoEffectSpell) =>
            Success(())
        }
    }
  }

  //Tests if a spawn is possible. Doesn't test reinforcements (since other things can
  //cause spawns, such as death spawns, this functions handles the most general tests).
  private def spawnIsLegal(spawnSide: Side, spawnName: PieceName, spawnLoc: Loc): Boolean = {
    Units.pieceMap.get(spawnName) match {
      case None => false
      case Some(spawnStats) =>
        //Make up a fake spec that won't match anything else
        val pieceSpec = SpawnedThisTurn(spawnName, spawnLoc, nthAtLoc = -1)
        canEndOnLoc(spawnSide, pieceSpec, spawnStats, spawnLoc, List())
    }
  }
  private def trySpawnIsLegal(spawnSide: Side, spawnName: PieceName, spawnLoc: Loc): Try[Unit] = {
    Units.pieceMap.get(spawnName) match {
      case None => failed("Unknown spawned piece name")
      case Some(spawnStats) =>
        //Make up a fake spec that won't match anything else
        val pieceSpec = SpawnedThisTurn(spawnName, spawnLoc, nthAtLoc = -1)
        tryCanEndOnLoc(spawnSide, pieceSpec, spawnStats, spawnLoc, List())
    }
  }

  //Check if a single action is legal
  private def tryLegalitySingle(action: PlayerAction): Try[Unit] = Try {
    failIf(turnNumber < 0, "Game is not started yet")
    action match {
      case Movements(movements) =>
        //Check basic properties of the set of movements
        val pieceIdsMoved = movements.map { case Movement(pieceId,_) => pieceId }
        failIf(pieceIdsMoved.distinct.length != movements.length, "More than one movement for the same piece")
        failIf(movements.isEmpty, "Empty set of movements")

        movements.foreach { case Movement(pieceSpec,path) =>
          //Check basic properties of path
          failUnless(path.length > 1, "Empty or trivial movement path")
          failUnless(path(0) != path.last, "Circular movement path")
          failUnless(path.forall { loc => tiles.inBounds(loc) }, "Movement out of bounds")
          failUnless((1 to (path.length - 1)).forall { idx => topology.distance(path(idx-1),path(idx)) == 1 },
            "Movement path locations not all adjacent")

          findPiece(pieceSpec) match {
            case None => fail("Moving a nonexistent or dead piece")
            case Some(piece) =>
              //Ownership
              failUnless(piece.side == side, "Piece controlled by other side")

              //Piece currently at the start of the path
              failUnless(piece.loc == path(0), "Moved piece is not at the head of the path")

              val pieceStats = piece.curStats(this)

              //Piece movement range and state is ok
              failIf(pieceStats.moveRange <= 0, "Piece cannot move")
              piece.actState match {
                case Moving(stepsUsed) =>
                  failUnless(path.length - 1 <= pieceStats.moveRange - stepsUsed, "Movement range of piece is not large enough")
                case Attacking(_) | Spawning | DoneActing =>
                  fail("Piece has already acted or cannot move any more this turn")
              }

              //Check spaces along the path
              path.foreach { loc => tryCanMoveThroughLoc(piece, loc).get}
              //Check space at the end of the path
              tryCanEndOnLoc(piece.side, piece.spec, pieceStats, path.last, movements).get
          }
        }

      case Attack(attackerSpec, targetSpec) =>
        (findPiece(attackerSpec),findPiece(targetSpec)) match {
          case (None, _) => fail("Attacking with a nonexistent or dead piece")
          case (Some(_),None) => fail("Attacking a nonexistent or dead piece")
          case (Some(attacker),Some(target)) =>
            failUnless(attacker.side == side, "Attacker is controlled by other side")
            failUnless(target.side != side, "Target piece is friendly")
            failUnless(tiles.inBounds(target.loc), "Target location not in bounds")
            val attackerStats = attacker.curStats(this)
            failUnless(topology.distance(attacker.loc,target.loc) <= attackerStats.attackRange, "Attack range not large enough")
            val targetStats = target.curStats(this)
            tryCanAttack(attackerStats, attacker.hasMoved, attacker.actState, targetStats).get
        }

      case Spawn(spawnLoc, spawnName) =>
        //A bunch of tests that don't depend on the spawner or on reinforcements state
        trySpawnIsLegal(side, spawnName, spawnLoc).get
        val spawnStats = Units.pieceMap(spawnName)

        def trySpawnUsing(piece: Piece): Try[Unit] = {
          val distance = topology.distance(spawnLoc,piece.loc)
          if(piece.side != side)  Failure(new Exception("Spawner controlled by other side"))
          else if((!spawnStats.isEldritch || distance > 1) && (piece.curStats(this).spawnRange < distance))  Failure(new Exception("Location not within range of spawner"))
          else if(piece.actState > Spawning)  Failure(new Exception("Spawner cannot act any more this turn"))
          else Success(())
        }

        failUnless(pieceById.values.exists { piece => trySpawnUsing(piece).isSuccess }, "No non-newly-spawned piece with spawn in range")
        failUnless(reinforcements(side).contains(spawnName), "No such piece in reinforcements")

      case SpellsAndAbilities(spellsAndAbilities) =>
        //Ensure no spells played more than once
        val spellIdsPlayed = spellsAndAbilities.flatMap { played => played.spellId }
        failIf(spellIdsPlayed.distinct.length != spellIdsPlayed.length, "Spell played more than once")

        //Check that all spells and abilities are targeted legally
        spellsAndAbilities.foreach { played =>
          failUnless(played.side == side, "Playing spell or ability for the wrong side")
          (played.pieceSpecAndKey, played.spellId) match {
            case (None, None) => fail("Spell or ability did not specify piece ability or spell id")
            case (Some(_), Some(_)) => fail("Spell or ability specifies both piece ability and spell id")
            case (Some((pieceSpec,key)),None) =>
              findPiece(pieceSpec) match {
                case None => fail("Using ability of a nonexistent or dead piece")
                case Some(piece) =>
                  failUnless(piece.side == side, "Piece controlled by other side")
                  failIf(piece.actState >= DoneActing, "Piece has already acted or cannot act any more this turn")
                  val pieceStats = piece.curStats(this)
                  pieceStats.abilities.get(key) match {
                    case None => fail("Piece does not have this ability")
                    case Some(ability:SelfEnchantAbility) =>
                      failUnless(ability.isUsableNow(piece), ability.unusableError)
                    case Some(ability:TargetedAbility) =>
                      failUnless(ability.isUsableNow(piece), ability.unusableError)
                      findPiece(played.target0) match {
                        case None => fail("No target specified for ability")
                        case Some(target) => failUnless(ability.canTarget(piece,target), ability.targetError)
                      }
                  }
              }
            case (None, Some(spellId)) =>
              spellsInHand(side).contains(spellId) match {
                case false => fail("Spell not in hand or already played or discarded")
                case true =>
                  spellsRevealed(side).get(spellId) match {
                    case None => fail("Spell was not revealed")
                    case Some(spellName) => trySpellTargetLegality(spellName,played).get
                  }
              }
          }
        }

        //Check discard/sorcery constraints
        var discardIds: Map[Int,Int] = Map()
        spellsAndAbilities.foreach { played =>
          val isSorcery: Boolean = (played.pieceSpecAndKey, played.spellId) match {
            case (Some((pieceSpec,key)),None) =>
              findPiece(pieceSpec).get.curStats(this).abilities(key).isSorcery
            case (None, Some(spellId)) =>
              val spell = Spells.spellMap(spellsRevealed(side)(spellId))
              spell.spellType match {
                case Sorcery => true
                case NormalSpell | Cantrip | DoubleCantrip => false
              }
            //Already checked that exactly one is Some
            case _ => assertUnreachable()
          }
          played.discardingId match {
            case None =>
              failIf(isSorcery, "Must discard a spell to play a sorcery")
            case Some(discardId) =>
              failIf(!isSorcery, "Discarding a spell to play a spell or ability that is not a sorcery")
              discardIds = discardIds.update(discardId) { count => count.getOrElse(0) + 1 }
          }
        }
        //Make sure all discards are legal
        discardIds.foreach { case (discardId, count) =>
          spellsInHand(side).contains(discardId) match {
            case false => fail("Discarded spell is not in hand or is already played or discarded")
            case true =>
              val spell = Spells.spellMap(spellsRevealed(side)(discardId))
              spell.spellType match {
                case (Sorcery | NormalSpell) =>
                  failIf(spellIdsPlayed.contains(discardId), "Cannot both play a normal spell or sorcery and discard it")
                  failIf(count > 1, "Attempting to power more than one sorcery per discarded spell")
                case Cantrip =>
                  failIf(count > 1, "Attempting to power more than one sorcery per discarded cantrip")
                case DoubleCantrip =>
                  failIf(count > 2, "Attempting to power more than two sorceries with special cantrip")
              }
          }
        }
    }
  }

  //Perform a single action, doing nothing if it isn't legal
  private def doActionSingle(action:PlayerAction): Try[Unit] = {
    tryLegalitySingle(action).map { case () => doActionSingleAssumeLegal(action) }
  }

  private def doActionSingleAssumeLegal(action:PlayerAction): Unit = {
    action match {
      case Movements(movements) =>
        movements.foreach { movement =>
          val piece = findPiece(movement.pieceSpec).get
          val src = movement.path(0)
          val dest = movement.path.last
          pieces(src) = pieces(src).filterNot { p => p.id == piece.id }
          pieces(dest) = pieces(dest) :+ piece
          piece.loc = dest
          piece.hasMoved = true
          piece.actState = piece.actState match {
            case Moving(n) => Moving(n + movement.path.length - 1)
            case Attacking(_) | Spawning | DoneActing => assertUnreachable()
          }
        }
      case Attack(attackerSpec, targetSpec) =>
        val attacker = findPiece(attackerSpec).get
        val target = findPiece(targetSpec).get
        val attackerStats = attacker.curStats(this)
        val attackEffect = attackerStats.attackEffect.get
        applyEffect(attackEffect,target)
        attacker.hasAttacked = true
        attacker.actState = attacker.actState match {
          case Moving(_) => Attacking(1)
          case Attacking(n) => Attacking(n+1)
          case Spawning | DoneActing => assertUnreachable()
        }
        if(attackerStats.hasBlink)
          unsummonPiece(attacker)

      case Spawn(spawnLoc, spawnName) =>
        spawnPieceInternal(side,spawnName,spawnLoc) match {
          case Some(_: Piece) => ()
          case None => assertUnreachable()
        }
        reinforcements(side) = {
          reinforcements(side).get(spawnName) match {
            case None => assertUnreachable()
            case Some(n) =>
              if(n <= 1) reinforcements(side) - spawnName
              else reinforcements(side) + (spawnName -> (n-1))
          }
        }

      case SpellsAndAbilities(spellsAndAbilities) =>
        //Apply spell effects
        spellsAndAbilities.foreach { played => (played.pieceSpecAndKey, played.spellId) match {
          case (None, None) => assertUnreachable()
          case (Some(_), Some(_)) => assertUnreachable()
          case (Some((pieceSpec,key)),None) =>
            val piece = findPiece(pieceSpec).get
            val pieceStats = piece.curStats(this)
            pieceStats.abilities(key) match {
              case (ability:SelfEnchantAbility) =>
                piece.modsWithDuration = piece.modsWithDuration :+ ability.mod
              case (ability:TargetedAbility) =>
                val target = findPiece(played.target0).get
                applyEffect(ability.effect,target)
            }
          case (None, Some(spellId)) =>
            val spell = Spells.spellMap(spellsRevealed(side)(spellId))
            spell match {
              case (_: NoEffectSpell) => ()
              case (spell: TargetedSpell) =>
                val target = findPiece(played.target0).get
                applyEffect(spell.effect,target)
              case (spell: TileSpell) =>
                tiles(played.loc0) = spell.effect(tiles(played.loc0))
                val piecesOnTile = pieces(played.loc0)
                piecesOnTile.foreach { piece => killIfEnoughDamage(piece) }
            }
        }}

        //Remove spells from hand
        spellsAndAbilities.foreach { played =>
          played.discardingId.foreach { id => spellsInHand(side) = spellsInHand(side).filterNot { i => i == id } }
          played.spellId.foreach { id => spellsInHand(side) = spellsInHand(side).filterNot { i => i == id } }
        }
    }
  }

  private def applyEffect(effect: TargetEffect, piece: Piece): Unit = {
    effect match {
      case Damage(n) =>
        piece.damage += n
        killIfEnoughDamage(piece)
      case Unsummon =>
        unsummonPiece(piece)
      case Kill =>
        killPiece(piece)
      case Enchant(modWithDuration) =>
        piece.modsWithDuration = piece.modsWithDuration :+ modWithDuration
        killIfEnoughDamage(piece)
      case TransformInto(newName) =>
        removeFromBoard(piece)
        spawnPieceInternal(piece.side,newName,piece.loc) match {
          case Some(_: Piece) => ()
          case None => ()
            //Piece was unable to legally belong on that square, so treat it as if killed
            updateAfterPieceKill(piece.side,Units.pieceMap(newName),piece.loc)
        }
    }
  }

  //Kill a piece if it has enough accumulated damage
  private def killIfEnoughDamage(piece: Piece): Unit = {
    val stats = piece.curStats(this)
    if(piece.damage >= stats.defense)
      killPiece(piece)
  }

  //Perform the rebase and death spawn updates happening after a piece kill
  private def updateAfterPieceKill(pieceSide: Side, pieceStats: PieceStats, loc: Loc): Unit = {
    //Rebate mana
    manaThisRound(pieceSide) += pieceStats.rebate
    totalMana(pieceSide) += pieceStats.rebate

    //Death spawn
    pieceStats.deathSpawn.foreach { deathSpawn =>
      spawnPieceInternal(pieceSide,deathSpawn,loc) match {
        case Some(_: Piece) => ()
        case None =>
          //Piece was unable to legally belong on that square, so treat it as if killed
          updateAfterPieceKill(pieceSide,Units.pieceMap(deathSpawn),loc)
      }
    }
  }

  //Kill a piece, for any reason
  private def killPiece(piece: Piece): Unit = {
    removeFromBoard(piece)
    updateAfterPieceKill(piece.side,piece.curStats(this),piece.loc)
  }

  private def unsummonPiece(piece: Piece): Unit = {
    removeFromBoard(piece)
    addReinforcementInternal(piece.side,piece.baseStats.name)
  }
  private def removeFromBoard(piece: Piece): Unit = {
    pieces(piece.loc) = pieces(piece.loc).filterNot { p => p.id == piece.id }
    pieceById = pieceById - piece.id
    piece.spawnedThisTurn.foreach { spawnedThisTurn => piecesSpawnedThisTurn = piecesSpawnedThisTurn - spawnedThisTurn }
  }

  private def addReinforcementInternal(side: Side, pieceName: PieceName): Unit = {
    reinforcements(side) = reinforcements(side) + (pieceName -> (reinforcements(side).getOrElse(pieceName,0) + 1))
  }

  //Does check for legality of spawn, returning the piece on success
  private def spawnPieceInternal(spawnSide: Side, spawnName: PieceName, spawnLoc: Loc): Option[Piece] = {
    if(!spawnIsLegal(spawnSide, spawnName, spawnLoc))
      None
    else {
      val nthAtLoc = numPiecesSpawnedThisTurnAt.get(spawnLoc).getOrElse(0)
      val piece = Piece.createInternal(spawnSide, spawnName, nextPieceId, spawnLoc, nthAtLoc)
      pieces(spawnLoc) = pieces(spawnLoc) :+ piece
      pieceById += (piece.id -> piece)
      nextPieceId += 1
      piecesSpawnedThisTurn += (piece.spawnedThisTurn.get -> piece)
      numPiecesSpawnedThisTurnAt += (spawnLoc -> nthAtLoc)
      Some(piece)
    }
  }

  private def refreshPieceForStartOfTurn(piece: Piece): Unit = {
    piece.damage = 0
    piece.actState = Moving(0)
    piece.hasMoved = false
    piece.hasAttacked = false

    piece.spawnedThisTurn.foreach { spawnedThisTurn => piecesSpawnedThisTurn = piecesSpawnedThisTurn - spawnedThisTurn }
    piece.spawnedThisTurn = None
  }
}

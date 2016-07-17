import scala.util.{Try,Success,Failure}
import scala.collection.immutable.{Queue, Vector}

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
  * Also implemented are GeneralActions - gaining spells and buying pieces.
  * These are recorded separately from PlayerActions. This is because they aren't subject to the same
  * undo/redo/reordering logic that player actions are subject to, since they involve interaction with the broader game.
  *
  * Given general action G and player action P, an invariant that should hold is:
  * - If P followed by G is legal, then G followed by P must also be legal and lead to the same board state.
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
case class SpawnedThisTurn(name: String, spawnLoc: Loc, nthAtLoc: Int) extends PieceSpec
with Ordered[SpawnedThisTurn] {
  def compare(that: SpawnedThisTurn): Int =
    Ordering[(String,Int,Int,Int)].compare(
        (name,spawnLoc.x,spawnLoc.y,nthAtLoc),
        (that.name,that.spawnLoc.x,that.spawnLoc.y,that.nthAtLoc)
    )
}

/**
  * PlayerAction:
  * A single action taken by a player. Does not include "General" actions of gaining spells or buying pieces.
  * PlayerActions are immutable and their data should be independent of any of the mutable types
  * on the board. That is, it should be meaningful to ask whether an action would be legal
  * on a different board, or if the order of actions were rearranged, etc.
  *
  * Notes:
  * Movements: movements is a list because we need to support piece swaps, triangular rotations, etc.
  * Attack: self-explanatory
  * Spawn: freeSpawnerSpec should only be specified if spawning something for free, like the per-turn zombie from a graveyard.
  * SpellsAndAbilities: spellsAndAbilities is a list because we need to support playing sorceries and cantrips together.
  */
sealed trait PlayerAction
case class Movements(movements: List[Movement]) extends PlayerAction
case class Attack(attackerSpec: PieceSpec, targetLoc: Loc, targetSpec: PieceSpec) extends PlayerAction
case class Spawn(spawnLoc: Loc, spawnStats: PieceStats, freeSpawnerSpec: Option[PieceSpec]) extends PlayerAction
case class SpellsAndAbilities(spellsAndAbilities: List[PlayedSpellOrAbility]) extends PlayerAction

//Note: path should contain both the start and ending location
case class Movement(pieceSpec: PieceSpec, path: Array[Loc])

//Data for a played spell or piece ability.
//Since different spells and abilities affect different things and have different numbers
//of targets, not all fields may be applicable.
case class PlayedSpellOrAbility(
  val pieceSpecAndKey: Option[(PieceSpec,String)], //Some if it was a piece, (pieceId, key of ability)
  val spellId: Option[Int], //Some if it was a spell
  val target0: PieceSpec,
  val target1: PieceSpec,
  val loc0: Loc,
  //Spell ids that were discarded to power this spell or ability, if it was a sorcery
  val discardingId: Option[Int]
)

object PlayerAction {
  //Does action involve pieceSpec in any way?
  def involvesPiece(action: PlayerAction, pieceSpec: PieceSpec): Boolean = {
    action match {
      case Movements(movements) =>
        movements.exists { case Movement(pSpec, _) => pieceSpec == pSpec }
      case Attack(aSpec,_,tSpec) => pieceSpec == aSpec || pieceSpec == tSpec
      case Spawn(spawnLoc,spawnStats,fSpec) =>
        fSpec.exists { fSpec => pieceSpec == fSpec } || (
          pieceSpec match {
            case StartedTurnWithID(_) => false
            //Note that we don't check nthAtLoc - this means local undo will undo all units spawned on that hex with that name.
            case SpawnedThisTurn(name,sLoc,_) => name == spawnStats.name && sLoc == spawnLoc
          }
        )
      case SpellsAndAbilities(spellsAndAbilities) =>
        spellsAndAbilities.exists { played =>
          played.pieceSpecAndKey.exists { case (pSpec,_) => pieceSpec == pSpec } ||
          pieceSpec == played.target0 ||
          pieceSpec == played.target1
        }
    }
  }
}


//TODO think about how to manage the opposing side not being able to see what spells you own...

/** GeneralAction:
  * Actions relating to this board that involve interaction with the broader game (a shared spell pool, a shared mana pool).
  * These are NOT part of the normal action stack.
  *
  * Requirement: spellId should be a unique identifier for a particular spell card. Users of BoardState should ensure that this is the case.
  */
sealed trait GeneralAction
case class BuyReinforcement(side: Side, pieceStats: PieceStats) extends GeneralAction
case class GainSpell(side: Side, spellId: Int, spell: Spell) extends GeneralAction

/**
 * Message:
 * A line in a log of notable events that happened in the board up to this point
 */
case class Message(
  val message: String,
  val mtype: MessageType,
  val timeLeft: Double
)
/** MessageType:
 *  Extra metadata about what kind of message it is
 */
sealed trait MessageType
case class PlayerActionMsgType(side: Side) extends MessageType
case class GeneralActionMsgType(side: Side) extends MessageType
case object TurnChangeMsgType extends MessageType
case object InternalEventMsgType extends MessageType

/** Tile:
 *  A single tile on the board
 *  Possibly enchanted due to spells. Later in list -> spells were played later.
 */
object Tile {
  def create(terrain : Terrain) = new Tile(terrain, modsWithDuration = List())
}
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
  def create(side: Side, baseStats: PieceStats, id: Int, loc: Loc, nthAtLoc: Int, board: BoardState): Piece = {
    new Piece(
      side = side,
      baseStats = baseStats,
      id = id,
      loc = loc,
      board = board,
      modsWithDuration = List(),
      damage = 0,
      actState = DoneActing,
      hasMoved = false,
      hasAttacked = false,
      hasFreeSpawned = false,
      spawnedThisTurn = Some(SpawnedThisTurn(baseStats.name,loc,nthAtLoc))
    )
  }
}
class Piece private (
  val side: Side,
  val baseStats: PieceStats,
  val id: Int,
  var loc: Loc, //BoardState is responsible for updating this as the piece moves
  var board: BoardState,
  //Modifiers from spells, etc, along with the number of turns they will last
  var modsWithDuration: List[PieceModWithDuration],
  //Damage dealt to this piece
  var damage: Int,
  //Indicates what this piece is allowed to do given what it's done
  var actState: ActState,
  //Indicates what this piece actually DID do this turn so far.
  var hasMoved: Boolean,
  var hasAttacked: Boolean,
  var hasFreeSpawned: Boolean,
  //If the piece was newly spawned this turn
  var spawnedThisTurn: Option[SpawnedThisTurn]
) {
  def copy(newBoard: BoardState) = {
    new Piece(
      side = side,
      baseStats = baseStats,
      id = id,
      loc = loc,
      board = newBoard,
      modsWithDuration = modsWithDuration,
      damage = damage,
      hasMoved = hasMoved,
      actState = actState,
      hasAttacked = hasAttacked,
      hasFreeSpawned = hasFreeSpawned,
      spawnedThisTurn = spawnedThisTurn
    )
  }

  //Taking into account all mods on this piece as well as the mods on the tile the piece is standing on
  def curStats: PieceStats = {
    (modsWithDuration ++ board.tiles(loc).modsWithDuration).foldLeft(baseStats) {
      (pieceStats,mod) => mod.mod(pieceStats)
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
      reinforcements = SideArray.create(List()),
      spells = SideArray.create(Map()),
      side = S0,
      messages = Vector(),
      mana = SideArray.create(0),
      totalMana = SideArray.create(0),
      totalCosts = SideArray.create(0)
    )
    val message = "New board started"
    board.addMessage(message,TurnChangeMsgType)
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
  }
}
import BoardState.Imports._

class BoardState private (
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

  //List of all reinforcement pieces in hand
  val reinforcements: SideArray[List[PieceStats]],

  //List of all spells in hand, indexed by spellID
  val spells: SideArray[Map[Int,Spell]],

  //Current side to move
  var side: Side,

  //Messages for external display to user as a result of events
  var messages: Vector[Message],
  //Accumulated mana from spires and rebate for costs for units that died, this turn.
  //(Only clears at the beginning of a side's turn)
  val mana: SideArray[Int],
  //Same, but never clears - summed over the whole board's lifetime.
  val totalMana: SideArray[Int],
  //Total cost of units added to reinforcements of this board over the board's lifetime
  val totalCosts: SideArray[Int]
) {
  val xSize: Int = tiles.xSize
  val ySize: Int = tiles.ySize
  val topology: PlaneTopology = tiles.topology

  //TODO rework this, either drop the message feature or implement it better (maybe in Board.scala rather than here to put it along with history stuff)
  //Transient, used only for message logging and is set by external code prior to calling
  //functions to modify board state
  var timeLeft: Double = 0.0
  def setTimeLeft(t: Double) = {
    timeLeft = t
  }

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
      spells = spells.copy(),
      side = side,
      messages = messages,
      mana = mana,
      totalMana = totalMana,
      totalCosts = totalCosts
    )
    val newPieceById = pieceById.mapValues { piece => piece.copy(newBoard) }
    val newPiecesSpawnedThisTurn = piecesSpawnedThisTurn.mapValues { piece => newPieceById(piece.id) }
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
      case action :: Nil => tryLegalitySingle(list.head)
      case _ =>
        val board = this.copy()
        def loop(list: List[PlayerAction]): Try[Unit] = {
          list match {
            case Nil => Success(())
            case action :: Nil => board.tryLegalitySingle(list.head)
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

  //End the current turn and begin the next turn
  def endTurn(): Unit = {
    //Count and accumulate mana. Wailing units do generate mana
    var newMana = 0
    pieceById.values.foreach { piece =>
      if(piece.side == side)
        newMana += piece.curStats.extraMana + (if(tiles(piece.loc).terrain == ManaSpire) 1 else 0)
    }

    //Wailing units that attacked die
    val attackedWailings = pieceById.iterator.filter { case (pieceId,piece) =>
      piece.curStats.isWailing && piece.hasAttacked
    }
    attackedWailings.toList.foreach { case (_pieceId,piece) => killPiece(piece) }

    //Heal damage, reset piece state, decay modifiers
    pieceById.values.foreach { piece =>
      refreshPieceForStartOfTurn(piece)
      piece.modsWithDuration = piece.modsWithDuration.flatMap(_.decay)
    }
    //Decay tile modifiers
    tiles.mapInPlace { tile =>
      if(tile.modsWithDuration.isEmpty)
        tile
      else
        tile.copy(modsWithDuration = tile.modsWithDuration.flatMap(_.decay))
    }

    mana(side) += newMana
    totalMana(side) += newMana

    //TODO put team or player names into here?
    val message = "%s turn ended, generated %d mana (board lifetime %d, costs %d, net %d)".format(
      side.toString(),newMana,totalMana(side),totalCosts(side),totalMana(side) - totalCosts(side)
    )
    addMessage(message,TurnChangeMsgType)

    //Flip turn
    side = side.opp
    turnNumber += 1

    //Clear mana for the side to move, and other board state
    mana(side) = 0
    piecesSpawnedThisTurn = Map()
    numPiecesSpawnedThisTurnAt = Map()
  }

  //Perform a GeneralAction. These are always legal.
  def doGeneralAction(action: GeneralAction): Unit = {
    action match {
      case BuyReinforcement(side,pieceStats) =>
        addReinforcementInternal(side,pieceStats)
        totalCosts(side) = totalCosts(side) + pieceStats.cost
        val message = "Bought %s (cost %s mana)".format(pieceStats.name,pieceStats.cost)
        addMessage(message,GeneralActionMsgType(side))

      case GainSpell(side,spellId,spell) =>
        if(spells(side).contains(spellId))
          throw new Exception("spellId is not a unique id and has already occurred in this BoardState: " + spellId)
        spells(side) = spells(side) + (spellId -> spell)
    }
  }

  //Directly spawn a piece if it possible to do so. Exposed for use to set up initial boards.
  def spawnPieceInitial(side: Side, pieceStats: PieceStats, loc: Loc): Try[Unit] = {
    spawnPieceInternal(side,pieceStats,loc).map { piece =>
      refreshPieceForStartOfTurn(piece)
    }
  }

  //Is there a piece on the current board matching this spec?
  def pieceExists(spec: PieceSpec): Boolean = {
    findPiece(spec).nonEmpty
  }

  def legalMoves(piece : Piece) : Set[Loc] = {
    var q = scala.collection.mutable.Queue[(Loc, Int)]()
    var seen = scala.collection.mutable.HashSet[Loc]()
    var ans = scala.collection.mutable.HashSet[Loc]()

    val range = piece.actState match {
      case Moving(stepsUsed) => piece.baseStats.moveRange - stepsUsed
      case Attacking(_) | Spawning | DoneActing => 0
    }

    q += ((piece.loc, 0))
    while(!q.isEmpty) {
      val (x,d) = q.dequeue
      if(piece.board.tiles.inBounds(x) && !seen.contains(x)) {
        seen += x
        println(x)
        val terrain_ok = canWalkOnTile(piece.baseStats, piece.board.tiles(x)) && d<=piece.baseStats.moveRange
        val has_enemy = piece.board.pieces(x).exists { other => other.side != piece.side }
        val within_range = d <= range
        if(terrain_ok && within_range && (!has_enemy || piece.baseStats.isFlying)) {
          if(!has_enemy) {
            ans += x
          }
          piece.board.tiles.topology.forEachAdj(x) { y => q.enqueue((y, d+1))}
        }
      }
    }
    Set() ++ ans
  }

  //HELPER FUNCTIONS -------------------------------------------------------------------------------------
  private def addMessage(message: String, mtype: MessageType): Unit = {
    messages = messages :+ Message(message,mtype,timeLeft)
  }

  private def findPiece(spec: PieceSpec): Option[Piece] = {
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

  private def canWalkOnTile(pieceStats: PieceStats, tile: Tile): Boolean = {
    tile.terrain match {
      case Wall => false
      case Ground | ManaSpire | Spawner(_,_) => true
      case Water => pieceStats.isFlying
    }
  }
  private def canSwarmTogether(pieceStats: PieceStats, otherStats: PieceStats): Boolean = {
    pieceStats.swarmMax > 1 && otherStats.swarmMax > 1 && pieceStats.name == otherStats.name
  }

  //None means unlimited
  private def numAttacksLimit(attackEffect: Option[TargetEffect], hasFlurry: Boolean): Option[Int] = {
    attackEffect match {
      case None => Some(0)
      case Some(Kill | Unsummon | Enchant(_) | TransformInto(_)) => if(hasFlurry) None else Some(1)
      case Some(Damage(n)) => if(hasFlurry) Some(n) else Some(1)
    }
  }

  //Raises an exception to indicate illegality. Used in tryLegality.
  private def trySpellTargetLegalityExn(spell: Spell, played: PlayedSpellOrAbility): Unit = {
    spell match {
      case (spell: TargetedSpell) =>
        findPiece(played.target0) match {
          case None => fail("No target specified for spell")
          case Some(target) => failUnless(spell.canTarget(side,target), spell.targetError)
        }
      case (spell: TileSpell) =>
        failUnless(tiles.inBounds(played.loc0), "Target location not in bounds")
        failUnless(spell.canTarget(side,tiles(played.loc0),pieces(played.loc0)), spell.targetError)
      case (spell: NoEffectSpell) =>
        ()
    }
  }

  //Raises an exception to indicate illegality. Doesn't test reinforcements (since other things can
  //cause spawns, such as death spawns, this functions handles the most general tests).
  private def trySpawnLegality(spawnSide: Side, spawnStats: PieceStats, spawnLoc: Loc): Unit = {
    failUnless(tiles.inBounds(spawnLoc), "Spawn location not in bounds")

    failUnless(pieces(spawnLoc).forall { other => other.side == spawnSide },
      "Cannot spawn on to spaces with enemies")
    failUnless(canWalkOnTile(spawnStats,tiles(spawnLoc)), "Non-flying pieces cannot spawn over obstacles")

    //Count pieces for swarm
    var minSwarmMax = spawnStats.swarmMax
    pieces(spawnLoc).foreach { other =>
      failUnless(canSwarmTogether(other.curStats,spawnStats), "Piece would spawn in same space as other pieces")
      minSwarmMax = Math.min(minSwarmMax,other.curStats.swarmMax)
    }
    failIf(pieces(spawnLoc).length + 1 > minSwarmMax, "Would exceed maximum allowed swarming pieces in same space")
  }

  //Check if a single action is legal
  private def tryLegalitySingle(action: PlayerAction): Try[Unit] = Try {
    failIf(turnNumber < 0, "Game is not started yet")
    action match {
      case Movements(movements) =>
        def isMovingNow(piece: Piece) = movements.exists { case Movement(id,_) => piece.id == id }
        def pieceIfMovingTo(pieceSpec: PieceSpec, path: Array[Loc], dest: Loc): Option[Piece] =
          if(path.length > 0 && path.last == dest) findPiece(pieceSpec)
          else None

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
              val dest = path.last
              //Ownership
              failUnless(piece.side == side, "Piece controlled by other side")

              //Piece currently at the start of the path
              failUnless(piece.loc == path(0), "Moved piece is not at the head of the path")

              val pieceStats = piece.curStats

              //Piece movement range and state is ok
              failIf(pieceStats.moveRange <= 0, "Piece cannot move")
              piece.actState match {
                case Moving(stepsUsed) =>
                  failUnless(path.length - 1 <= pieceStats.moveRange - stepsUsed, "Movement range of piece is not large enough")
                case Attacking(_) | Spawning | DoneActing =>
                  fail("Piece has already acted or cannot move any more this turn")
              }

              //Check spaces along the path
              path.foreach { loc =>
                //Tiles are all walkable by this piece
                failUnless(canWalkOnTile(pieceStats,tiles(loc)), "Non-flying pieces cannot move over obstacles")
                failUnless(pieceStats.isFlying || pieces(loc).forall { other => other.side == piece.side },
                  "Non-flying pieces cannot move through enemies")
              }
              failUnless(pieces(path.last).forall { other => other.side == piece.side },
                "Cannot move on to spaces with enemies")

              //Count pieces on final destination for swarm
              var piecesOnFinalSquare = 1
              var minSwarmMax = pieceStats.swarmMax
              pieces(dest).foreach { other =>
                if(!isMovingNow(other)) {
                  failUnless(canSwarmTogether(other.curStats,pieceStats), "Piece would end in same space as other pieces")
                  piecesOnFinalSquare += 1
                  minSwarmMax = Math.min(minSwarmMax,other.curStats.swarmMax)
                }
              }
              movements.foreach { case Movement(otherSpec,otherPath) =>
                if(pieceSpec != otherSpec) {
                  pieceIfMovingTo(otherSpec,otherPath,dest).foreach { other =>
                    failUnless(canSwarmTogether(other.curStats,pieceStats), "Piece would end in same space as other pieces")
                    piecesOnFinalSquare += 1
                    minSwarmMax = Math.min(minSwarmMax,other.curStats.swarmMax)
                  }
                }
              }
              failIf(piecesOnFinalSquare > minSwarmMax, "Would exceed maximum allowed swarming pieces in same space")
          }
        }

      case Attack(attackerSpec, targetLoc, targetSpec) =>
        (findPiece(attackerSpec),findPiece(targetSpec)) match {
          case (None, _) => fail("Attacking with a nonexistent or dead piece")
          case (Some(_),None) => fail("Attacking a nonexistent or dead piece")
          case (Some(attacker),Some(target)) =>
            failUnless(attacker.side == side, "Attacker is controlled by other side")
            failUnless(target.side != side, "Target piece is friendly")
            failUnless(tiles.inBounds(targetLoc), "Target location not in bounds")
            failUnless(target.loc == targetLoc, "Target piece is not or is no longer in the right spot")

            val attackerStats = attacker.curStats
            val targetStats = target.curStats
            failIf(attackerStats.attackRange <= 0, "Piece cannot attack")
            failIf(attackerStats.isLumbering && attacker.hasMoved, "Lumbering pieces cannot both move and attack on the same turn")

            //Check attack state + flurry
            attacker.actState match {
              case Moving(_) => ()
              case Attacking(numAttacks) =>
                //Quick termination and slightly more specific err message
                failIf(!attackerStats.hasFlurry && numAttacks > 0, "Piece already attacked")
                //Fuller check
                numAttacksLimit(attackerStats.attackEffect,attackerStats.hasFlurry).foreach { limit =>
                  failIf(numAttacks >= limit, "Piece already assigned all of its attacks")
                }
              case Spawning | DoneActing =>
                fail("Piece has already acted or cannot attack any more this turn")
            }

            failUnless(topology.distance(attacker.loc,targetLoc) <= attackerStats.attackRange, "Attack range not large enough")

            attackerStats.attackEffect match {
              case None => fail("Piece cannot attack")
              case Some(Damage(_)) => ()
              case Some(Kill) => failIf(targetStats.isNecromancer, "Death attacks cannot hurt necromancers")
              case Some(Unsummon) => failIf(targetStats.isPersistent, "Target is persistent - cannot be unsummoned")
              case Some(Enchant(_)) => ()
              case Some(TransformInto(_)) => failIf(targetStats.isNecromancer, "Necromancers cannot be transformed")
            }

            failIf(attackerStats.isWailing && targetStats.isNecromancer, "Wailing pieces cannot hurt necromancers")
            failIf(!attackerStats.canHurtNecromancer && targetStats.isNecromancer, "Piece not allowed to hurt necromancer")
        }

      case Spawn(spawnLoc, spawnStats, freeSpawnerSpec) =>
        //A bunch of tests that don't depend on the spawner or on reinforcements state
        trySpawnLegality(side, spawnStats, spawnLoc)

        def trySpawnUsing(piece: Piece): Try[Unit] = {
          val distance = topology.distance(spawnLoc,piece.loc)
          if(piece.side != side)  Failure(new Exception("Spawner controlled by other side"))
          else if((!spawnStats.isEldritch || distance > 1) && (piece.curStats.spawnRange < distance))  Failure(new Exception("Location not within range of spawner"))
          else if(piece.actState > Spawning)  Failure(new Exception("Spawner cannot act any more this turn"))
          else Success(())
        }

        freeSpawnerSpec match {
          case None =>
            failUnless(pieceById.values.exists { piece => trySpawnUsing(piece).isSuccess }, "No piece with spawn in range")
            failUnless(reinforcements(side).contains(spawnStats), "No such piece in reinforcements")
          case Some(freeSpawnerSpec) =>
            findPiece(freeSpawnerSpec) match {
              case None => fail("Free spawning using a nonexistent or dead piece")
              case Some(freeSpawner) =>
                trySpawnUsing(freeSpawner).get : Unit
                failIf(freeSpawner.hasFreeSpawned, "Piece has already free-spawned")
                failUnless(freeSpawner.curStats.freeSpawn.contains(spawnStats), "Free-spawning piece of the wrong type")
            }
        }

      case SpellsAndAbilities(spellsAndAbilities) =>
        //Ensure no spells played more than once
        val spellIdsPlayed = spellsAndAbilities.flatMap { played => played.spellId }
        failIf(spellIdsPlayed.distinct.length != spellIdsPlayed.length, "Spell played more than once")

        //Check that all spells and abilities are targeted legally
        spellsAndAbilities.foreach { played =>
          (played.pieceSpecAndKey, played.spellId) match {
            case (None, None) => fail("Spell or ability did not specify piece ability or spell id")
            case (Some(_), Some(_)) => fail("Spell or ability specifies both piece ability and spell id")
            case (Some((pieceSpec,key)),None) =>
              findPiece(pieceSpec) match {
                case None => fail("Using ability of a nonexistent or dead piece")
                case Some(piece) =>
                  failUnless(piece.side == side, "Piece controlled by other side")
                  failIf(piece.actState >= DoneActing, "Piece has already acted or cannot act any more this turn")
                  val pieceStats = piece.curStats
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
              spells(side).get(spellId) match {
                case None => fail("Spell not in hand or already played or discarded")
                case Some(spell) => trySpellTargetLegalityExn(spell,played)
              }
          }
        }

        //Check discard/sorcery constraints
        var discardIds: Map[Int,Int] = Map()
        spellsAndAbilities.foreach { played =>
          val isSorcery: Boolean = (played.pieceSpecAndKey, played.spellId) match {
            case (Some((pieceSpec,key)),None) =>
              findPiece(pieceSpec).get.curStats.abilities(key).isSorcery
            case (None, Some(spellId)) =>
              spells(side)(spellId).spellType match {
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
          spells(side).get(discardId) match {
            case None => fail("Discarded spell is not in hand or is already played or discarded")
            case Some(spell) =>
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
  private def doActionSingle(action:PlayerAction): Try[Unit] = tryLegalitySingle(action).map { case () =>
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
            case Moving(n) => nextGoodActState(piece,Moving(n + movement.path.length - 1))
            case Attacking(_) | Spawning | DoneActing => assertUnreachable()
          }
        }
      case Attack(attackerSpec, targetLoc, targetSpec) =>
        val attacker = findPiece(attackerSpec).get
        val target = findPiece(targetSpec).get
        val stats = attacker.curStats
        val attackEffect = stats.attackEffect.get match {
          case Damage(n) => if(stats.hasFlurry) Damage(n) else Damage(1) //flurry hits 1 at a time
          case x => x
        }
        applyEffect(attackEffect,target)
        attacker.hasAttacked = true
        attacker.actState = attacker.actState match {
          case Moving(_) => nextGoodActState(attacker,Attacking(1))
          case Attacking(n) => nextGoodActState(attacker,Attacking(n+1))
          case Spawning | DoneActing => assertUnreachable()
        }
        if(stats.hasBlink)
          blinkPiece(attacker)

      case Spawn(spawnLoc, spawnStats, freeSpawnerSpec) =>
        spawnPieceInternal(side,spawnStats,spawnLoc) match {
          case Success(_: Piece) => ()
          case Failure(_) => assertUnreachable()
        }
        freeSpawnerSpec match {
          case None =>
            reinforcements(side).filterNotFirst { stats => stats == spawnStats }
          case Some(spawnerSpec) =>
            val spawner = findPiece(spawnerSpec).get
            spawner.hasFreeSpawned = true
        }
        val message = "Spawned %s on %s".format(spawnStats.name,spawnLoc.toString())
        addMessage(message,PlayerActionMsgType(side))

      case SpellsAndAbilities(spellsAndAbilities) =>
        //Apply spell effects
        spellsAndAbilities.foreach { played => (played.pieceSpecAndKey, played.spellId) match {
          case (None, None) => assertUnreachable()
          case (Some(_), Some(_)) => assertUnreachable()
          case (Some((pieceSpec,key)),None) =>
            val piece = findPiece(pieceSpec).get
            val pieceStats = piece.curStats
            pieceStats.abilities(key) match {
              case (ability:SelfEnchantAbility) =>
                piece.modsWithDuration = piece.modsWithDuration :+ ability.mod
              case (ability:TargetedAbility) =>
                val target = findPiece(played.target0).get
                applyEffect(ability.effect,target)
            }
          case (None, Some(spellId)) =>
            spells(side)(spellId) match {
              case (spell: NoEffectSpell) => ()
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
          played.discardingId.foreach { id => spells(side) = spells(side) - id }
          played.spellId.foreach { id => spells(side) = spells(side) - id }
        }
    }
  }

  //Find the next good act state of a piece >= the one specified that makes sense given its stats
  private def nextGoodActState(piece: Piece, actState: ActState): ActState = {
    val stats = piece.curStats
    def loop(actState: ActState): ActState = {
      actState match {
        case DoneActing => DoneActing
        case Spawning => Spawning
        case Attacking(numAttacks) =>
          if(stats.attackEffect.isEmpty ||
            stats.attackRange <= 0 ||
            stats.isLumbering && piece.hasMoved ||
            numAttacksLimit(stats.attackEffect, stats.hasFlurry).exists { limit => numAttacks >= limit })
            loop(Spawning)
          else
            Attacking(numAttacks)
        case Moving(numSteps) =>
          if(numSteps >= stats.moveRange)
            loop(Attacking(0))
          else
            Moving(numSteps)
      }
    }
    loop(actState)
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
      case TransformInto(newStats) =>
        removeFromBoard(piece)
        spawnPieceInternal(piece.side,newStats,piece.loc) : Try[Piece] //ignore
        ()
    }
  }

  //Kill a piece if it has enough accumulated damage
  private def killIfEnoughDamage(piece: Piece): Unit = {
    val stats = piece.curStats
    if(piece.damage >= stats.defense)
      killPiece(piece)
  }

  //Kill a piece, for any reason
  private def killPiece(piece: Piece): Unit = {
    removeFromBoard(piece)
    val stats = piece.curStats

    //Rebate mana
    mana(piece.side) += stats.rebate
    totalMana(piece.side) += stats.rebate
    var rebateMessage = ""
    if(stats.rebate != 0)
      rebateMessage = "(rebate " + stats.rebate + ")"

    //Death spawn
    var deathSpawnMessage = ""
    stats.deathSpawn.foreach { deathSpawn =>
      spawnPieceInternal(piece.side,deathSpawn,piece.loc) match {
        case Success(_: Piece) => deathSpawnMessage = " (deathspawn: " + deathSpawn.name + ")"
        case Failure(err) => deathSpawnMessage = " (deathspawn prevented: " + err.getMessage + ")"
      }
    }

    val message = "Killed %s on %s%s%s".format(stats.name,piece.loc.toString(),rebateMessage,deathSpawnMessage)
    addMessage(message,PlayerActionMsgType(side))
  }

  private def unsummonPiece(piece: Piece): Unit = {
    removeFromBoard(piece)
    addReinforcementInternal(piece.side,piece.baseStats)
    val stats = piece.curStats
    val message = "Unsummoned %s on %s".format(stats.name,piece.loc.toString())
    addMessage(message,PlayerActionMsgType(side))
  }

  private def blinkPiece(piece: Piece): Unit = {
    removeFromBoard(piece)
    addReinforcementInternal(piece.side,piece.baseStats)
    val stats = piece.curStats
    val message = "Blinked %s on %s after attack".format(stats.name,piece.loc.toString())
    addMessage(message,PlayerActionMsgType(side))
  }
  private def removeFromBoard(piece: Piece): Unit = {
    pieces(piece.loc) = pieces(piece.loc).filterNot { p => p.id == piece.id }
    pieceById = pieceById - piece.id
    piece.spawnedThisTurn.foreach { spawnedThisTurn => piecesSpawnedThisTurn - spawnedThisTurn }
  }

  //Unlike addReinforcement, doesn't log an event and doesn't produce messages
  private def addReinforcementInternal(side: Side, pieceStats: PieceStats): Unit = {
    reinforcements(side) = reinforcements(side) :+ pieceStats
  }

  //Doesn't log an event and doesn't produce messages, but does check for legality of spawn
  private def spawnPieceInternal(spawnSide: Side, spawnStats: PieceStats, spawnLoc: Loc): Try[Piece] = Try {
    //A bunch of tests that don't depend on the spawner or on reinforcements state
    trySpawnLegality(spawnSide, spawnStats, spawnLoc)

    val nthAtLoc = numPiecesSpawnedThisTurnAt.get(spawnLoc).getOrElse(0)
    val piece = Piece.create(spawnSide, spawnStats, nextPieceId, spawnLoc, nthAtLoc, this)
    pieces(spawnLoc) = pieces(spawnLoc) :+ piece
    pieceById += (piece.id -> piece)
    nextPieceId += 1
    piecesSpawnedThisTurn += (piece.spawnedThisTurn.get -> piece)
    numPiecesSpawnedThisTurnAt += (spawnLoc -> nthAtLoc)
    piece
  }

  private def refreshPieceForStartOfTurn(piece: Piece): Unit = {
    piece.damage = 0
    piece.actState = nextGoodActState(piece,Moving(0))
    piece.hasMoved = false
    piece.hasAttacked = false
    piece.hasFreeSpawned = false
    piece.spawnedThisTurn = None
  }
}

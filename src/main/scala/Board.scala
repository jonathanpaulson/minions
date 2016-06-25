import scala.util.{Try,Success,Failure}
import RichImplicits._
import scala.collection.immutable.Vector

//TODO should try to organize this a code bit more

/**
 * The board and various (often mutable) data structures and types relating directly to it.
 */

//TODO think about how to incorporate generaling actions
/*
sealed trait Action
case class PlayerAction(action: PlayerAction) extends Event
case class GeneralAction(action: GeneralAction) extends Event
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
 * Action:
 * A single action taken by a player.
 * Actions are immutable and their data should be independent of any of the mutable types
 * on the board. That is, it should be meaningful to ask whether an action would be legal
 * on a different board, or if the order of actions were rearranged, etc.
 */
sealed trait PlayerAction
case class Movements(movements: List[Movement]) extends PlayerAction
case class Attack(pieceSpec: PieceSpec, targetLoc: Loc, targetSpec: PieceSpec) extends PlayerAction
case class Spawn(pieceSpec: PieceSpec, spawnLoc: Loc, spawnStats: PieceStats) extends PlayerAction
case class SpellsAndAbilities(spellsAndAbilities: List[PlayedSpellOrAbility]) extends PlayerAction

//Path should also include the starting location of the piece.
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
case class ActionMsgType(side: Side) extends MessageType
case class ExternalMsgType(side: Side) extends MessageType
case object TurnChangeMsgType extends MessageType
case object InternalEventMsgType extends MessageType

/** Tile:
 *  A single tile on the board
 *  Possibly enchanted due to spells. Later in list -> spells were played later.
 */
case class Tile(
  val terrain: Terrain,
  var modsWithDuration: List[PieceModWithDuration]
)

/**
 * Piece:
 * A single piece on the board.
 */
object Piece {
  def create(side: Side, baseStats: PieceStats, id: Int, loc: Loc, nthAtLoc: Int, board: Board): Piece = {
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
  var loc: Loc, //Board is responsible for updating this as the piece moves
  var board: Board,
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
  def copy(newBoard: Board) = {
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
 * Board:
 * The full state of one board of the game!
 */
object Board {
  def create(tiles: Plane[Tile]): Board = {
    new Board(
      tiles = tiles,
      pieces = Plane.create(tiles.xSize,tiles.ySize,tiles.topology,List()),
      pieceById = Map(),
      nextPieceId = 0,
      piecesSpawnedThisTurn = Map(),
      numPiecesSpawnedThisTurnAt = Map(),
      turnNumber = -1, //Indicates that the game hasn't started yet
      reinforcements = SideArray.create(List()),
      spells = SideArray.create(Map()),
      nextSpellId = 0,
      side = S0,
      actionsThisTurn = Vector(),
      actionsTotal = Vector(),
      messages = Vector(),
      mana = SideArray.create(0),
      totalMana = SideArray.create(0),
      totalCosts = SideArray.create(0)
    )
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
import Board.Imports._

class Board private (
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

  //Number of turns completed, or -1 if game unstarted
  var turnNumber: Int,

  //List of all reinforcement pieces in hand
  val reinforcements: SideArray[List[PieceStats]],

  //List of all spells in hand, indexed by spellID
  val spells: SideArray[Map[Int,Spell]],
  var nextSpellId: Int, //Counter for assigning per-board unique ids to spells

  //Current side to move
  var side: Side,

  //All actions taken by the side to move this turn
  var actionsThisTurn: Vector[List[PlayerAction]],
  //All actions over the whole history of the board
  var actionsTotal: Vector[List[PlayerAction]],

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

  //Transient, used only for message logging and is set by external code prior to calling
  //functions to modify board state
  var timeLeft: Double = 0.0
  def setTimeLeft(t: Double) = {
    timeLeft = t
  }

  def copy(): Board = {
    val newBoard = new Board(
      tiles = tiles.copy(),
      pieces = pieces.copy(),
      pieceById = Map(), //Set below after construction
      nextPieceId = nextPieceId,
      piecesSpawnedThisTurn = Map(), //Set below after construction
      numPiecesSpawnedThisTurnAt = numPiecesSpawnedThisTurnAt,
      turnNumber = turnNumber,
      reinforcements = reinforcements.copy(),
      spells = spells.copy(),
      nextSpellId = nextSpellId,
      side = side,
      actionsThisTurn = actionsThisTurn,
      actionsTotal = actionsTotal,
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

  //Perform a sequence of actions, stopping on before the first illegal action, if any
  def doActions(actions: Seq[PlayerAction]): Try[Unit] = {
    var goodActions: List[PlayerAction] = List()
    val error = actions.findMap { action =>
      doActionSingle(action) match {
        case Success(()) => goodActions = action :: goodActions; None
        case Failure(err) => Some(Failure(err))
      }
    }
    goodActions = goodActions.reverse
    actionsThisTurn = actionsThisTurn :+ goodActions
    actionsTotal = actionsTotal :+ goodActions
    error.getOrElse(Success())
  }

  //End the current turn and begin the next turn. Called also at the start of the game just after setup
  def beginNextTurn(): Unit = {
    //Count and accumulate mana, except on the start of game. Wailing units do generate mana
    var newMana = 0
    if(turnNumber != -1)
    {
      pieceById.values.foreach { piece =>
        if(piece.side == side)
          newMana += piece.curStats.extraMana + (if(tiles(piece.loc).terrain == ManaSpire) 1 else 0)
      }
    }

    //Wailing units that attacked die
    val attackedWailings = pieceById.iterator.filter { case (pieceId,piece) =>
      piece.curStats.isWailing && piece.hasAttacked
    }
    attackedWailings.toList.foreach { case (_pieceId,piece) => killPiece(piece) }

    //Heal damage, reset piece state, decay modifiers
    pieceById.values.foreach { piece =>
      piece.damage = 0
      piece.actState = nextGoodActState(piece,Moving(0))
      piece.hasMoved = false
      piece.hasAttacked = false
      piece.hasFreeSpawned = false
      piece.spawnedThisTurn = None
      piece.modsWithDuration = piece.modsWithDuration.flatMap(_.decay)
    }
    //Decay tile modifiers
    tiles.foreach { tile => tile.modsWithDuration = tile.modsWithDuration.flatMap(_.decay) }

    mana(side) += newMana
    totalMana(side) += newMana

    if(turnNumber == -1) {
      val message = "New board started"
      addMessage(message,TurnChangeMsgType)
    }
    if(turnNumber != -1) {
      //TODO put team or player names into here?
      val message = "%s turn ended, generated %d mana (board lifetime %d, costs %d, net %d)".format(
        side.toString(),newMana,totalMana(side),totalCosts(side),totalMana(side) - totalCosts(side)
      )
      addMessage(message,TurnChangeMsgType)
    }

    //Flip turn
    side = side.opp
    turnNumber += 1

    //Clear mana for the side to move, and other board state
    mana(side) = 0
    piecesSpawnedThisTurn = Map()
    numPiecesSpawnedThisTurnAt = Map()
  }

  def addReinforcement(side: Side, pieceStats: PieceStats): Unit = {
    //TODO
  }

  def addSpell(side: Side, spell: Spell): Unit = {
    //TODO
  }

  //Directly spawn a piece if it possible to do so. Exposed for use to set up initial boards.
  def spawnPieceInitial(side: Side, pieceStats: PieceStats, loc: Loc): Try[Unit] = {
    if(turnNumber != -1)
      Failure(new Exception("Can only spawn pieces initially before the game has started"))
    else
      spawnPieceInternal(side,pieceStats,loc)
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
  private def trySpawnLegality(spawnSide: Side, spawnStats: PieceStats, spawnLoc: Loc) {
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
          failUnless((1 to (path.length - 1)).forall { idx => topology.distance(path.last,path(idx)) == 1 },
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

      case Attack(pieceSpec, targetLoc, targetSpec) =>
        (findPiece(pieceSpec),findPiece(targetSpec)) match {
          case (None, _) => fail("Attacking with a nonexistent or dead piece")
          case (Some(_),None) => fail("Attacking a nonexistent or dead piece")
          case (Some(piece),Some(target)) =>
            failUnless(piece.side == side, "Piece controlled by other side")
            failUnless(target.side != side, "Target piece is friendly")
            failUnless(tiles.inBounds(targetLoc), "Target location not in bounds")
            failUnless(target.loc == targetLoc, "Target piece is not or is no longer in the right spot")

            val pieceStats = piece.curStats
            val targetStats = target.curStats
            failIf(pieceStats.attackRange <= 0, "Piece cannot attack")
            failIf(pieceStats.isLumbering && piece.hasMoved, "Lumbering pieces cannot both move and attack on the same turn")

            //Check attack state + flurry
            piece.actState match {
              case Moving(_) => ()
              case Attacking(numAttacks) =>
                //Quick termination and slightly more specific err message
                failIf(!pieceStats.hasFlurry && numAttacks > 0, "Piece already attacked")
                //Fuller check
                numAttacksLimit(pieceStats.attackEffect,pieceStats.hasFlurry).foreach { limit =>
                  failIf(numAttacks >= limit, "Piece already assigned all of its attacks")
                }
              case Spawning | DoneActing =>
                fail("Piece has already acted or cannot attack any more this turn")
            }

            failUnless(topology.distance(piece.loc,targetLoc) <= pieceStats.attackRange, "Attack range not large enough")

            pieceStats.attackEffect match {
              case None => fail("Piece cannot attack")
              case Some(Damage(_)) => ()
              case Some(Kill) => failIf(targetStats.isNecromancer, "Death attacks cannot hurt necromancers")
              case Some(Unsummon) => failIf(targetStats.isPersistent, "Target is persistent - cannot be unsummoned")
              case Some(Enchant(_)) => ()
              case Some(TransformInto(_)) => failIf(targetStats.isNecromancer, "Necromancers cannot be transformed")
            }

            failIf(pieceStats.isWailing && targetStats.isNecromancer, "Wailing pieces cannot hurt necromancers")
            failIf(!pieceStats.canHurtNecromancer && targetStats.isNecromancer, "Piece not allowed to hurt necromancer")
        }

      case Spawn(pieceSpec, spawnLoc, spawnStats) =>
        findPiece(pieceSpec) match {
          case None => fail("Spawning using a nonexistent or dead piece")
          case Some(piece) =>
            val pieceStats = piece.curStats
            failUnless(piece.side == side, "Piece controlled by other side")
            failIf(pieceStats.spawnRange <= 0 && !spawnStats.isEldritch, "Piece cannot spawn")

            //A bunch of tests that don't depend on the spawner or on reinforcements state
            trySpawnLegality(side, spawnStats, spawnLoc)

            failUnless(topology.distance(piece.loc,spawnLoc) <= Math.max(pieceStats.spawnRange,1), "Spawn range not large enough")

            val isFreeSpawn = !piece.hasFreeSpawned && pieceStats.freeSpawn.contains(spawnStats)
            failUnless(isFreeSpawn || reinforcements(side).contains(spawnStats), "No such piece in reinforcements")

            failUnless(piece.actState <= Spawning, "Piece cannot spawn any more this turn")
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
      case Attack(pieceSpec, targetLoc, targetSpec) =>
        val piece = findPiece(pieceSpec).get
        val target = findPiece(targetSpec).get
        val stats = piece.curStats
        val attackEffect = stats.attackEffect.get match {
          case Damage(n) => if(stats.hasFlurry) Damage(n) else Damage(1) //flurry hits 1 at a time
          case x => x
        }
        applyEffect(attackEffect,target)
        piece.hasAttacked = true
        piece.actState = piece.actState match {
          case Moving(_) => nextGoodActState(piece,Attacking(1))
          case Attacking(n) => nextGoodActState(piece,Attacking(n+1))
          case Spawning | DoneActing => assertUnreachable()
        }
        if(stats.hasBlink)
          blinkPiece(piece)

      case Spawn(pieceSpec, spawnLoc, spawnStats) =>
        val piece = findPiece(pieceSpec).get
        if(!piece.hasFreeSpawned && piece.curStats.freeSpawn.contains(spawnStats))
          piece.hasFreeSpawned = true
        else {
          var first = true
          reinforcements(side).filterNotFirst { stats => stats == spawnStats }
        }
        spawnPieceInternal(side,spawnStats,spawnLoc) match {
          case Success(()) => ()
          case Failure(_) => assertUnreachable()
        }
        val message = "Spawned %s on %s".format(spawnStats.name,spawnLoc.toString())
        addMessage(message,ActionMsgType(side))

      case SpellsAndAbilities(spellsAndAbilities) =>
        //TODO
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
      case TransformInto(newStats) =>
        removeFromBoard(piece)
        spawnPieceInternal(piece.side,newStats,piece.loc)
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
        case Success(()) => deathSpawnMessage = " (deathspawn: " + deathSpawn.name + ")"
        case Failure(err) => deathSpawnMessage = " (deathspawn prevented: " + err.getMessage + ")"
      }
    }

    val message = "Killed %s on %s%s%s".format(stats.name,piece.loc.toString(),rebateMessage,deathSpawnMessage)
    addMessage(message,ActionMsgType(side))
  }

  private def unsummonPiece(piece: Piece): Unit = {
    removeFromBoard(piece)
    addReinforcementInternal(piece.side,piece.baseStats)
    val stats = piece.curStats
    val message = "Unsummoned %s on %s".format(stats.name,piece.loc.toString())
    addMessage(message,ActionMsgType(side))
  }

  private def blinkPiece(piece: Piece): Unit = {
    removeFromBoard(piece)
    addReinforcementInternal(piece.side,piece.baseStats)
    val stats = piece.curStats
    val message = "Blinked %s on %s after attack".format(stats.name,piece.loc.toString())
    addMessage(message,ActionMsgType(side))
  }
  private def removeFromBoard(piece: Piece): Unit = {
    pieces(piece.loc) = pieces(piece.loc).filterNot { p => p.id == piece.id }
    pieceById = pieceById - piece.id
    piece.spawnedThisTurn.foreach { spawnedThisTurn => piecesSpawnedThisTurn - spawnedThisTurn }
  }

  //Unlike addReinforcement, doesn't log an event and doesn't produce messages
  private def addReinforcementInternal(side: Side, pieceStats: PieceStats): Unit = {
    this.reinforcements.update(side, pieceStats :: this.reinforcements(side))
  }

  //Doesn't log an event and doesn't produce messages, but does check for legality of spawn
  private def spawnPieceInternal(spawnSide: Side, spawnStats: PieceStats, spawnLoc: Loc): Try[Unit] = Try {
    //A bunch of tests that don't depend on the spawner or on reinforcements state
    trySpawnLegality(spawnSide, spawnStats, spawnLoc)

    val nthAtLoc = numPiecesSpawnedThisTurnAt.get(spawnLoc).getOrElse(0)
    val piece = Piece.create(spawnSide, spawnStats, nextPieceId, spawnLoc, nthAtLoc, this)
    pieces(spawnLoc) = pieces(spawnLoc) :+ piece
    pieceById += (piece.id -> piece)
    nextPieceId += 1
    piecesSpawnedThisTurn += (piece.spawnedThisTurn.get -> piece)
    numPiecesSpawnedThisTurnAt += (spawnLoc -> nthAtLoc)
  }
}




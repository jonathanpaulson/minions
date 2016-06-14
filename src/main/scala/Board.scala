import scala.util.Try
import RichImplicits._

/**
 * The board and various (often mutable) data structures and types relating directly to it.
 */

/**
 * Action:
 * A single action taken by a player.
 * Actions are immutable and their data should be independent of any of the mutable types
 * on the board. That is, it should be meaningful to ask whether an action would be legal
 * on a different board, or if the order of actions were rearranged, etc.
 */
sealed trait Action
case class Movements(movements: List[Movement]) extends Action
case class Attack(pieceId: Int, targetLoc: Loc, targetId: Int) extends Action
case class Spawn(pieceId: Int, spawnLoc: Loc, spawnStats: PieceStats) extends Action
case class SpellsAndAbilities(spellsAndAbilities: List[PlayedSpellOrAbility]) extends Action

//Path should include the starting location of the piece.
case class Movement(pieceId: Int, path: Array[Loc])

//Data for a played spell or piece ability.
//Since different spells and abilities affect different things and have different numbers
//of targets, not all fields may be applicable.
case class PlayedSpellOrAbility(
  val pieceIdAndKey: Option[(Int,String)], //Some if it was a piece, (pieceId, key of ability)
  val spellId: Option[Int], //Some if it was a spell
  val target0: Int,
  val target1: Int,
  val loc0: Loc,
  //Spell ids that were discarded to power this spell or ability, if it was a sorcery
  val discardingId: Option[Int]
)

/**
 * Piece:
 * A single piece on the board.
 */
object Piece {
  def create(side: Side, baseStats: PieceStats, id: Int, loc: Loc, board: Board): Piece = {
    new Piece(
      side = side,
      baseStats = baseStats,
      id = id,
      loc = loc,
      board = board,
      modsWithDuration = List(),
      damage = 0,
      actState = Moving(0),
      hasMoved = false,
      hasAttacked = false,
      hasFreeSpawned = false
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
  var hasFreeSpawned: Boolean
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
      hasFreeSpawned = hasFreeSpawned
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
      turnNumber = 0,
      reinforcements = SideArray.create(List()),
      spells = SideArray.create(Map()),
      nextSpellId = 0,
      side = S0,
      revActionsThisTurn = List()
    )
  }
}

class Board private (
  //Tiles of the board
  val tiles: Plane[Tile],
  //List of pieces in each space. Order is irrelevant
  val pieces: Plane[List[Piece]],

  //Map of all pieces currently on the board by pieceId.
  var pieceById: Map[Int,Piece],
  var nextPieceId: Int, //Counter for assigning per-board unique ids to pieces

  //Number of turns completed
  var turnNumber: Int,

  //List of all reinforcement pieces in hand
  val reinforcements: SideArray[List[PieceStats]],

  //List of all spells in hand, indexed by spellID
  val spells: SideArray[Map[Int,Spell]],
  var nextSpellId: Int, //Counter for assigning per-board unique ids to spells

  //Current side to move
  var side: Side,

  //List of all actions taken by the side to move this turn, later actions in list taken earlier
  var revActionsThisTurn: List[Action]
) {
  val xSize = tiles.xSize
  val ySize = tiles.ySize
  val topology = tiles.topology

  def copy(): Board = {
    val newBoard = new Board(
      tiles = tiles.copy(),
      pieces = pieces.copy(),
      pieceById = Map(),
      nextPieceId,
      turnNumber,
      reinforcements = reinforcements.copy(),
      spells = spells.copy(),
      nextSpellId = nextSpellId,
      side = side,
      revActionsThisTurn = revActionsThisTurn
    )
    val newPieceById = pieceById.mapValues { piece => piece.copy(newBoard) }
    newBoard.pieces.transform { pieceList => pieceList.map { piece => newPieceById(piece.id) } }
    newBoard
  }

  def tryLegality(action: Action): Try[Unit] = Try {
    def failUnless(b: Boolean, message: String) =
      if(!b) throw new Exception(message)
    def failIf(b: Boolean, message: String) =
      if(b) throw new Exception(message)
    def fail(message: String) =
      throw new Exception(message)

    action match {
      case Movements(movements) =>
        def isMovingNow(piece: Piece) = movements.exists { case Movement(id,_) => piece.id == id }
        def pieceIfMovingTo(pieceId: Int, path: Array[Loc], dest: Loc): Option[Piece] =
          if(path.length > 0 && path.last == dest) pieceById.get(pieceId)
          else None

        //Check basic properties of the set of movements
        val pieceIdsMoved = movements.map { case Movement(pieceId,_) => pieceId }
        failIf(pieceIdsMoved.distinct.length != movements.length, "More than one movement for the same piece")
        failIf(movements.isEmpty, "Empty set of movements")

        movements.foreach { case Movement(pieceId,path) =>
          //Check basic properties of path
          failUnless(path.length > 1, "Empty or trivial movement path")
          failUnless(path(0) != path.last, "Circular movement path")
          failUnless(path.forall { loc => tiles.inBounds(loc) }, "Movement out of bounds")
          failUnless((1 to (path.length - 1)).forall { idx => topology.distance(path.last,path(idx)) == 1 },
              "Movement path locations not all adjacent")

          pieceById.get(pieceId) match {
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
              movements.foreach { case Movement(otherId,otherPath) =>
                if(otherId != pieceId) {
                  pieceIfMovingTo(otherId,otherPath,dest).foreach { other =>
                    failUnless(canSwarmTogether(other.curStats,pieceStats), "Piece would end in same space as other pieces")
                    piecesOnFinalSquare += 1
                    minSwarmMax = Math.min(minSwarmMax,other.curStats.swarmMax)
                  }
                }
              }
              failIf(piecesOnFinalSquare > minSwarmMax, "Would exceed maximum allowed swarming pieces in same space")
          }
        }

      case Attack(pieceId, targetLoc, targetId) =>
        (pieceById.get(pieceId),pieceById.get(targetId)) match {
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
                numAttacksLimit(pieceStats,pieceStats.hasFlurry).foreach { limit =>
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

            failIf(pieceStats.isWailing && targetStats.isNecromancer, "Wailing pieces cannot to hurt necromancers")
            failIf(!pieceStats.canHurtNecromancer && targetStats.isNecromancer, "Piece not allowed to hurt necromancer")
        }

      case Spawn(pieceId, spawnLoc, spawnStats) =>
        pieceById.get(pieceId) match {
          case None => fail("Spawning using a nonexistent or dead piece")
          case Some(piece) =>
            val pieceStats = piece.curStats
            failUnless(piece.side == side, "Piece controlled by other side")
            failUnless(pieceStats.spawnRange > 0, "Piece cannot spawn")
            failUnless(tiles.inBounds(spawnLoc), "Spawn location not in bounds")
            failUnless(topology.distance(piece.loc,spawnLoc) <= pieceStats.spawnRange, "Spawn range not large enough")

            val isFreeSpawn = !piece.hasFreeSpawned && pieceStats.freeSpawn.contains(spawnStats)
            failUnless(isFreeSpawn || reinforcements(side).contains(spawnStats), "No such piece in reinforcements")

            failUnless(piece.actState <= Spawning, "Piece cannot spawn any more this turn")

            failUnless(pieces(spawnLoc).forall { other => other.side == piece.side },
                "Cannot spawn on to spaces with enemies")
            failUnless(canWalkOnTile(spawnStats,tiles(spawnLoc)), "Non-flying pieces cannot spawn over obstacles")
        }

      case SpellsAndAbilities(spellsAndAbilities) =>
        //Ensure no spells played more than once
        val spellIdsPlayed = spellsAndAbilities.flatMap { played => played.spellId }
        failIf(spellIdsPlayed.distinct.length != spellIdsPlayed.length, "Spell played more than once")

        //Check that all spells and abilities are targeted legally
        spellsAndAbilities.foreach { played =>
          (played.pieceIdAndKey, played.spellId) match {
            case (None, None) => fail("Spell or ability did not specify piece ability or spell id")
            case (Some(_), Some(_)) => fail("Spell or ability specifies both piece ability and spell id")
            case (Some((pieceId,key)),None) =>
              pieceById.get(pieceId) match {
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
                      pieceById.get(played.target0) match {
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
          val isSorcery: Boolean = (played.pieceIdAndKey, played.spellId) match {
            case (Some((pieceId,key)),None) =>
              pieceById(pieceId).curStats.abilities(key).isSorcery
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


case class Movements(movements: List[Movement]) extends Action
case class Attack(pieceId: Int, targetLoc: Loc, targetId: Int) extends Action
case class Spawn(pieceId: Int, spawnLoc: Loc, spawnStats: PieceStats) extends Action
case class SpellsAndAbilities(spellsAndAbilities: List[PlayedSpellOrAbility]) extends Action

case class PlayedSpellOrAbility(
  val pieceIdAndKey: Option[(Int,String)], //Some if it was a piece, (pieceId, key of ability)
  val spellId: Option[Int], //Some if it was a spell
  val target0: Int,
  val target1: Int,
  val loc0: Loc,
  //Spell ids that were discarded to power this spell or ability, if it was a sorcery
  val discardingId: Option[Int]
)


  def doAction(action:Action): Try[Unit] = tryLegality(action).map { case () =>
    action match {
      case Movements(movements) =>
        movements.foreach { movement =>
          val piece = pieceById(movement.pieceId)
          val src = movement.path(0)
          val dest = movement.path.last
          pieces(src) = pieces(src).filterNot { p => p.id == piece.id }
          pieces(dest) = pieces(dest) :+ piece
          piece.loc = dest
          piece.hasMoved = true
          piece.actState = piece.actState match {
            case Moving(steps) =>
              val newSteps = steps + movement.path.length - 1
              nextGoodActState(piece,Moving(newSteps))
            case Attacking(_) | Spawning | DoneActing => assertUnreachable()
          }
        }
      case Attack(pieceId, targetLoc, targetId) =>
      case Spawn(pieceId, spawnLoc, spawnStats) =>
      case SpellsAndAbilities(spellsAndAbilities) =>
    }
  }

  //Find the next good act state of a piece >= the one specified that makes sense given its stats
  def nextGoodActState(piece: Piece, actState: ActState): ActState = {
    val stats = piece.curStats
    def loop(actState: ActState): ActState = {
      actState match {
        case DoneActing => DoneActing
        case Spawning =>
          if(stats.spawnRange <= 0) DoneActing
          else                      Spawning
        case Attacking(numAttacks) =>
          if(stats.attackEffect.isEmpty ||
             stats.attackRange <= 0 ||
             (!stats.hasFlurry && numAttacks > 0) ||
             (stats.hasFlurry && numAttacks > stats.attackEffect


    while(state < DoneActing)
    state match {
      case Moving(steps) =>
        if
  }

  def wouldBeLegal(actions: Seq[Action]): Boolean = {
    //TODO
  }

  def endTurn(): Unit = {
    //TODO
  }

  def addReinforcement(side: Side, pieceStats: PieceStats): Unit = {
    //TODO
  }

  def addSpell(side: Side, spell: Spell): Unit = {
    //TODO
  }

  def initialSpawn(side: Side, pieceStats: PieceStats, loc: Loc): Unit = {
    //TODO
  }


  //HELPER FUNCTIONS -------------------------------------------------------------------------------------

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
  private def numAttacksLimit(attackEffect: TargetEffect, hasFlurry: Boolean): Option[Int] = {
    attackEffect match {
      case None => Some(0)
      case Some(Kill | Unsummon | Enchant(_) | TransformInto(_)) => if(hasFlurry) None else Some(1)
      case Some(Damage(n)) => if(hasFlurry) Some(n) else Some(1)
    }
  }

  //Raises an exception to indicate illegality. Used in tryLegality.
  private def trySpellTargetLegalityExn(spell: Spell, played: PlayedSpellOrAbility): Unit = {
    def failUnless(b: Boolean, message: String) =
      if(!b) throw new Exception(message)
    def failIf(b: Boolean, message: String) =
      if(b) throw new Exception(message)
    def fail(message: String) =
      throw new Exception(message)

    spell match {
      case (spell: TargetedSpell) =>
        pieceById.get(played.target0) match {
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

}




package minionsgame.core

import scala.reflect.ClassTag

import RichImplicits._

/**
 * A whole bunch of standard types for the game.
 * Mostly immutable, except for a few obvious array-like types (SideArray, Plane).
 */

/**
 * Side:
 * Side to move (i.e. minions is a 1 team vs 1 team game).
 */
sealed trait Side {
  val int: Int
  def opp: Side = this match { case S0 => S1  case S1 => S0 }
}
case object S0 extends Side { val int = 0 }
case object S1 extends Side { val int = 1 }

/**
 * TargetEffect:
 * The effects of an attack or a spell on a target piece.
 */
sealed trait TargetEffect
case class Damage(damage: Int) extends TargetEffect
case object Unsummon extends TargetEffect
case object Kill extends TargetEffect
case class Enchant(modWithDuration: PieceModWithDuration) extends TargetEffect
case class TransformInto(newStats: PieceStats) extends TargetEffect

/**
 * ActState:
 * Current status of piece during a turn, representing the basic structure move => attack => spawn
 * For purpose of ordering/comparison, greater states are those that come after earlier states.
 */
sealed trait ActState extends Ordered[ActState] {
  val order: Int
  def compare(that: ActState) : Int = {
    (this,that) match {
      case (Moving(x),Moving(y)) => x.compare(y)
      case (Attacking(x),Attacking(y)) => x.compare(y)
      case (_,_) => this.order.compare(that.order)
    }
  }
}
case class Moving(val steps: Int) extends ActState { val order = 0 }
case class Attacking(val strikes: Int) extends ActState { val order = 1 }
case object Spawning extends ActState { val order = 2 }
case object DoneActing extends ActState { val order = 3 }
object ActState {
  val start = Moving(0)
}

/**
 * PieceStats:
 * All the immutable stats of a piece. Also used to represent pieces in reinforcements (i.e. in hand).
 */
case class PieceStats(
  //name is for internal identification and use in recorded files and such.
  //displayName is for the GUI.
  //The separation of these two allows us to make graphical and cosmetic changes to things like piece names without
  //invalidating recorded games (although of course there's no help for modifications to piece stats that affect gameplay!)
  val name: String,
  val displayName: String,

  val attackEffect: Option[TargetEffect],
  val defense: Int,
  val moveRange: Int,
  val attackRange: Int,

  val cost: Int,
  val rebate: Int,

  val isNecromancer: Boolean,
  val isFlying: Boolean,     //Can move over water, opposing enemies
  val isLumbering: Boolean,  //Cannot move and attack on the same turn
  val isPersistent: Boolean, //Cannot be unsummoned (sent back to reinforcements/hand)
  val isEldritch: Boolean,   //Can spawn next to any unit
  val isWailing: Boolean,    //At the end of turn if it attacked, piece dies
  val hasFlurry: Boolean,    //Attack can be divided between any number of pieces
  val hasBlink: Boolean,     //Piece unsummons immediately after attacking
  val canHurtNecromancer: Boolean, //Piece not allowed to attack necromancer

  val swarmMax: Int,   //Number of copies of piece with same name that can occupy a space
  val spawnRange: Int, //Radius at which this unit can spawn reinforcements
  val extraMana: Int,  //Mana generated by this piece per turn

  val deathSpawn: Option[PieceStats], //Automatic spawn upon death

  //Abilities that a piece can use by discarding a spell
  val abilities: Map[String,PieceAbility]
)

/**
 * PieceMod:
 * Modifications to piece stats that can happen due to spells, enchanted terrain, abilities.
 * Comparison is done using the key field only, which we rely on as a key to distinguish effects
 * since functions are not comparable in Scala.
 */
case class PieceMod(
  val key: String,  //MUST be a UNIQUE key for different modifiers!
  val name: String, //For display purposes
  val desc: String,
  val transform: (PieceStats => PieceStats)
) {
  def apply(pieceStats: PieceStats) = transform(pieceStats)

  override def equals(o: Any): Boolean = o match {
    case that: PieceMod => this.key == that.key
    case _ => false
  }
  override def hashCode: Int = key.hashCode
}

/**
 * PieceModWithDuration:
 * Essentially a tuple of a PieceMod and a number of turns that it lasts.
 */
case class PieceModWithDuration(
  val mod: PieceMod,
  val turnsLeft: Option[Int] //Counts down on EACH side's turn. None = Permanent
) {
  //Decay turnsLeft by one turn, returning None if the PieceMod decays away entirely.
  def decay: Option[PieceModWithDuration] = {
    turnsLeft match {
      case None => Some(this)
      case Some(turnsLeft) =>
        if(turnsLeft <= 1) None
        else Some(PieceModWithDuration(mod,Some(turnsLeft-1)))
    }
  }
}

/**
 * PieceAbility:
 * Ability that pieces can use if.
 * Comparison is done using the key field only, which we rely on as a key to distinguish abilities
 * since functions are not comparable in Scala.
 */
sealed trait PieceAbility {
  val key: String  //MUST be a UNIQUE key for different modifiers!
  val name: String //For display purposes
  val desc: String
  val isSorcery: Boolean //Requires a discarded spell
  val isUsableNow: Piece => Boolean
  val unusableError: String //Error message when not usable now

  override def equals(o: Any): Boolean = o match {
    case that: PieceAbility => this.key == that.key
    case _ => false
  }
  override def hashCode: Int = key.hashCode
}

//Discard abilities that target the piece itself
case class SelfEnchantAbility(
  val key: String,  //MUST be a UNIQUE key for different modifiers!
  val name: String,
  val desc: String,
  val isSorcery: Boolean,
  val isUsableNow: Piece => Boolean,
  val unusableError: String, //Error message when not usable now
  val mod: PieceModWithDuration
) extends PieceAbility {
  override def equals(o: Any): Boolean = super.equals(o)
  override def hashCode: Int = super.hashCode
}
//Discard abilities that target another piece
case class TargetedAbility(
  val key: String,  //MUST be a UNIQUE key for different modifiers!
  val name: String,
  val desc: String,
  val isSorcery: Boolean,
  val isUsableNow: Piece => Boolean,
  val unusableError: String, //Error message when not usable now
  val canTarget: (Piece, Piece) => Boolean, //(piece, target)
  val targetError: String, //Error message when target not legal
  val effect: TargetEffect
) extends PieceAbility {
  override def equals(o: Any): Boolean = super.equals(o)
  override def hashCode: Int = super.hashCode
}

/**
 * SpellType:
 * The type of a spell. Normal spells are just played as usual. Sorceries require discarding a spell.
 * Cantrips if discarded for a sorcery or ability can be played for their effect.
 * DoubleCantrips are cantrips that power two sorceries.
 */
sealed trait SpellType
case object NormalSpell extends SpellType
case object Sorcery extends SpellType
case object Cantrip extends SpellType
case object DoubleCantrip extends SpellType

/**
 * Spell:
 * A spell that necromancers can hold in hand and play.
 * Comparison is done using the key field only, which we rely on as a key to distinguish abilities
 * since functions are not comparable in Scala.
 */
sealed trait Spell {
  val key: String //MUST be a UNIQUE key for different spells!
  val name: String
  val desc: String
  val spellType: SpellType

  override def equals(o: Any): Boolean = o match {
    case that: Spell => this.key == that.key
    case _ => false
  }
  override def hashCode: Int = key.hashCode
}

/**
 * NoEffectSpell:
 * A spell that has no effect (exists only to be discarded)
 */
case class NoEffectSpell(
  val key: String,  //MUST be a UNIQUE key for different modifiers!
  val name: String,
  val desc: String,
  val spellType: SpellType
) extends Spell {
  override def equals(o: Any): Boolean = super.equals(o)
  override def hashCode: Int = super.hashCode
}

/**
 * TargetedSpell:
 * Targets a single piece and applies an effect.
 */
case class TargetedSpell(
  val key: String,  //MUST be a UNIQUE key for different modifiers!
  val name: String,
  val desc: String,
  val spellType: SpellType,
  val canTarget: (Side, Piece) => Boolean, //(spell caster side, target)
  val targetError: String, //Error message when target not legal
  val effect: TargetEffect
) extends Spell {
  override def equals(o: Any): Boolean = super.equals(o)
  override def hashCode: Int = super.hashCode
}

/**
 * TileSpell:
 * Targets a single tile and applies an effect.
 */
case class TileSpell(
  val key: String,  //MUST be a UNIQUE key for different modifiers!
  val name: String,
  val desc: String,
  val spellType: SpellType,
  val canTarget: (Side, Tile, List[Piece]) => Boolean, //(spell caster side, tile, pieces on tile)
  val targetError: String, //Error message when target not legal
  val effect: Tile => Tile
) extends Spell {
  override def equals(o: Any): Boolean = super.equals(o)
  override def hashCode: Int = super.hashCode
}


/**
 * Terrain:
 * The type of terrain of a single space on the board.
 */
sealed trait Terrain
case object Wall extends Terrain
case object Ground extends Terrain
case object Water extends Terrain
case object ManaSpire extends Terrain
//TODO implement this
case class Spawner(side:Side, pieceStats:PieceStats) extends Terrain
//TODO implement this
//case class UnstableGround extends Terrain  //Units unsummon at the end of turn

/**
 * Loc, Vec:
 * Basic integer points and vectors
 */
case class Vec(dx: Int, dy:Int) extends Ordered[Vec] {
  def +(v: Vec) = Vec(dx+v.dx, dy+v.dy)
  def -(v: Vec) = Vec(dx-v.dx, dy-v.dy)
  def +(l: Loc) = Loc(dx+l.x, dy+l.y)
  def compare(that: Vec): Int = Ordering[(Int,Int)].compare((dx,dy),(that.dx,that.dy))
}

case class Loc(x:Int, y:Int) {
  def +(v: Vec) = Loc(x+v.dx, y+v.dy)
  def -(l: Loc) = Vec(x-l.x, y-l.y)
  def compare(that: Loc): Int = Ordering[(Int,Int)].compare((x,y),(that.x,that.y))

  override def toString: String = {
    Loc.xCoordString(x) + Loc.yCoordString(y)
  }
}
object Loc {
  def xCoordString(x: Int): String = {
    if(x < 0)
      x.toString()
    else {
      var xx = x+1
      var s = ""
      while(xx > 0) {
        s = s + ('a' + (xx % 26)).toChar
        xx = xx / 26
      }
      s
    }
  }
  def yCoordString(y: Int): String = {
    y.toString()
  }
}

/**
 * SideArray:
 * Length-two array indexed by Side.
 */
object SideArray {
  def create[T:ClassTag](initial: T) = new SideArray[T](Array.fill[T](2)(initial))
}
class SideArray[T:ClassTag] private (
  private val arr: Array[T]
) {
  def apply(s:Side): T = arr(s.int)
  def update(s:Side, elt: T): Unit = arr(s.int) = elt

  def copy(): SideArray[T] = new SideArray[T](arr.clone())

  def map[U:ClassTag](f: T => U): SideArray[U] = new SideArray[U](arr.map(f))
  def foreach(f: T => Unit): Unit = arr.foreach(f)
  def foldLeft[U](z: U)(f: (U,T) => U) = arr.foldLeft(z)(f)
  def find(f: T => Boolean): Option[T] = arr.find(f)
  def findMap[U](f: T => Option[U]): Option[U] = arr.findMap(f)
  def transform(f: T => T): Unit = { arr.transform(f); () }
}

/**
 * Plane:
 * 2-dimensional array with configurable topology
 */
object Plane {
  def create[T:ClassTag](
    xSize: Int,
    ySize: Int,
    topology: PlaneTopology,
    initial: T
  ) : Plane[T] = {
    new Plane(xSize,ySize,topology,Array.fill[T](xSize,ySize)(initial))
  }
}
class Plane[T:ClassTag] private (
  val xSize: Int,
  val ySize: Int,
  val topology: PlaneTopology,
  private val arr: Array[Array[T]]
) {
  def apply(x:Int, y:Int): T = arr(x)(y)
  def apply(loc: Loc): T = arr(loc.x)(loc.y)
  def update(x:Int, y:Int, elt: T): Unit = arr(x)(y) = elt
  def update(loc: Loc, elt: T): Unit = arr(loc.x)(loc.y) = elt

  def inBounds(x: Int, y: Int): Boolean = x >= 0 && x < xSize && y >= 0 && y < ySize
  def inBounds(loc: Loc): Boolean = inBounds(loc.x,loc.y)

  def copy(): Plane[T] = new Plane[T](xSize,ySize,topology, arr.map { subarr => subarr.clone() })

  def map[U:ClassTag](f: T => U): Plane[U] = new Plane[U](xSize,ySize,topology,arr.map { subarr => subarr.map(f) })
  def foreach(f: T => Unit): Unit = arr.foreach { subarr => subarr.foreach(f) }
  def foldLeft[U](z: U)(f: (U,T) => U) = arr.foldLeft(z) { (z,subarr) => subarr.foldLeft(z)(f) }
  def find(f: T => Boolean): Option[T] = arr.findMap(_.find(f))
  def findMap[U](f: T => Option[U]): Option[U] = arr.findMap(_.findMap(f))
  def transform(f: T => T): Unit = { arr.foreach { subarr => subarr.transform(f); () } }

  def mapInPlace(f: T => T): Unit = {
    for (x <- 0 to arr.length-1) {
      for(y <- 0 to arr(x).length-1) {
        arr(x)(y) = f(arr(x)(y))
      }
    }
  }

  def foreachi(f: (Loc, T) => Unit): Unit = {
    for (x <- 0 to arr.length-1) {
      for(y <- 0 to arr(x).length-1) {
        f(Loc(x,y), arr(x)(y))
      }
    }
  }
}

/**
 * PlaneTopology:
 * Specifies the topology of Plane - i.e. the distance metric and adjacency relationships
 */
trait PlaneTopology {
  def adj(loc: Loc): Seq[Loc]
  def forEachAdj(loc: Loc)(f: Loc => Unit) : Unit
  def distance(loc0: Loc, loc1: Loc): Int

  def forEachReachable(loc: Loc, steps: Int)(f: Loc => Boolean) : Unit = {
    var reached = Set[Loc]()
    var thisQueue = scala.collection.mutable.Queue[Loc]()
    thisQueue += loc

    for(i <- 1 to steps) {
      var nextQueue = scala.collection.mutable.Queue[Loc]()
      thisQueue.foreach { loc =>
        if(!reached.contains(loc)) {
          reached += loc
          val doContinue = f(loc)
          if(doContinue && i < steps) {
            forEachAdj(loc) { adj => nextQueue.enqueue(adj) }
          }
        }
      }
      thisQueue = nextQueue
    }
  }
}

/**
 * RegularTopology:
 * Topologies in which every location is congruent to every other location, and with the same orientation
 * with respect to X and Y.
 */
sealed trait RegularTopology extends PlaneTopology {
  val adjOffsets: List[Vec]
  def distance(loc0: Loc, loc1: Loc): Int

  def adj(loc: Loc): Seq[Loc] = {
    adjOffsets.map { vec => loc + vec }
  }
  def forEachAdj(loc: Loc)(f: Loc => Unit) = {
    adjOffsets.foreach { vec => f(loc+vec) }
  }
}

case object SquareTopology extends RegularTopology {
  val adjOffsets: List[Vec] = List(Vec(-1,-1),Vec(0,-1),Vec(1,-1),Vec(-1,0),Vec(1,0),Vec(-1,1),Vec(0,1),Vec(1,1))
  def distance(loc0: Loc, loc1: Loc): Int = {
    Math.max(Math.abs(loc1.x-loc0.x),Math.abs(loc1.y-loc0.y))
  }
}
case object ManhattanTopology extends RegularTopology {
  val adjOffsets: List[Vec] = List(Vec(0,-1),Vec(-1,0),Vec(1,0),Vec(0,1))
  def distance(loc0: Loc, loc1: Loc): Int = {
    Math.abs(loc1.x-loc0.x) + Math.abs(loc1.y-loc0.y)
  }
}
case object HexTopology extends RegularTopology {
  val adjOffsets: List[Vec] = List(Vec(0,-1),Vec(1,-1),Vec(-1,0),Vec(1,0),Vec(-1,1),Vec(0,1))
  def distance(loc0: Loc, loc1: Loc): Int = {
    Math.max(Math.max(Math.abs(loc1.x-loc0.x),Math.abs(loc1.y-loc0.y)),Math.abs((loc1.y-loc0.y) + (loc1.x-loc0.x)))
  }
}

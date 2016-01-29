import scala.reflect.ClassTag

sealed trait Side {
  def opp: Side = this match { case S1 => S2  case S2 => S1 }
}
case object S2 extends Side
case object S1 extends Side

sealed trait Attack
case class Normal(damage: Int) extends Attack
case object Unsummon extends Attack
case object Kill extends Attack

case class Piece(
  val attack: Attack,
  val defense: Int,
  val moveRange: Int,
  val attackRange: Int,

  val cost: Int,
  val rebate: Int,

  val deathSpawn: Option[Piece],
  val freeSpawn: Option[Piece],

  val isFlying: Boolean,
  val isLumbering: Boolean,
  val isAnchored: Boolean,
  val isEldritch: Boolean,
  val isShrieking: Boolean,
  val isCaptain: Boolean,
  val hasFlurry: Boolean,
  val hasBlink: Boolean,
  val canHurtCaptain: Boolean,

  val swarmMax: Int,
  val spawnRange: Int,

  val discardToKillZombies: Boolean,
  val discardForRange: Boolean,
  val discardForUnlumber: Boolean,
  val discardForMoveRange: Boolean
)

sealed trait Tile
case object Wall extends Tile
case object Ground extends Tile
case object Water extends Tile
case object ManaSpire extends Tile
case class Spawner(side:Side, piece:Piece) extends Tile

case class Loc(x:Int, y:Int) {
  def +(v: Vec) = Loc(x+v.dx, y+v.dy)
  def -(l: Loc) = Vec(x-l.x, y-l.y)
}

case class Vec(dx: Int, dy:Int) {
  def +(v: Vec) = Vec(dx+v.dx, dy+v.dy)
  def -(v: Vec) = Vec(dx-v.dx, dy-v.dy)
  def +(l: Loc) = Loc(dx+l.x, dy+l.y)
}

class Plane[T:ClassTag](
  val width: Int,
  val height: Int,
  initial: T
) {
  private val arr: Array[Array[T]] = Array.fill[T](width,height)(initial)
  def apply(x:Int, y:Int): T = arr(x)(y)
  def apply(loc: Loc): T = arr(loc.x)(loc.y)
  def update(x:Int, y:Int, elt: T): Unit = arr(x)(y) = elt
  def update(loc: Loc, elt: T): Unit = arr(loc.x)(loc.y) = elt
}

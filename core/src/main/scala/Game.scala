package minionsgame.core

/** TechLevel:
  * How far long the tech tree a side is for a given unit
  */

sealed trait TechLevel {
  val int : Int
  override def toString() : String = this match {
    case T0 => ""
    case T1 => "*"
    case T2 => "**"
  }
}
case object T0 extends TechLevel { val int = 0 }
case object T1 extends TechLevel { val int = 1 }
case object T2 extends TechLevel { val int = 2 }


/** Tech:
  * Element in the tech sequence.
  */
sealed trait Tech
case class PieceTech(pieceName:PieceName) extends Tech

/** TechState:
  * State of a single tech.
  */
case class TechState(
  val tech: Tech,
  val level: SideArray[TechLevel]
)

/** Game:
  * The "global" state which isn't local to a board.
  */
case class Game (
  val mana: SideArray[Int],
  val techLine: Array[TechState],
  // TODO(jpaulson): Spells for the moving side
) {



}

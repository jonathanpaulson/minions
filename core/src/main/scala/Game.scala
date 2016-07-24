// TODO(jpaulson): Add logic to interact with the rest of the game.

/** TechLevel:
  How far long the tech tree a side is for a given unit
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
/** Game:
  *  The "global" state which isn't local to a board.
  */
case class Game (
  val mana : SideArray[Int],
  val tech : SideArray[Array[TechLevel]]
  // TODO(jpaulson): Spells for the moving side
) {
}

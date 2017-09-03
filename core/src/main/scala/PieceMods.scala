package minionsgame.core

/**
 * PieceMod:
 * Modifications to piece stats that can happen due to spells, enchanted terrain, abilities.
 */
sealed trait PieceMod {
  val displayName: String
  val desc: String
  def apply(pieceStats: PieceStats): PieceStats

  override def toString: String = this match {
    case PieceMods.Shield => "Shield"
  }
}
object PieceMod {
  def ofString(s:String): PieceMod = {
    s match {
      case "Shield" => PieceMods.Shield
      case _ => throw new Exception("Could not parse PieceMod: " + s)
    }
  }
}

object PieceMods {

  case object Shield extends PieceMod {
    val displayName = "Shielded"
    val desc = "Doubles defense and gives persistence"
    def apply(pieceStats: PieceStats): PieceStats = {
      pieceStats.copy(
        isBaseStats = false,
        isPersistent = true,
        defense = pieceStats.defense * 2
      )
    }
  }
}

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
    case PieceMods.DoubleAttack => "DoubleAttack"
    case PieceMods.UnsummonAttack => "UnsummonAttack"
  }
}
object PieceMod {
  def ofString(s:String): PieceMod = {
    s match {
      case "Shield" => PieceMods.Shield
      case "DoubleAttack" => PieceMods.DoubleAttack
      case "UnsummonAttack" => PieceMods.UnsummonAttack
      case _ => throw new Exception("Could not parse PieceMod: " + s)
    }
  }
}

object PieceMods {

  case object Shield extends PieceMod {
    val displayName = "Shielded"
    val desc = "Doubled defense and persistent"
    def apply(pieceStats: PieceStats): PieceStats = {
      pieceStats.copy(
        isBaseStats = false,
        isPersistent = true,
        defense = pieceStats.defense * 2
      )
    }
  }

  case object DoubleAttack extends PieceMod {
    val displayName = "Doubled Strike"
    val desc = "Can attack twice this turn"
    def apply(pieceStats: PieceStats): PieceStats = {
      pieceStats.copy(
        isBaseStats = false,
        numAttacks = Math.max(pieceStats.numAttacks,2)
      )
    }
  }

  case object UnsummonAttack extends PieceMod {
    val displayName = "Unsummon Strike"
    val desc = "Attack unsummons this turn"
    def apply(pieceStats: PieceStats): PieceStats = {
      pieceStats.copy(
        isBaseStats = false,
        attackEffect = Some(Unsummon)
      )
    }
  }

}

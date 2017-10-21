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
    case PieceMods.MoveThree => "MoveThree"
    case PieceMods.AirStrike => "AirStrike"
    case PieceMods.Spawner => "Spawner"
    case PieceMods.Slow => "Slow"
    case PieceMods.Shackled => "Shackled"
    case PieceMods.Frozen => "Frozen"
  }
}
object PieceMod {
  def ofString(s:String): PieceMod = {
    s match {
      case "Shield" => PieceMods.Shield
      case "DoubleAttack" => PieceMods.DoubleAttack
      case "UnsummonAttack" => PieceMods.UnsummonAttack
      case "MoveThree" => PieceMods.MoveThree
      case "AirStrike" => PieceMods.AirStrike
      case "Spawner" => PieceMods.Spawner
      case "Slow" => PieceMods.Slow
      case "Shackled" => PieceMods.Shackled
      case "Frozen" => PieceMods.Frozen
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
        defense = pieceStats.defense match {
          case None => None
          case Some(d) => Some(d*2)
        }
      )
    }
  }

  case object DoubleAttack extends PieceMod {
    val displayName = "Doubled Strike"
    val desc = "Can attack twice"
    def apply(pieceStats: PieceStats): PieceStats = {
      pieceStats.copy(
        isBaseStats = false,
        numAttacks = Math.max(pieceStats.numAttacks,2)
      )
    }
  }

  case object UnsummonAttack extends PieceMod {
    val displayName = "Unsummon Strike"
    val desc = "Attack unsummons"
    def apply(pieceStats: PieceStats): PieceStats = {
      pieceStats.copy(
        isBaseStats = false,
        attackEffect = Some(Unsummon)
      )
    }
  }

  case object MoveThree extends PieceMod {
    val displayName = "Move Three"
    val desc = "Can move three"
    def apply(pieceStats : PieceStats): PieceStats = {
      pieceStats.copy(
        isBaseStats = false,
        moveRange = Math.max(pieceStats.moveRange, 3)
      )
    }
  }

  case object AirStrike extends PieceMod {
    val displayName = "Air Strike"
    val desc = "Range 3 attack vs flying units"
    def apply(pieceStats : PieceStats): PieceStats = {
      pieceStats.copy(
        isBaseStats = false,
        attackRangeVsFlying = Math.max(pieceStats.attackRangeVsFlying, 3)
      )
    }
  }

  case object Spawner extends PieceMod {
    val displayName = "Spawner"
    val desc = "Can act as spawner"
    def apply(pieceStats : PieceStats): PieceStats = {
      pieceStats.copy(
        isBaseStats = false,
        spawnRange = Math.max(pieceStats.spawnRange,1)
      )
    }
  }

  case object Slow extends PieceMod {
    val displayName = "Slow"
    val desc = "Becomes lumbering"
    def apply(pieceStats : PieceStats): PieceStats = {
      pieceStats.copy(
        isBaseStats = false,
        isLumbering = true
      )
    }
  }

  case object Shackled extends PieceMod {
    val displayName = "Slow"
    val desc = "Move range 1"
    def apply(pieceStats : PieceStats): PieceStats = {
      pieceStats.copy(
        isBaseStats = false,
        moveRange = Math.min(pieceStats.moveRange, 1)
      )
    }
  }

  case object Frozen extends PieceMod {
    val displayName = "Frozen"
    val desc = "Cannot attack"
    def apply(pieceStats : PieceStats): PieceStats = {
      pieceStats.copy(
        isBaseStats = false,
        attackEffect = None
      )
    }
  }

}

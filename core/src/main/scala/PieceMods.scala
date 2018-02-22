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
    case PieceMods.Shielded => "Shielded"
    case PieceMods.Protected => "Protected"
    case PieceMods.DoubleAttack => "DoubleAttack"
    case PieceMods.UnsummonAttack => "UnsummonAttack"
    case PieceMods.MoveThree => "MoveThree"
    case PieceMods.RangeTwo => "RangeTwo"
    case PieceMods.AirStrike => "AirStrike"
    case PieceMods.Spawner => "Spawner"
    case PieceMods.Lumbering => "Lumbering"
    case PieceMods.Shackled => "Shackled"
    case PieceMods.Frozen => "Frozen"
    case PieceMods.Weakened => "Weakened"
  }

  def isGood: Boolean = this match {
    case PieceMods.Shielded => true
    case PieceMods.Protected => true
    case PieceMods.DoubleAttack => true
    case PieceMods.UnsummonAttack => true
    case PieceMods.RangeTwo => true
    case PieceMods.MoveThree => true
    case PieceMods.AirStrike => true
    case PieceMods.Spawner => true
    case PieceMods.Lumbering => false
    case PieceMods.Shackled => false
    case PieceMods.Frozen => false
    case PieceMods.Weakened => false
  }

}
object PieceMod {
  def ofString(s:String): PieceMod = {
    s match {
      case "Shielded" => PieceMods.Shielded
      case "Protected" => PieceMods.Protected
      case "DoubleAttack" => PieceMods.DoubleAttack
      case "UnsummonAttack" => PieceMods.UnsummonAttack
      case "RangeTwo" => PieceMods.RangeTwo
      case "AirStrike" => PieceMods.AirStrike
      case "Spawner" => PieceMods.Spawner
      case "Lumbering" => PieceMods.Lumbering
      case "Shackled" => PieceMods.Shackled
      case "Frozen" => PieceMods.Frozen
      case "Weakened" => PieceMods.Weakened
      case _ => throw new Exception("Could not parse PieceMod: " + s)
    }
  }
}

object PieceMods {
  case object Shielded extends PieceMod {
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

  case object Protected extends PieceMod {
    val displayName = "Protected"
    val desc = "+2 defense"
    def apply(pieceStats: PieceStats): PieceStats = {
      pieceStats.copy(
        isBaseStats = false,
        defense = pieceStats.defense match {
          case None => None
          case Some(d) => Some(d+2)
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

  case object RangeTwo extends PieceMod {
    val displayName = "Range Two"
    val desc = "Can attack at range two"
    def apply(pieceStats : PieceStats): PieceStats = {
      pieceStats.copy(
        isBaseStats = false,
        attackRange = Math.max(pieceStats.attackRange, 2)
        attackRangeVsFlying = Math.max(pieceStats.attackRangeVsFlying, 2)
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
        spawnRange = Some(Math.max(1,pieceStats.spawnRange.getOrElse(1)))
      )
    }
  }

  case object Lumbering extends PieceMod {
    val displayName = "Lumbering"
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
    val desc = "Move range 1 and attack range 1"
    def apply(pieceStats : PieceStats): PieceStats = {
      pieceStats.copy(
        isBaseStats = false,
        moveRange = Math.min(pieceStats.moveRange, 1),
        attackRange = Math.min(pieceStats.attackRange, 1),
        attackRangeVsFlying = Math.min(pieceStats.attackRangeVsFlying, 1),
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

  case object Weakened extends PieceMod {
    val displayName = "Weakened"
    val desc = "-1 attack, cannot attack if < 1"
    def apply(pieceStats : PieceStats): PieceStats = {
      val reduce_flurry = pieceStats.numAttacks > 1 && pieceStats.attackEffect == Some(Damage(1))
      pieceStats.copy(
        isBaseStats = false,
        numAttacks = if(reduce_flurry) pieceStats.numAttacks-1 else pieceStats.numAttacks,
        attackEffect = pieceStats.attackEffect match {
          case None => None
          case Some(Damage(n)) =>
            if(reduce_flurry) Some(Damage(n))
            else if(n <= 1) None
            else Some(Damage(n-1))
          case Some(Unsummon) | Some(Kill) | Some(Enchant(_)) | Some(TransformInto(_)) =>
            pieceStats.attackEffect
        }
      )
    }
  }

}

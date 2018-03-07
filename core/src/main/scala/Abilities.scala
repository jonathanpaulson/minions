package minionsgame.core
import scala.util.{Try,Success,Failure}

//For wailing units
case object Suicide extends PieceAbility {
  val name = "suicide"
  val displayName = "Suicide"
  val desc = List("Kills this piece (without having spent all attacks).")
  val isSorcery = false
  val spawnPhaseOnly = false
  val tryIsUsableNow = { (_:Piece) => Success(()) }
}

case object KillAdjacent extends PieceAbility {
  val name = "scream"
  val displayName = "Scream"
  val desc = List("Kills all adjacent enemy minions")
  val isSorcery = true
  val spawnPhaseOnly = false
  val tryIsUsableNow = { (_:Piece) => Success(()) }
}

case object SpawnZombies extends PieceAbility {
  val name = "spawn_zombies"
  val displayName = "Spawn Zombies"
  val desc = List("Spawn a zombie in every adjacent hex")
  val isSorcery = true
  val spawnPhaseOnly = true
  val tryIsUsableNow = { (_:Piece) => Success(()) }
}

case object MoveEarthquake extends PieceAbility {
  val name = "move_earthquake"
  val displayName = "Move Earthquake"
  val desc = List("Move Earthquake (only passable by unit types with >= 2 speed)", "to adjacent empty Ground hex")
  val isSorcery = true
  val spawnPhaseOnly = true
  val tryIsUsableNow = { (_:Piece) => Success(()) }
}
case object MoveFlood extends PieceAbility {
  val name = "move_flood"
  val displayName = "Move Flood"
  val desc = List("Move Flood (only passable by flying unit types)", "to adjacent empty Ground hex")
  val isSorcery = true
  val spawnPhaseOnly = true
  val tryIsUsableNow = { (_:Piece) => Success(()) }
}
case object MoveWhirlwind extends PieceAbility {
  val name = "move_whirlwind"
  val displayName = "Move Whirlwind"
  val desc = List("Move Whirlwind (only passable by persistent unit types)", "to adjacent empty Ground hex")
  val isSorcery = true
  val spawnPhaseOnly = true
  val tryIsUsableNow = { (_:Piece) => Success(()) }
}
case object MoveFirestorm extends PieceAbility {
  val name = "move_firestorm"
  val displayName = "Move Firestorm"
  val desc = List("Move Firestorm (only passable by unit types with >= 4 health)", "to adjacent empty Ground hex")
  val isSorcery = true
  val spawnPhaseOnly = true
  val tryIsUsableNow = { (_:Piece) => Success(()) }
}
case object MoveTerrain extends PieceAbility {
  val name = "move_terrain"
  val displayName = "Move Terrain"
  val desc = List("Move one of the four terrain tiles", "to target adjacent empty Ground hex")
  val isSorcery = true
  val spawnPhaseOnly = true
  val tryIsUsableNow = { (_:Piece) => Success(()) }
}

case object Abilities {
  val double_attack = SelfEnchantAbility(
    name = "double_attack",
    displayName = "Double Attack",
    desc = List("Can attack twice this turn"),
    isSorcery = true,
    spawnPhaseOnly = false,
    tryIsUsableNow = { (piece: Piece) =>
      if(piece.actState >= Spawning) Failure(new Exception("Piece has already acted or cannot act this turn"))
      else Success(())
    },
    mod = PieceModWithDuration(PieceMods.DoubleAttack,turnsLeft = Some(1))
  )
  val range_two = SelfEnchantAbility(
    name = "rangetwo",
    displayName = "Range Two (sorcery)",
    desc = List("Has range two until end of turn"),
    isSorcery = true,
    spawnPhaseOnly = false,
    tryIsUsableNow = { (piece: Piece) =>
      piece.actState match {
        case DoneActing | Spawning => Failure(new Exception("Piece has already finished its turn"))
        case Moving(_) | Attacking(_) => Success(())
      }
    },
    mod = PieceModWithDuration(PieceMods.RangeTwo, turnsLeft = Some(1))
  )
  val move_three = SelfEnchantAbility(
    name = "movethree",
    displayName = "Move Three (sorcery)",
    desc = List("Move three this turn"),
    isSorcery = true,
    spawnPhaseOnly = false,
    tryIsUsableNow = { (piece: Piece) =>
      piece.actState match {
        case DoneActing | Spawning | Attacking(_) => Failure(new Exception("Piece has already acted or attacked this turn"))
        case Moving(_) => Success(())
      }
    },
    mod = PieceModWithDuration(PieceMods.MoveThree, turnsLeft = Some(1))
  )
  val airstrike = SelfEnchantAbility(
    name = "airstrike",
    displayName = "Air Strike",
    desc = List("Attack range 3 vs. flying this turn"),
    isSorcery = true,
    spawnPhaseOnly = false,
    tryIsUsableNow = { (piece: Piece) =>
      if(piece.actState >= Spawning) Failure(new Exception("Piece has already acted or cannot act this turn"))
      else Success(())
    },
    mod = PieceModWithDuration(PieceMods.AirStrike, turnsLeft = Some(1))
  )

  val abilities = Array(
    Suicide,KillAdjacent,SpawnZombies,
    MoveEarthquake,MoveFlood,MoveWhirlwind,MoveFirestorm,MoveTerrain,
    double_attack,range_two,move_three,airstrike
  )
  val abilityMap: Map[String,PieceAbility] = abilities.map { ability => (ability.name -> ability) }.toMap
}

package minionsgame.core
import scala.collection.immutable.Map

// TODO(jpaulson): Add atual unit data

object Units {
  val zombie = PieceStats(
    name = "zombie",
    displayName = "Zombie",
    isBaseStats = true,
    attackEffect = Some(Damage(1)),
    defense = 2,
    moveRange = 5,
    attackRange = 2,
    numAttacks = 1,
    cost = 2,
    rebate = 0,
    isNecromancer = false,
    isFlying = true,
    isLumbering = false,
    isPersistent = false,
    isEldritch = false,
    isWailing = false,
    hasBlink = false,
    canHurtNecromancer = true,
    swarmMax = 3, //Swarming 3 for testing
    spawnRange = 1,
    extraMana = 0,
    deathSpawn = None,
    abilities = Map.empty
  )

  val bat = PieceStats(
    name = "bat",
    displayName = "Bat",
    isBaseStats = true,
    attackEffect = Some(Damage(1)),
    defense = 1,
    moveRange = 3,
    attackRange = 1,
    numAttacks = 1,
    cost = 4,
    rebate = 2,
    isNecromancer = false,
    isFlying = true,
    isLumbering = false,
    isPersistent = false,
    isEldritch = false,
    isWailing = false,
    hasBlink = false,
    canHurtNecromancer = true,
    swarmMax = 1,
    spawnRange = 0,
    extraMana = 0,
    deathSpawn = None,
    abilities = Map.empty
  )

  val pieces = Array(zombie,bat)
  val pieceMap: Map[PieceName,PieceStats] = pieces.groupBy(piece => piece.name).mapValues { pieces =>
    assert(pieces.length == 1)
    pieces.head
  }


  val techs = List(bat, bat, bat, bat, bat, bat, bat, bat, bat)
}

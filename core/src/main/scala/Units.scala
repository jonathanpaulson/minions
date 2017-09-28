package minionsgame.core
import scala.collection.immutable.Map

// TODO(jpaulson): Add atual unit data

object Units {
  private def createPieceStats(
    name : String,
    displayName: String = "",
    attackEffect : Option[TargetEffect],
    defense : Int,
    moveRange : Int,
    attackRange : Int,
    numAttacks : Int = 1,
    cost : Int,
    rebate : Int,
    isNecromancer : Boolean = false,
    isFlying : Boolean = false,
    isLumbering : Boolean = false,
    isPersistent : Boolean = false,
    isEldritch : Boolean = false,
    isWailing : Boolean = false,
    hasBlink : Boolean = false,
    canHurtNecromancer : Boolean = true,
    swarmMax : Int = 1,
    spawnRange : Int = 0,
    extraMana : Int = 0,
    deathSpawn : Option[PieceName] = None,
    abilities : Map[String,PieceAbility] = Map.empty
  ) : PieceStats = {
    PieceStats(
      name = name,
      displayName = (if(displayName == "") name.capitalize else displayName),
      isBaseStats = true,
      attackEffect = attackEffect,
      defense = defense,
      moveRange = moveRange,
      attackRange = attackRange,
      numAttacks = numAttacks,
      cost = cost,
      rebate = rebate,
      isNecromancer = isNecromancer,
      isFlying = isFlying,
      isLumbering = isLumbering,
      isPersistent = isPersistent,
      isEldritch = isEldritch,
      isWailing = isWailing,
      hasBlink = hasBlink,
      canHurtNecromancer = canHurtNecromancer,
      swarmMax = swarmMax,
      spawnRange = spawnRange,
      extraMana = extraMana,
      deathSpawn = deathSpawn,
      abilities = abilities
    )
  }

  val test = createPieceStats(
    name = "test",
    attackEffect = Some(Damage(1)),
    defense = 2,
    moveRange = 5,
    attackRange = 2,
    cost = 2,
    rebate = 1,
    isFlying = true,
    swarmMax = 3
  )

  val necromancer = createPieceStats(
    name = "necromancer",
    attackEffect = Some(Unsummon),
    defense = 7,
    moveRange = 1,
    attackRange = 1,
    cost = 0,
    rebate = 0,
    isNecromancer = true,
    spawnRange = 1
  )

  val zombie = createPieceStats(
    name = "zombie",
    attackEffect = Some(Damage(1)),
    defense = 2,
    moveRange = 1,
    attackRange = 1,
    cost = 2,
    rebate = 0
  )

  val acolyte = createPieceStats(
    name = "acolyte",
    attackEffect = None,
    defense = 2,
    moveRange = 2,
    attackRange = 0,
    cost = 5,
    rebate = 3,
    spawnRange = 1
  )

  val initiate = createPieceStats(
    name = "initiate",
    attackEffect = Some(Damage(5)),
    defense = 5,
    moveRange = 2,
    attackRange = 1,
    cost = 5,
    rebate = 3,
    spawnRange = 1,
    isLumbering = true
  )

  val skeleton = createPieceStats(
    name = "skeleton",
    attackEffect = Some(Damage(5)),
    defense = 2,
    moveRange = 1,
    attackRange = 1,
    cost = 4,
    rebate = 2
  )

  val serpent = createPieceStats(
    name = "serpent",
    attackEffect = Some(Damage(3)),
    defense = 1,
    moveRange = 2,
    attackRange = 1,
    cost = 5,
    rebate = 3
  )

  val bat = createPieceStats(
    name = "bat",
    attackEffect = Some(Damage(1)),
    defense = 1,
    moveRange = 3,
    attackRange = 1,
    cost = 4,
    rebate = 2,
    isFlying = true
  )

  val ghost = createPieceStats(
    name = "ghost",
    attackEffect = Some(Damage(1)),
    defense = 4,
    moveRange = 1,
    attackRange = 1,
    cost = 3,
    rebate = 0,
    isFlying = true
  )

  val wight = createPieceStats(
    name = "wight",
    attackEffect = Some(Damage(3)),
    defense = 3,
    moveRange = 1,
    attackRange = 1,
    cost = 5,
    rebate = 0,
    deathSpawn = Some(zombie.name),
    abilities = Map.empty // TODO(jpaulson): Attack twice as a sorcery
  )

  val haunt = createPieceStats(
    name = "haunt",
    attackEffect = Some(Damage(2)),
    defense = 2,
    moveRange = 0,
    attackRange = 3,
    cost = 5,
    rebate = 1,
    hasBlink = true
  )

  val shrieker = createPieceStats(
    name = "shrieker",
    attackEffect = Some(Damage(1)),
    defense = 2,
    moveRange = 3,
    attackRange = 1,
    numAttacks = 2,
    cost = 2,
    rebate = 0,
    isWailing = true,
    canHurtNecromancer = false
  )

  val warg = createPieceStats(
    name = "warg",
    attackEffect = Some(Damage(2)),
    defense = 3,
    moveRange = 3,
    attackRange = 1,
    cost = 7,
    rebate = 5
  )

  //All pieces
  val pieces = Array(test, necromancer, zombie, initiate, skeleton, serpent, bat, ghost, wight, haunt, shrieker, warg)

  //Generally, we store and send the PieceName everywhere in the protocol, since unlike a PieceStats it's easily serialized.
  //This is the global map that everything uses to look up the stats again from the name.
  val pieceMap: Map[PieceName,PieceStats] = pieces.groupBy(piece => piece.name).mapValues { pieces =>
    assert(pieces.length == 1)
    pieces.head
  }

}

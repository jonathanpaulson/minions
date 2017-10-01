package minionsgame.core
import scala.collection.immutable.Map
import scala.util.{Try,Success,Failure}

// TODO(jpaulson): Add atual unit data

object Units {
  private def createPieceStats(
    name : String,
    shortDisplayName: String = "",
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
    canHurtNecromancer : Boolean = true,
    swarmMax : Int = 1,
    spawnRange : Int = 0,
    extraMana : Int = 0,
    extraSorceryPower : Int = 0,
    deathSpawn : Option[PieceName] = None,
    abilities : Map[String,PieceAbility] = Map.empty
  ) : PieceStats = {
    PieceStats(
      name = name,
      shortDisplayName = (if(shortDisplayName == "") name.capitalize else shortDisplayName),
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
      canHurtNecromancer = canHurtNecromancer,
      swarmMax = swarmMax,
      spawnRange = spawnRange,
      extraMana = extraMana,
      extraSorceryPower = extraSorceryPower,
      deathSpawn = deathSpawn,
      abilities = abilities
    )
  }

  val test = createPieceStats(
    name = "test",
    cost = 2,
    rebate = 1,
    attackEffect = Some(Damage(1)),
    defense = 2,
    moveRange = 5,
    attackRange = 2,
    isFlying = true,
    swarmMax = 3
  )

  val necromancer = createPieceStats(
    name = "necromancer",
    shortDisplayName = "Necro",
    cost = 0,
    rebate = 0,
    attackEffect = Some(Unsummon),
    defense = 7,
    moveRange = 1,
    attackRange = 1,
    isPersistent = true,
    isNecromancer = true,
    spawnRange = 1,
    extraMana = 3,
    //TODO while there are no spells, we'll just have necromancers power unit ability sorceries
    extraSorceryPower = 2
  )

  val zombie = createPieceStats(
    name = "zombie",
    cost = 2,
    rebate = 0,
    attackEffect = Some(Damage(1)),
    defense = 2,
    moveRange = 1,
    attackRange = 1,
    isLumbering = true
  )

  val acolyte = createPieceStats(
    name = "acolyte",
    cost = 5,
    rebate = 3,
    attackEffect = None,
    defense = 2,
    moveRange = 2,
    attackRange = 0,
    spawnRange = 1
  )

  val spire = createPieceStats(
    name = "spire",
    cost = 7,
    rebate = 4,
    attackEffect = Some(Damage(4)),
    defense = 4,
    moveRange = 0,
    attackRange = 1,
    isPersistent = true
  )

  val initiate = createPieceStats(
    name = "initiate",
    cost = 5,
    rebate = 3,
    attackEffect = Some(Damage(5)),
    defense = 5,
    moveRange = 2,
    attackRange = 1,
    spawnRange = 1,
    isLumbering = true
  )

  val skeleton = createPieceStats(
    name = "skeleton",
    cost = 4,
    rebate = 2,
    attackEffect = Some(Damage(5)),
    defense = 2,
    moveRange = 1,
    attackRange = 1,
  )

  val serpent = createPieceStats(
    name = "serpent",
    cost = 5,
    rebate = 3,
    attackEffect = Some(Damage(3)),
    defense = 1,
    moveRange = 2,
    attackRange = 1,
  )

  val bat = createPieceStats(
    name = "bat",
    cost = 4,
    rebate = 2,
    attackEffect = Some(Damage(1)),
    defense = 1,
    moveRange = 3,
    attackRange = 1,
    isFlying = true
  )

  val ghost = createPieceStats(
    name = "ghost",
    cost = 3,
    rebate = 0,
    attackEffect = Some(Damage(1)),
    defense = 4,
    moveRange = 1,
    attackRange = 1,
    isFlying = true,
    isPersistent = true
  )

  val wight = createPieceStats(
    name = "wight",
    cost = 5,
    rebate = 0,
    deathSpawn = Some(zombie.name),
    attackEffect = Some(Damage(3)),
    defense = 3,
    moveRange = 1,
    attackRange = 1,
    abilities = Map("doubleattack" -> SelfEnchantAbility(
      name = "doubleattack",
      displayName = "Double Strike (sorcery)",
      desc = "Pay 1 sorcery power: Attacks twice",
      isSorcery = true,
      tryIsUsableNow = { (_: Piece) => Success(()) },
      mod = PieceModWithDuration(PieceMods.DoubleAttack,turnsLeft = Some(1))
    ))
  )

  val haunt = createPieceStats(
    name = "haunt",
    cost = 5,
    rebate = 1,
    attackEffect = Some(Damage(2)),
    defense = 2,
    moveRange = 0,
    attackRange = 3,
    abilities = Map(BlinkAbility.name -> BlinkAbility)
  )

  val shrieker = createPieceStats(
    name = "shrieker",
    cost = 2,
    rebate = 0,
    attackEffect = Some(Damage(1)),
    defense = 2,
    moveRange = 3,
    attackRange = 1,
    numAttacks = 2,
    isWailing = true,
    canHurtNecromancer = false,
    abilities = Map(SuicideAbility.name -> SuicideAbility)
  )

  val warg = createPieceStats(
    name = "warg",
    cost = 7,
    rebate = 5,
    attackEffect = Some(Damage(2)),
    defense = 3,
    moveRange = 3,
    attackRange = 1,
  )

  val dark_tower = createPieceStats(
    name = "dark_tower",
    displayName = "Dark Tower",
    cost = 8,
    rebate = 0,
    deathSpawn = Some(spire.name),
    attackEffect = Some(Damage(4)),
    defense = 4,
    moveRange = 0,
    attackRange = 2,
    isPersistent = true,
    spawnRange = 1
  )

  val witch = createPieceStats(
    name = "witch",
    cost = 4,
    rebate = 0,
    attackEffect = Some(Damage(1)),
    defense = 2,
    moveRange = 1,
    attackRange = 3,
    swarmMax = 3,
  )

  val lich = createPieceStats(
    name = "lich",
    cost = 8,
    deathSpawn = Some(skeleton.name),
    rebate = 0,
    attackEffect = Some(Kill),
    defense = 3,
    moveRange = 1,
    attackRange = 3,
    isLumbering = true,
  )

  val mummy = createPieceStats(
    name = "mummy",
    cost = 8,
    rebate = 0,
    attackEffect = Some(Damage(5)),
    defense = 8,
    moveRange = 1,
    attackRange = 1,
    deathSpawn = Some(initiate.name),
  )

  val bone_rat = createPieceStats(
    name = "bone_rat",
    shortDisplayName = "BnRat",
    displayName = "Bone Rat",
    cost = 2,
    rebate = 1,
    attackEffect = Some(Damage(1)),
    defense = 1,
    moveRange = 1,
    attackRange = 1,
    swarmMax = 3,
    abilities = Map("unsummonattack" -> SelfEnchantAbility(
      name = "unsummonattack",
      displayName = "Unsummon Strike (sorcery)",
      desc = "Pay 1 sorcery power: Attack unsummons",
      isSorcery = true,
      tryIsUsableNow = { (_: Piece) => Success(()) },
      mod = PieceModWithDuration(PieceMods.DoubleAttack,turnsLeft = Some(1))
    ))
  )

  //All pieces
  val pieces = Array(
    test,
    necromancer,
    zombie, acolyte, spire,
    initiate, skeleton, serpent, bat, ghost, wight, haunt, shrieker, warg, dark_tower, witch, lich, mummy, bone_rat
  )

  //Generally, we store and send the PieceName everywhere in the protocol, since unlike a PieceStats it's easily serialized.
  //This is the global map that everything uses to look up the stats again from the name.
  val pieceMap: Map[PieceName,PieceStats] = pieces.groupBy(piece => piece.name).mapValues { pieces =>
    assert(pieces.length == 1)
    pieces.head
  }

}

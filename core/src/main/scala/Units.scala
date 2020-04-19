package minionsgame.core
import scala.collection.immutable.Map
import scala.util.{Try,Success,Failure}

object Units {
  private def createPieceStats(
    name : String,
    shortDisplayName: String = "",
    displayName: String = "",
    attackEffect : Option[TargetEffect],
    defense : Option[Int],
    moveRange : Int,
    attackRange : Int,
    attackRangeVsFlying: Int = 0,
    numAttacks : Int = 1,
    cost : Int,
    rebate : Int,
    isNecromancer : Boolean = false,
    isFlying : Boolean = false,
    isLumbering : Boolean = false,
    isPersistent : Boolean = false,
    isEldritch : Boolean = false,
    isWailing : Boolean = false,
    canBlink : Boolean = false,
    canHurtNecromancer : Boolean = true,
    swarmMax : Int = 1,
    spawnRange : Option[Int] = None,
    extraSouls : Int = 0,
    extraMana : Int = 0,
    deathSpawn : Option[PieceName] = None,
    perTurnReinforcement : Option[PieceName] = None,
    abilities : List[PieceAbility] = List.empty,
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
      attackRangeVsFlying = Math.max(attackRange,attackRangeVsFlying),
      numAttacks = numAttacks,
      cost = cost,
      rebate = rebate,
      isNecromancer = isNecromancer,
      isFlying = isFlying,
      isLumbering = isLumbering,
      isPersistent = isPersistent,
      isEldritch = isEldritch,
      isWailing = isWailing,
      canBlink = canBlink,
      canHurtNecromancer = canHurtNecromancer,
      swarmMax = swarmMax,
      spawnRange = spawnRange,
      extraSouls = extraSouls,
      extraMana = extraMana,
      deathSpawn = deathSpawn,
      perTurnReinforcement = perTurnReinforcement,
      abilities = abilities.map { ability => (ability.name -> ability) }.toMap
    )
  }

  val necromancer = createPieceStats(
    name = "necromancer",
    shortDisplayName = "Necro",
    cost = 0,
    rebate = 0,
    moveRange = 1,
    attackRange = 1,
    attackEffect = Some(Unsummon),
    defense = Some(7),
    spawnRange = Some(1),
    isPersistent = true,
    isNecromancer = true,
    extraSouls = 3
  )
  val mana_necromancer = createPieceStats(
    name = "mana_necromancer",
    displayName = "ManaNecromancer",
    shortDisplayName = "ManaNe",
    cost = 0,
    rebate = 0,
    moveRange = 1,
    attackRange = 1,
    attackEffect = Some(Unsummon),
    defense = Some(7),
    spawnRange = Some(1),
    isPersistent = true,
    isNecromancer = true,
    extraSouls = 3,
    extraMana = 1
  )
  val battle_necromancer = createPieceStats(
    name = "battle_necromancer",
    displayName = "FlurryNecromancer",
    shortDisplayName = "FlurryNec",
    cost = 0,
    rebate = 0,
    moveRange = 1,
    attackRange = 1,
    attackEffect = Some(Damage(1)),
    numAttacks = 4,
    defense = Some(10),
    spawnRange = Some(1),
    isPersistent = true,
    isNecromancer = true,
    extraSouls = 3
  )
  val swarm_necromancer = createPieceStats(
    name = "swarm_necromancer",
    shortDisplayName = "SNec",
    displayName = "Swarm Necromancer",
    cost = 0,
    rebate = 0,
    moveRange = 1,
    attackRange = 1,
    attackEffect = Some(Unsummon),
    defense = Some(10),
    spawnRange = Some(1),
    isPersistent = true,
    swarmMax = 3,
    isNecromancer = true,
    extraSouls = 1,
  )
  val arcane_necromancer = createPieceStats(
    name = "arcane_necromancer",
    shortDisplayName = "ArcNec",
    displayName = "Arcane Necromancer",
    cost = 0,
    rebate = 0,
    moveRange = 1,
    attackRange = 1,
    attackEffect = Some(Unsummon),
    defense = Some(7),
    spawnRange = Some(1),
    isPersistent = true,
    isNecromancer = true,
    extraSouls = 4,
  )
  val mounted_necromancer = createPieceStats(
    name = "mounted_necromancer",
    shortDisplayName = "MntNec",
    displayName = "Mounted Necromancer",
    cost = 0,
    rebate = 0,
    moveRange = 2,
    attackRange = 1,
    attackEffect = Some(Unsummon),
    defense = Some(10),
    spawnRange = Some(1),
    isPersistent = true,
    isNecromancer = true,
    extraSouls = 3,
  )
  val ranged_necromancer = createPieceStats(
    name = "ranged_necromancer",
    shortDisplayName = "RngNec",
    displayName = "Ranged Necromancer",
    cost = 0,
    rebate = 0,
    moveRange = 1,
    attackRange = 3,
    attackEffect = Some(Unsummon),
    defense = Some(10),
    spawnRange = Some(1),
    isPersistent = true,
    isNecromancer = true,
    extraSouls = 3,
  )
  val deadly_necromancer = createPieceStats(
    name = "deadly_necromancer",
    shortDisplayName = "DedNec",
    displayName = "Deadly Necromancer",
    cost = 0,
    rebate = 0,
    moveRange = 1,
    attackRange = 1,
    attackEffect = Some(Kill),
    defense = Some(10),
    spawnRange = Some(1),
    isPersistent = true,
    isNecromancer = true,
    extraSouls = 3,
    canHurtNecromancer = false,
  )
  val immortal_necromancer = createPieceStats(
    name = "immortal_necromancer",
    shortDisplayName = "ImmNec",
    displayName = "Immortal Necromancer",
    cost = 0,
    rebate = 0,
    moveRange = 1,
    isFlying = true,
    attackRange = 1,
    attackEffect = Some(Unsummon),
    defense = None,
    spawnRange = Some(1),
    isPersistent = true,
    isNecromancer = true,
    extraSouls = 3,
  )
  val summoner_necromancer = createPieceStats(
    name = "summoner_necromancer",
    shortDisplayName = "SumNec",
    displayName = "Summoner Necromancer",
    cost = 0,
    rebate = 0,
    moveRange = 1,
    attackRange = 1,
    attackEffect = Some(Unsummon),
    defense = Some(7),
    spawnRange = Some(2),
    isPersistent = true,
    isNecromancer = true,
    extraSouls = 3,
  )

  val zombie = createPieceStats(
    name = "zombie",
    cost = 2,
    rebate = 0,
    moveRange = 1,
    isLumbering = true,
    attackRange = 1,
    attackEffect = Some(Damage(1)),
    defense = Some(2),
  )

  val zombie_necromancer = createPieceStats(
    name = "zombie_necromancer",
    shortDisplayName = "ZomNec",
    displayName = "Zombie Necromancer",
    cost = 0,
    rebate = 0,
    moveRange = 1,
    attackRange = 1,
    attackEffect = Some(Unsummon),
    defense = Some(10),
    spawnRange = Some(1),
    isPersistent = true,
    isNecromancer = true,
    extraSouls = 3,
    perTurnReinforcement = Some(zombie.name)
  )

  val acolyte = createPieceStats(
    name = "acolyte",
    cost = 5,
    rebate = 3,
    moveRange = 2,
    attackRange = 0,
    attackEffect = None,
    defense = Some(2),
    spawnRange = Some(1)
  )

  val spire = createPieceStats(
    name = "spire",
    cost = 4,
    rebate = 1,
    moveRange = 0,
    attackRange = 1,
    attackEffect = Some(Damage(5)),
    defense = Some(4),
    spawnRange = Some(1),
  )

  val initiate = createPieceStats(
    name = "initiate",
    cost = 2,
    rebate = 0,
    moveRange = 2,
    isLumbering = true,
    attackRange = 1,
    attackEffect = Some(Damage(1)),
    defense = Some(3),
    spawnRange = Some(1),
  )

  val skeleton = createPieceStats(
    name = "skeleton",
    cost = 4,
    rebate = 2,
    moveRange = 1,
    attackRange = 1,
    attackEffect = Some(Damage(5)),
    defense = Some(2),
  )

  val serpent = createPieceStats(
    name = "serpent",
    cost = 5,
    rebate = 3,
    moveRange = 2,
    attackRange = 1,
    attackEffect = Some(Damage(3)),
    defense = Some(1),
    abilities = List(MoveFlood),
  )

  val bat = createPieceStats(
    name = "bat",
    cost = 4,
    rebate = 2,
    moveRange = 3,
    isFlying = true,
    attackRange = 1,
    attackEffect = Some(Damage(1)),
    defense = Some(1),
  )

  val ghost = createPieceStats(
    name = "ghost",
    cost = 3,
    rebate = 1,
    moveRange = 1,
    isFlying = true,
    attackRange = 1,
    attackEffect = Some(Damage(1)),
    defense = Some(4),
    isPersistent = true
  )

  val wight = createPieceStats(
    name = "wight",
    cost = 5,
    rebate = 2,
    moveRange = 0,
    attackRange = 2,
    attackEffect = Some(Damage(1)),
    numAttacks = 5,
    defense = Some(3),
    canBlink = true,
  )

  val haunt = createPieceStats(
    name = "haunt",
    cost = 5,
    rebate = 1,
    moveRange = 0,
    attackRange = 3,
    attackEffect = Some(Damage(2)),
    defense = Some(2),
    canBlink = true,
    abilities = List(MoveEarthquake)
  )

  val shrieker = createPieceStats(
    name = "shrieker",
    cost = 6,
    rebate = 4,
    moveRange = 3,
    attackRange = 1,
    attackEffect = Some(Damage(3)),
    defense = Some(1),
  )

  val fog = createPieceStats(
    name = "fog",
    cost = 5,
    rebate = 2,
    moveRange = 2,
    attackRange = 1,
    attackEffect = Some(Damage(1)),
    defense = Some(7),
    spawnRange = Some(1),
    isFlying = true,
  )

  val dark_tower = createPieceStats(
    name = "dark_tower",
    shortDisplayName = "DTower",
    displayName = "Dark Tower",
    cost = 6,
    rebate = 0,
    moveRange = 0,
    attackRange = 2,
    attackEffect = Some(Damage(5)),
    defense = Some(4),
    spawnRange = Some(1),
    deathSpawn = Some(spire.name),
    abilities = List(SpawnZombies),
  )

  val sorcerer = createPieceStats(
    name = "sorcerer",
    cost = 4,
    rebate = 0,
    moveRange = 1,
    attackRange = 2,
    attackEffect = Some(Unsummon),
    defense = Some(3),
    swarmMax = 3,
  )

  val witch = createPieceStats(
    name = "witch",
    cost = 3,
    rebate = 0,
    moveRange = 1,
    attackRange = 3,
    attackEffect = Some(Damage(1)),
    defense = Some(1),
    swarmMax = 3,
  )

  val vampire = createPieceStats(
    name = "vampire",
    cost = 6,
    rebate = 3,
    moveRange = 1,
    isFlying = true,
    attackRange = 1,
    attackEffect = Some(Damage(1)),
    numAttacks = 2,
    defense = Some(5),
    isPersistent = true,
    abilities = List(Abilities.move_three),
  )

  val mummy = createPieceStats(
    name = "mummy",
    cost = 5,
    rebate = 1,
    moveRange = 1,
    attackRange = 1,
    attackEffect = Some(Damage(5)),
    defense = Some(8),
    spawnRange = Some(1),
  )

  val lich = createPieceStats(
    name = "lich",
    cost = 8,
    rebate = 0,
    moveRange = 1,
    isLumbering = true,
    attackRange = 3,
    attackEffect = Some(Kill),
    defense = Some(3),
    deathSpawn = Some(skeleton.name),
    canHurtNecromancer = false,
  )

  val void = createPieceStats(
    name = "void",
    cost = 5,
    rebate = 3,
    moveRange = 2,
    attackRange = 2,
    attackEffect = Some(Unsummon),
    defense = Some(2),
    canBlink = true,
    abilities = List(MoveWhirlwind)
  )

  val hell_hound = createPieceStats(
    name = "hell_hound",
    shortDisplayName = "Hound",
    displayName = "Hell Hound",
    cost = 4,
    rebate = 2,
    moveRange = 4,
    attackRange = 1,
    attackEffect = Some(Damage(1)),
    defense = Some(2),
    swarmMax = 3,
    abilities = List(MoveFirestorm)
  )

  val wraith = createPieceStats(
    name = "wraith",
    cost = 6,
    rebate = 2,
    moveRange = 3,
    isFlying = true,
    isLumbering = true,
    attackRange = 2,
    attackEffect = Some(Damage(4)),
    defense = Some(9),
    isPersistent = true,
  )

  val fiend = createPieceStats(
    name = "fiend",
    cost = 9,
    rebate = 3,
    moveRange = 1,
    attackRange = 1,
    attackEffect = Some(Damage(1)),
    numAttacks = 9,
    defense = Some(9),
    abilities = List(Abilities.airstrike)
  )

  val banshee = createPieceStats(
    name = "banshee",
    cost = 6,
    rebate = 3,
    moveRange = 2,
    attackRange = 1,
    attackEffect = Some(Kill),
    defense = Some(2),
    isPersistent = true,
    canHurtNecromancer = false,
    abilities = List(Abilities.double_attack),
  )

  val elemental = createPieceStats(
    name = "elemental",
    shortDisplayName = "Element",
    cost = 8,
    rebate = 4,
    moveRange = 1,
    attackRange = 3,
    attackEffect = Some(Damage(3)),
    defense = Some(3),
    abilities = List(MoveTerrain)
  )

  val fallen_angel = createPieceStats(
    name = "fallen_angel",
    shortDisplayName = "FAngel",
    displayName = "Fallen Angel",
    cost = 13,
    rebate = 6,
    moveRange = 2,
    isFlying = true,
    attackRange = 2,
    attackEffect = Some(Damage(1)),
    numAttacks = 4,
    defense = Some(7),
    spawnRange = Some(1),
  )

  val shadowlord = createPieceStats(
    name = "shadowlord",
    shortDisplayName = "SLord",
    cost = 13,
    rebate = 9,
    moveRange = 2,
    isFlying = true,
    attackRange = 1,
    attackEffect = Some(Damage(7)),
    defense = Some(13),
    isPersistent = true,
  )

  //All pieces
  val pieces: Array[PieceStats] = Array(
    necromancer,
    arcane_necromancer,
    ranged_necromancer,
    mounted_necromancer,
    immortal_necromancer,
    deadly_necromancer,
    battle_necromancer,
    mana_necromancer,
    zombie_necromancer,
    //swarm_necromancer,
    //summoner_necromancer,
    zombie, acolyte, spire,
    initiate, skeleton, serpent, bat, ghost, wight, haunt, shrieker,
    fog, dark_tower, witch, vampire, mummy, lich, sorcerer, void, hell_hound,
    wraith, fiend, banshee, elemental, fallen_angel, shadowlord
  )

  //Necromancers awarded after a board resets
  val specialNecromancers: Array[PieceStats] = Array(
    arcane_necromancer,
    ranged_necromancer,
    mounted_necromancer,
    immortal_necromancer,
    deadly_necromancer,
    battle_necromancer,
    mana_necromancer,
    zombie_necromancer,
    //swarm_necromancer,
    //summoner_necromancer,
  )

  //Pieces that start off buyable
  val alwaysAcquiredPieces: Array[PieceStats] = Array(
    zombie,
    acolyte,
    spire
  )

  //Pieces that need to be unlocked, in order
  val techPieces: Array[PieceStats] = Array(
    initiate,
    skeleton,
    serpent,
    bat,
    ghost,
    wight,
    haunt,
    shrieker,
    fog,
    dark_tower,
    sorcerer,
    witch,
    vampire,
    mummy,
    lich,
    void,
    hell_hound,
    wraith,
    fiend,
    banshee,
    elemental,
    fallen_angel,
    shadowlord
  )

  //Generally, we store and send the PieceName everywhere in the protocol, since unlike a PieceStats it's easily serialized.
  //This is the global map that everything uses to look up the stats again from the name.
  val pieceMap: Map[PieceName,PieceStats] = pieces.groupBy(piece => piece.name).mapValues { pieces =>
    assert(pieces.length == 1)
    pieces.head
  }

}

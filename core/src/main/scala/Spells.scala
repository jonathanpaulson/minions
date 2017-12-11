package minionsgame.core
import scala.util.{Try,Success,Failure}

case object Spells {

  //OFFENSE---------------------------------------------------------------------------

  val fester = TargetedSpell(
    name = "fester",
    displayName = "Fester",
    shortDisplayName = "Fester",
    desc = List("Deal 1 damage to target minion", "that was dealt damage this turn."),
    spellType = NormalSpell,
    spawnPhaseOnly = false,
    tryCanTarget = { (_: Side, piece:Piece, board:BoardState) =>
      if(piece.curStats(board).isNecromancer) Failure(new Exception("Can only target minions"))
      else if(piece.damage <= 0) Failure(new Exception("Can only target damaged pieces"))
      else Success(())
    },
    effect = Damage(1)
  )

  val cleaveSunderTarget = { (side: Side, target:Piece, board:BoardState) =>
    if(target.curStats(board).isNecromancer) Failure(new Exception("Can only target minions"))
    else if(target.side == side) Failure(new Exception("Can only target enemies"))
    else {
      val existsCleaverInRange =
        board.pieceById.values.exists { piece =>
          val areEnemies = piece.side != target.side
          val inRange = board.topology.distance(target.loc, piece.loc) <= piece.curStats(board).attackRange
          val attackedAnother = piece.attackedPiecesThisTurn.exists { attacked => attacked.id != target.id }
          areEnemies && inRange && attackedAnother
        }
      if(!existsCleaverInRange) Failure(new Exception("No friendly piece in range that attacked a different minion this turn"))
      else Success(())
    }
  }
  val cleave = TargetedSpell(
    name = "cleave",
    displayName = "Cleave",
    shortDisplayName = "Cleave",
    desc = List("Deal 1 damage to target minion", "within range of one of your minions", "that attacked a *different* minion this turn"),
    spellType = NormalSpell,
    spawnPhaseOnly = false,
    tryCanTarget = cleaveSunderTarget,
    effect = Damage(1)
  )
  val sunder = TargetedSpell(
    name = "sunder",
    displayName = "Sunder",
    shortDisplayName = "Sunder",
    desc = List("Deal 2 damage to target minion", "within range of one of your minions", "that attacked a *different* minion this turn"),
    spellType = Sorcery,
    spawnPhaseOnly = false,
    tryCanTarget = cleaveSunderTarget,
    effect = Damage(2)
  )

  val unsummon = TargetedSpell(
    name = "unsummon",
    displayName = "Unsummon",
    shortDisplayName = "Unsumm",
    desc = List("Unsummon target damaged minion."),
    spellType = NormalSpell,
    spawnPhaseOnly = false,
    tryCanTarget = {(side: Side, piece:Piece, board:BoardState) =>
      val pieceStats = piece.curStats(board)
      if(pieceStats.isNecromancer) Failure(new Exception("Can only target minions"))
      else if(piece.damage <= 0) Failure(new Exception("Can only target damaged pieces"))
      else if(pieceStats.isPersistent) Failure(new Exception("Target is persistent"))
      else Success(())
    },
    effect = Unsummon
  )

  val dismember = TargetedSpell(
    name = "dismember",
    displayName = "Dismember",
    shortDisplayName = "Dismemb",
    desc = List("Deal 3 damage to target minion", "that was dealt damage this turn."),
    spellType = Sorcery,
    spawnPhaseOnly = false,
    tryCanTarget = { (_: Side, piece:Piece, board:BoardState) =>
      if(piece.curStats(board).isNecromancer) Failure(new Exception("Can only target minions"))
      else if(piece.damage <= 0) Failure(new Exception("Can only target damaged pieces"))
      else Success(())
    },
    effect = Damage(3)
  )

  val lightningBolt = TargetedSpell(
    name = "lightning_bolt",
    displayName = "Lightning Bolt",
    shortDisplayName = "Bolt",
    desc = List("Deal 1 damage to target enemy minion."),
    spellType = Sorcery,
    spawnPhaseOnly = false,
    tryCanTarget = { (side: Side, piece:Piece, board:BoardState) =>
      if(piece.side == side || piece.curStats(board).isNecromancer) Failure(new Exception("Can only target enemy minions"))
      else Success(())
    },
    effect = Damage(1)
  )

  val unholyMight = TargetedSpell(
    name = "unholy_might",
    displayName = "Unholy Might",
    shortDisplayName = "UMight",
    desc = List("Target friendly necromancer can", "attack twice this turn."),
    spellType = NormalSpell,
    spawnPhaseOnly = false,
    tryCanTarget = ((side: Side, piece:Piece, board:BoardState) =>
      if(piece.side != side || !piece.curStats(board).isNecromancer) Failure(new Exception("Can only target friendly necromancers"))
      else Success(())
    ),
    effect = Enchant(PieceModWithDuration(PieceMods.DoubleAttack, turnsLeft=Some(1)))
  )

  //DEFENSE---------------------------------------------------------------------------

  val shield = TargetedSpell(
    name = "shield",
    displayName = "Shield",
    shortDisplayName = "Shield",
    desc = List("Target friendly minion is persistent.", "Double its health until the start", "of your next turn."),
    spellType = NormalSpell,
    spawnPhaseOnly = false,
    tryCanTarget = { (side: Side, piece:Piece, board:BoardState) =>
      if(piece.side != side || piece.curStats(board).isNecromancer) Failure(new Exception("Can only target friendly minions"))
      else Success(())
    },
    effect = Enchant(PieceModWithDuration(PieceMods.Shielded, turnsLeft=Some(2)))
  )

  val protection = TargetedSpell(
    name = "protection",
    displayName = "Protection",
    shortDisplayName = "Protect",
    desc = List("Target friendly minion gets +2 health until", "the end of your next turn."),
    spellType = Cantrip,
    spawnPhaseOnly = false,
    tryCanTarget = { (side: Side, piece:Piece, board:BoardState) =>
      if(piece.side != side || piece.curStats(board).isNecromancer) Failure(new Exception("Can only target friendly minions"))
      else Success(())
    },
    effect = Enchant(PieceModWithDuration(PieceMods.Protected, turnsLeft=Some(2)))
  )

  val freezeRay = TargetedSpell(
    name = "freeze_ray",
    displayName = "Freeze Ray",
    shortDisplayName = "Freeze",
    desc = List("Target enemy minion cannot attack", "until the start of your next turn"),
    spellType = Sorcery,
    spawnPhaseOnly = false,
    tryCanTarget = { (side: Side, piece:Piece, board:BoardState) =>
      if(piece.side == side || piece.curStats(board).isNecromancer) Failure(new Exception("Can only target enemy minions"))
      else Success(())
    },
    effect = Enchant(PieceModWithDuration(PieceMods.Frozen, turnsLeft=Some(2)))
  )

  val weaken = TargetedSpell(
    name = "weaken",
    displayName = "Weaken",
    shortDisplayName = "Weaken",
    desc = List("Target enemy minion has -1 attack", "until the start of your next turn", "(minions with < 1 attack cannot attack)."),
    spellType = Cantrip,
    spawnPhaseOnly = false,
    tryCanTarget = ((side: Side, piece:Piece, board:BoardState) =>
      if(piece.side == side || piece.curStats(board).isNecromancer) Failure(new Exception("Can only target enemy minions"))
      else Success(())
    ),
    effect = Enchant(PieceModWithDuration(PieceMods.Weakened, turnsLeft=Some(2)))
  )

  val lumbering = TargetedSpell(
    name = "lumbering",
    displayName = "Lumbering",
    shortDisplayName = "Lumber",
    desc = List("Target enemy minion is lumbering", "until the start of your next turn."),
    spellType = NormalSpell,
    spawnPhaseOnly = false,
    tryCanTarget = ((side: Side, piece:Piece, board:BoardState) =>
      if(piece.side == side || piece.curStats(board).isNecromancer) Failure(new Exception("Can only target enemy minions"))
      else Success(())
    ),
    effect = Enchant(PieceModWithDuration(PieceMods.Lumbering, turnsLeft=Some(2)))
  )

  val shackle = TargetedSpell(
    name = "shackle",
    displayName = "Shackle",
    shortDisplayName = "Shackle",
    desc = List("Reduce target enemy minion to move range 1", "and attack range 1 until the start of", "your next turn."),
    spellType = NormalSpell,
    spawnPhaseOnly = false,
    tryCanTarget = ((side: Side, piece:Piece, board:BoardState) =>
      if(piece.side == side || piece.curStats(board).isNecromancer) Failure(new Exception("Can only target enemy minions"))
      else Success(())
    ),
    effect = Enchant(PieceModWithDuration(PieceMods.Shackled, turnsLeft=Some(2)))
  )

  //UTILITY---------------------------------------------------------------------------

  val reposition = PieceAndLocSpell(
    name = "reposition",
    displayName = "Reposition",
    shortDisplayName = "Repos",
    desc = List("Move target friendly minion to an adjacent location."),
    spellType = NormalSpell,
    spawnPhaseOnly = true,
    tryCanTargetPiece = ((side: Side, piece:Piece) =>
      if(piece.side != side || piece.baseStats.isNecromancer) Failure(new Exception("Can only target friendly minions"))
      else Success(())
    ),
    tryCanTarget = ((side: Side, piece:Piece, loc: Loc, board: BoardState) =>
      if(piece.side != side || piece.baseStats.isNecromancer) Failure(new Exception("Can only target friendly minions"))
      else if(board.topology.distance(loc,piece.loc) != 1) Failure(new Exception("Location is not adjacent"))
      else if(board.pieces(loc).nonEmpty) Failure(new Exception("Adjacent location is not empty"))
      else board.tryCanEndOnLoc(side, piece.spec, piece.baseStats, piece.curStats(board), loc, List())
    ),
    effect = { (board: BoardState, piece: Piece, loc: Loc) =>
      board.doMovePieceToLoc(piece,loc)
      piece.actState = Spawning
    }
  )
  val displace = PieceAndLocSpell(
    name = "displace",
    displayName = "Displace",
    shortDisplayName = "Displace",
    desc = List("Move target enemy minion to an adjacent location."),
    spellType = NormalSpell,
    spawnPhaseOnly = true,
    tryCanTargetPiece = ((side: Side, piece:Piece) =>
      if(piece.side == side || piece.baseStats.isNecromancer) Failure(new Exception("Can only target enemy minions"))
      else Success(())
    ),
    tryCanTarget = ((side: Side, piece:Piece, loc: Loc, board: BoardState) =>
      if(piece.side == side || piece.baseStats.isNecromancer) Failure(new Exception("Can only target enemy minions"))
      else if(board.topology.distance(loc,piece.loc) != 1) Failure(new Exception("Location is not adjacent"))
      else if(board.pieces(loc).nonEmpty) Failure(new Exception("Adjacent location is not empty"))
      else board.tryCanEndOnLoc(side, piece.spec, piece.baseStats, piece.curStats(board), loc, List())
    ),
    effect = { (board: BoardState, piece: Piece, loc: Loc) =>
      board.doMovePieceToLoc(piece,loc)
    }
  )

  val stumble = PieceAndLocSpell(
    name = "stumble",
    displayName = "Stumble",
    shortDisplayName = "Stumble",
    desc = List("Move target damaged minion to an adjacent location."),
    spellType = NormalSpell,
    spawnPhaseOnly = false,
    tryCanTargetPiece = ((side: Side, piece:Piece) =>
      if(piece.side == side || piece.baseStats.isNecromancer) Failure(new Exception("Can only target enemy minions"))
      else if(piece.damage <= 0) Failure(new Exception("Can only target damaged pieces"))
      else Success(())
    ),
    tryCanTarget = ((side: Side, piece:Piece, loc: Loc, board: BoardState) =>
      if(piece.side == side || piece.baseStats.isNecromancer) Failure(new Exception("Can only target enemy minions"))
      else if(piece.damage <= 0) Failure(new Exception("Can only target damaged pieces"))
      else if(board.topology.distance(loc,piece.loc) != 1) Failure(new Exception("Location is not adjacent"))
      else if(board.pieces(loc).nonEmpty) Failure(new Exception("Adjacent location is not empty"))
      else board.tryCanEndOnLoc(side, piece.spec, piece.baseStats, piece.curStats(board), loc, List())
    ),
    effect = { (board: BoardState, piece: Piece, loc: Loc) =>
      board.doMovePieceToLoc(piece,loc)
    }
  )

  val spawn = TargetedSpell(
    name = "spawn",
    displayName = "Spawn",
    shortDisplayName = "Spawn",
    desc = List("Target friendly minion can spawn", "until end of turn."),
    spellType = NormalSpell,
    spawnPhaseOnly = false,
    tryCanTarget = ((side: Side, piece:Piece, board:BoardState) =>
      if(piece.side != side || piece.curStats(board).isNecromancer) Failure(new Exception("Can only target friendly minions"))
      else if(piece.actState == DoneActing) Failure(new Exception("Piece cannot spawn or act this turn"))
      else Success(())
    ),
    effect = Enchant(PieceModWithDuration(PieceMods.Spawner, turnsLeft=Some(1)))
  )

  val blink = TargetedSpell(
    name = "blink",
    displayName = "Blink",
    shortDisplayName = "Blink",
    desc = List("Unsummon target friendly minion,", "even if it is persistent."),
    spellType = NormalSpell,
    spawnPhaseOnly = true,
    tryCanTarget = ((side: Side, piece:Piece, board:BoardState) =>
      if(piece.side != side || piece.curStats(board).isNecromancer) Failure(new Exception("Can only target friendly minions"))
      else Success(())
    ),
    effect = Unsummon
  )

  val raiseZombie = TileSpell(
    name = "raise_zombie",
    displayName = "Raise Zombie",
    shortDisplayName = "RZombie",
    desc = List("Spawn a free zombie on target location", "next to a friendly unit."),
    spellType = Cantrip,
    spawnPhaseOnly = false,
    tryCanTarget = ((side: Side, loc: Loc, board: BoardState) =>
      if(board.pieces(loc).nonEmpty) Failure(new Exception("Target location is not empty"))
      else {
        val adjacentToFriendly = board.pieceById.values.exists { piece =>
          piece.side == side && board.topology.distance(loc,piece.loc) == 1
        }
        if(!adjacentToFriendly) Failure(new Exception("Target location is not adjacent to a friendly unit"))
        else board.trySpawnIsLegal(side, Units.zombie.name, loc)
      }
    ),
    effect = { (board: BoardState, loc: Loc) =>
      val (_: Option[Piece]) = board.spawnPieceInternal(board.side, Units.zombie.name, loc)
    }
  )

  val canMoveTerrain = { (side: Side, loc: Loc, board: BoardState) =>
    if(board.pieces(loc).nonEmpty) Failure(new Exception("Target location is not empty"))
    else if(board.tiles(loc).terrain != Ground) Failure(new Exception("Target terrain is not Ground"))
    else Success(())
  }
  def moveTerrain(terrain: Terrain) = {
    { (board: BoardState, loc: Loc) =>
        board.tiles.transform { tile =>
          if(tile.terrain == terrain) {
            Tile(Ground, modsWithDuration = tile.modsWithDuration)
          } else {
            tile
          }
        }
        board.tiles(loc) = Tile(terrain, modsWithDuration = board.tiles(loc).modsWithDuration)
    }
  }

  val earthquake = TileSpell(
    name = "earthquake",
    displayName = "Earthquake",
    shortDisplayName = "Earth",
    desc = List("Move the Earthquake to target empty Ground hex", "(only passable by unit types with at least two speed)"),
    spellType = NormalSpell,
    spawnPhaseOnly = true,
    tryCanTarget = canMoveTerrain,
    effect = moveTerrain(Earthquake)
  )
  val firestorm = TileSpell(
    name = "firestorm",
    displayName = "Firestorm",
    shortDisplayName = "Fire",
    desc = List("Move the Firestorm to target empty Ground hex", "(only passable by unit types with at least four health)"),
    spellType = NormalSpell,
    spawnPhaseOnly = true,
    tryCanTarget = canMoveTerrain,
    effect = moveTerrain(Firestorm)
  )
  val flood = TileSpell(
    name = "flood",
    displayName = "Flood",
    shortDisplayName = "Water",
    desc = List("Move the Flood to target empty Ground hex", "(only passable by flying unit types)"),
    spellType = NormalSpell,
    spawnPhaseOnly = true,
    tryCanTarget = canMoveTerrain,
    effect = moveTerrain(Flood)
  )
  val whirlwind = TileSpell(
    name = "whirlwind",
    displayName = "Whirlwind",
    shortDisplayName = "Air",
    desc = List("Move the Whirlwind to target empty Ground hex", "(only passable by persistent unit types)"),
    spellType = NormalSpell,
    spawnPhaseOnly = true,
    tryCanTarget = canMoveTerrain,
    effect = moveTerrain(Whirlwind)
  )

  val normalize = TileSpell(
    name = "normalize",
    displayName = "Normalize",
    shortDisplayName = "Normalize",
    desc = List("Remove one of the four terrain tiles"),
    spellType = Cantrip,
    spawnPhaseOnly = true,
    tryCanTarget = ((side: Side, loc: Loc, board: BoardState) =>
        board.tiles(loc).terrain match {
          case Earthquake | Firestorm | Flood | Whirlwind => Success(())
          case Wall | Water | Ground | Graveyard | SorceryNode | Teleporter | StartHex(_) | Spawner(_) | Mist =>
            Failure(new Exception("Must target earthquake, firestorm, flood, or whirlwind"))
        }
    ),
    effect = { (board: BoardState, loc: Loc) =>
      board.tiles(loc) = Tile(Ground, modsWithDuration = board.tiles(loc).modsWithDuration)
    }
  )

  val doubleCantrip = NoEffectSpell(
    name = "doubleCantrip",
    displayName = "Double Cantrip",
    shortDisplayName = "DCant",
    desc = List("Gain 2 sorcery power when played or discarded."),
    spellType = DoubleCantrip,
    spawnPhaseOnly = false,
  )

  val spells = Array(
    fester,
    unsummon,
    dismember,
    lightningBolt,
    unholyMight,

    shield,
    protection,
    freezeRay,
    weaken,
    lumbering,
    shackle,

    reposition,
    displace,
    stumble,
    spawn,
    blink,
    raiseZombie,
    doubleCantrip,

    earthquake,
    firestorm,
    flood,
    whirlwind,
    normalize,

    cleave,
    sunder,
  )
  val spellMap: Map[SpellName,Spell] = spells.groupBy(spell => spell.name).mapValues { spells =>
    assert(spells.length == 1)
    spells.head
  }

  def createDeck(): List[SpellName] = {
    List(
      (fester,10),
      (unsummon, 10),
      (reposition, 10),
      (shield, 10),
      (stumble, 10),

      (dismember, 2),
      (lightningBolt, 2),
      (unholyMight, 2),

      (protection, 2),
      (freezeRay, 2),
      (weaken, 2),
      (lumbering, 2),
      (shackle, 2),

      (displace, 2),
      (spawn, 2),
      (blink, 2),
      (raiseZombie, 2),
      (doubleCantrip, 2),

      (earthquake, 2),
      (firestorm, 2),
      (flood, 2),
      (whirlwind, 2),
      (normalize, 2),

      (cleave, 2),
      (sunder, 2),
      // TODO SPELLS:
      // ReinforcementAndLocSpell
      // warp (sorcery) (spawn phase) (2) Spawn target minion in your reinforcements in any empty Ground hex
      // lesser spawn (cantrip) (spawn phase) (2) Spawn target minion in your reinforcements next to target (friendly?) minion

      // NoTargetSpell
      // Drain (2) (cantrip) - opponent has -1 sorcery power next turn (can sacrifice any unit to pay for it)
      // Rising horde (sorcery) (2) - all your units have spawn

      // terraform (spawn phase) (2) Move one of the four terrain tiles to target empty Ground hex

      // PieceAndPieceSpell
      // Critical hit (sorcery) (2) - Target minion takes damage from the previous attack again
      // Portal (spawn phase) (2) exchange the position of two friendly minions

      // Fireball (2) - Pick a hex. Any enemy units there at the end of your opponent's turn are destroyed

      // Stored Strength (2) - exchange this for one of the spells in the spell row
      // Repeat (2) (sorcery) - take a spell you played this turn back into your hand
      // Ward (2) - Your necromancer(s?) and all adjacent minions get +1 health
    ).flatMap {case (spell:Spell, count:Int) =>
      List.fill(count)(spell.name)
    }
  }
}

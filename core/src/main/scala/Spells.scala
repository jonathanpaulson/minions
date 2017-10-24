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
    tryCanTarget = { (_: Side, piece:Piece, pieceStats:PieceStats) =>
      if(pieceStats.isNecromancer) Failure(new Exception("Can only target minions"))
      else if(piece.damage <= 0) Failure(new Exception("Can only target damaged pieces"))
      else Success(())
    },
    effect = Damage(1)
  )

  val unsummon = TargetedSpell(
    name = "unsummon",
    displayName = "Unsummon",
    shortDisplayName = "Unsumm",
    desc = List("Unsummon target damaged minion."),
    spellType = NormalSpell,
    spawnPhaseOnly = false,
    tryCanTarget = ((side: Side, piece:Piece, pieceStats:PieceStats) =>
      if(pieceStats.isNecromancer) Failure(new Exception("Can only target minions"))
      else if(piece.damage <= 0) Failure(new Exception("Can only target damaged pieces"))
      else if(pieceStats.isPersistent) Failure(new Exception("Target is persistent"))
      else Success(())
    ),
    effect = Unsummon
  )

  val dismember = TargetedSpell(
    name = "dismember",
    displayName = "Dismember",
    shortDisplayName = "Dismemb",
    desc = List("Deal 3 damage to target minion", "that was dealt damage this turn."),
    spellType = Sorcery,
    spawnPhaseOnly = false,
    tryCanTarget = { (_: Side, piece:Piece, pieceStats:PieceStats) =>
      if(pieceStats.isNecromancer) Failure(new Exception("Can only target minions"))
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
    tryCanTarget = { (side: Side, piece:Piece, pieceStats:PieceStats) =>
      if(piece.side == side || pieceStats.isNecromancer) Failure(new Exception("Can only target enemy minions"))
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
    tryCanTarget = ((side: Side, piece:Piece, pieceStats:PieceStats) =>
      if(piece.side != side || !pieceStats.isNecromancer) Failure(new Exception("Can only target friendly necromancers"))
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
    tryCanTarget = { (side: Side, piece:Piece, pieceStats:PieceStats) =>
      if(piece.side != side || pieceStats.isNecromancer) Failure(new Exception("Can only target friendly minions"))
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
    tryCanTarget = { (side: Side, piece:Piece, pieceStats:PieceStats) =>
      if(piece.side != side || pieceStats.isNecromancer) Failure(new Exception("Can only target friendly minions"))
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
    tryCanTarget = { (side: Side, piece:Piece, pieceStats:PieceStats) =>
      if(piece.side == side || pieceStats.isNecromancer) Failure(new Exception("Can only target enemy minions"))
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
    tryCanTarget = ((side: Side, piece:Piece, pieceStats:PieceStats) =>
      if(piece.side == side || pieceStats.isNecromancer) Failure(new Exception("Can only target enemy minions"))
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
    tryCanTarget = ((side: Side, piece:Piece, pieceStats:PieceStats) =>
      if(piece.side == side || pieceStats.isNecromancer) Failure(new Exception("Can only target enemy minions"))
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
    tryCanTarget = ((side: Side, piece:Piece, pieceStats:PieceStats) =>
      if(piece.side == side || pieceStats.isNecromancer) Failure(new Exception("Can only target enemy minions"))
      else Success(())
    ),
    effect = Enchant(PieceModWithDuration(PieceMods.Shackled, turnsLeft=Some(2)))
  )

  //UTILITY---------------------------------------------------------------------------

  val spawn = TargetedSpell(
    name = "spawn",
    displayName = "Spawn",
    shortDisplayName = "Spawn",
    desc = List("Target friendly minion can spawn", "until end of turn."),
    spellType = NormalSpell,
    spawnPhaseOnly = false,
    tryCanTarget = ((side: Side, piece:Piece, pieceStats:PieceStats) =>
      if(piece.side != side || pieceStats.isNecromancer) Failure(new Exception("Can only target friendly minions"))
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
    tryCanTarget = ((side: Side, piece:Piece, pieceStats:PieceStats) =>
      if(piece.side != side || pieceStats.isNecromancer) Failure(new Exception("Can only target friendly minions"))
      else Success(())
    ),
    effect = Unsummon
  )

  val raiseZombie = TileSpell(
    name = "raise_zombie",
    displayName = "Raise Zombie",
    shortDisplayName = "RZombie",
    desc = List("Spawn a free zombie on target location", "next to a friendly unit."),
    spellType = NormalSpell,
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

    spawn,
    blink,
    raiseZombie,
    doubleCantrip,
  )
  val spellMap: Map[SpellName,Spell] = spells.groupBy(spell => spell.name).mapValues { spells =>
    assert(spells.length == 1)
    spells.head
  }

  def createDeck(): List[SpellName] = {
    List(
      fester,fester,fester,fester,fester,fester,
      unsummon,unsummon,unsummon,unsummon,unsummon,unsummon,
      dismember,dismember,
      lightningBolt,lightningBolt,
      unholyMight,unholyMight,

      shield,shield,shield,shield,shield,shield,
      protection,protection,
      freezeRay,freezeRay,
      weaken,weaken,
      lumbering,lumbering,
      shackle,shackle,

      spawn,spawn,spawn,spawn,
      blink,blink,blink,blink,
      raiseZombie,raiseZombie,raiseZombie,raiseZombie,
      doubleCantrip,doubleCantrip,
    ).map(_.name)
  }
}

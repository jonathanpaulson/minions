package minionsgame.core
import scala.util.{Try,Success,Failure}

case object Spells {
  val fester = TargetedSpell(
    name = "fester",
    displayName = "Fester",
    shortDisplayName = "Fester",
    desc = List("Deal 1 damage to target minion", "that was dealt damage this turn."),
    spellType = NormalSpell,
    tryCanTarget = { (_: Side, piece:Piece, pieceStats:PieceStats) =>
      if(pieceStats.isNecromancer) Failure(new Exception("Can only target minions"))
      else if(piece.damage <= 0) Failure(new Exception("Can only target damaged pieces"))
      else Success(())
    },
    effect = Damage(1)
  )

  val dismember = TargetedSpell(
    name = "dismember",
    displayName = "Dismember",
    shortDisplayName = "Dismemb",
    desc = List("Deal 3 damage to target minion", "that was dealt damage this turn."),
    spellType = Sorcery,
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
    tryCanTarget = { (side: Side, piece:Piece, pieceStats:PieceStats) =>
      if(piece.side == side || pieceStats.isNecromancer) Failure(new Exception("Can only target enemy minions"))
      else Success(())
    },
    effect = Damage(1)
  )

  val freeze = TargetedSpell(
    name = "freeze",
    displayName = "Freeze",
    shortDisplayName = "Freeze",
    desc = List("Target enemy minion cannot attack", "until the start of your next turn"),
    spellType = Sorcery,
    tryCanTarget = { (side: Side, piece:Piece, pieceStats:PieceStats) =>
      if(piece.side == side || pieceStats.isNecromancer) Failure(new Exception("Can only target enemy minions"))
      else Success(())
    },
    effect = Enchant(PieceModWithDuration(PieceMods.Frozen, turnsLeft=Some(2)))
  )

  val shield = TargetedSpell(
    name = "shield",
    displayName = "Shield",
    shortDisplayName = "Shield",
    desc = List("Target friendly minion's defense is doubled", "and is persistent until the start of your next turn."),
    spellType = NormalSpell,
    tryCanTarget = { (side: Side, piece:Piece, pieceStats:PieceStats) =>
      if(piece.side != side || pieceStats.isNecromancer) Failure(new Exception("Can only target friendly minions"))
      else Success(())
    },
    effect = Enchant(PieceModWithDuration(PieceMods.Shield, turnsLeft=Some(2)))
  )

  val unsummon = TargetedSpell(
    name = "unsummon",
    displayName = "Unsummon",
    shortDisplayName = "Unsumm",
    desc = List("Unsummon target damaged minion."),
    spellType = NormalSpell,
    tryCanTarget = ((side: Side, piece:Piece, pieceStats:PieceStats) =>
      if(pieceStats.isNecromancer) Failure(new Exception("Can only target minions"))
      else if(piece.damage <= 0) Failure(new Exception("Can only target damaged pieces"))
      else if(pieceStats.isPersistent) Failure(new Exception("Target is persistent"))
      else Success(())
    ),
    effect = Unsummon
  )

  val spawn = TargetedSpell(
    name = "spawn",
    displayName = "Spawn",
    shortDisplayName = "Spawn",
    desc = List("Target friendly minion can spawn", "until end of turn."),
    spellType = NormalSpell,
    tryCanTarget = ((side: Side, piece:Piece, pieceStats:PieceStats) =>
      if(piece.side != side || pieceStats.isNecromancer) Failure(new Exception("Can only target friendly minions"))
      else if(piece.actState == DoneActing) Failure(new Exception("Piece cannot spawn or act this turn"))
      else Success(())
    ),
    effect = Enchant(PieceModWithDuration(PieceMods.Spawner, turnsLeft=Some(1)))
  )

  val slow = TargetedSpell(
    name = "slow",
    displayName = "Slow",
    shortDisplayName = "Slow",
    desc = List("Target enemy minion is lumbering", "until the start of your next turn."),
    spellType = NormalSpell,
    tryCanTarget = ((side: Side, piece:Piece, pieceStats:PieceStats) =>
      if(piece.side == side || pieceStats.isNecromancer) Failure(new Exception("Can only target enemy minions"))
      else Success(())
    ),
    effect = Enchant(PieceModWithDuration(PieceMods.Slow, turnsLeft=Some(2)))
  )

  val shackle = TargetedSpell(
    name = "shackle",
    displayName = "Shackle",
    shortDisplayName = "Shackle",
    desc = List("Target enemy minion has move speed 1", "until the start of your next turn"),
    spellType = NormalSpell,
    tryCanTarget = ((side: Side, piece:Piece, pieceStats:PieceStats) =>
      if(piece.side == side || pieceStats.isNecromancer) Failure(new Exception("Can only target enemy minions"))
      else Success(())
    ),
    effect = Enchant(PieceModWithDuration(PieceMods.Shackled, turnsLeft=Some(2)))
  )

  val blink = TargetedSpell(
    name = "blink",
    displayName = "Blink",
    shortDisplayName = "Blink",
    desc = List("Unsummon target friendly minion."),
    spellType = NormalSpell,
    tryCanTarget = ((side: Side, piece:Piece, pieceStats:PieceStats) =>
      if(piece.side != side || pieceStats.isNecromancer) Failure(new Exception("Can only target friendly minions"))
      else if(pieceStats.isPersistent) Failure(new Exception("Target is persistent"))
      else Success(())
    ),
    effect = Unsummon
  )

  val spells = Array(fester,dismember,lightningBolt,freeze,shield,unsummon,spawn,slow,shackle,blink)
  val spellMap: Map[SpellName,Spell] = spells.groupBy(spell => spell.name).mapValues { spells =>
    assert(spells.length == 1)
    spells.head
  }

  def createDeck(): List[SpellName] = {
    List(
      fester,fester,
      dismember,
      lightningBolt,
      freeze,
      shield,shield,
      unsummon,unsummon,
      spawn,spawn,
      slow,slow,
      shackle,shackle,
      blink,blink
    ).map(_.name)
  }
}

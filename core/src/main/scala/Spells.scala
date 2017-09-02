package minionsgame.core

case object Spells {
  val fester = TargetedSpell(
    name = "fester",
    displayName = "Fester",
    desc = "Deal 1 damage to target minion that was dealt damage this turn.",
    spellType = NormalSpell,
    canTarget = ((side: Side, piece:Piece) => piece.side != side && !piece.curStats.isNecromancer && piece.damage > 0),
    targetError = "Can target only minions and only if they were dealt damage",
    effect = Damage(1)
  )

  val spells = Array(fester)
  val spellMap: Map[SpellName,Spell] = spells.groupBy(spell => spell.name).mapValues { spells =>
    assert(spells.length == 1)
    spells.head
  }

}

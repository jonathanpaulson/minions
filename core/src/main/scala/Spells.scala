
case object Spells {
  val festerSpell = TargetedSpell(
    key = "fester_spell",
    name = "Fester",
    desc = "Deal 1 damage to target minion that was dealt damage this turn.",
    spellType = NormalSpell,
    canTarget = ((side: Side, piece:Piece) => piece.side != side && !piece.curStats.isNecromancer && piece.damage > 0),
    targetError = "Can target only minions and only if they were dealt damage",
    effect = Damage(1)
  )
}



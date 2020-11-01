package minionsgame.core

import scala.util.{Try,Success,Failure}
import scala.collection.immutable.Vector

import RichImplicits._

//Holds potentially-nonpublic information separate from the boards that might be necessary for the boards
//to perform legality checking. Putting it in this separate class makes it easy to keep the handling of this
//information clean and control when and how it gets updated.
case class ExternalInfo(
  val pieceMap: Map[PieceName, PieceStats],
  //Map of all spells that have been revealed
  var spellsRevealed: Map[SpellId,SpellName]
) {
  def revealSpells(spellIdsAndNames: Array[(SpellId,SpellName)]) = {
    spellIdsAndNames.foreach { case (spellId,spellName) =>
      spellsRevealed = spellsRevealed + (spellId -> spellName)
    }
  }
}
object ExternalInfo {
  def create(pieceMap : Map[PieceName, PieceStats]): ExternalInfo = {
    new ExternalInfo(pieceMap, spellsRevealed = Map())
  }
}

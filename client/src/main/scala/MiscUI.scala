package minionsgame.jsclient

import minionsgame.core._
import RichImplicits._

object Reinforcements {
  def getLocs(side: Side, flipDisplay: Boolean, board: BoardState): Array[Loc] = {
    val locs = Array(
      Loc(-4,7),Loc(-3,7),Loc(-2,7),
      Loc(-4,8),Loc(-3,8),Loc(-2,8),
      Loc(-5,9),Loc(-4,9),Loc(-3,9),Loc(-2,9),
      Loc(-5,10),Loc(-4,10),Loc(-3,10),Loc(-2,10),
      Loc(-6,11),Loc(-5,11),Loc(-4,11),Loc(-3,11),Loc(-2,11),
      Loc(-6,12),Loc(-5,12),Loc(-4,12),Loc(-3,12),Loc(-2,12)
    )

    (side,flipDisplay) match {
      case (S0,false) | (S1,true) =>
        locs
      case (S1,false) | (S0,true) =>
        locs.map { loc => Loc(board.tiles.xSize - loc.x - 1, board.tiles.ySize - loc.y - 1) }
    }
  }

  def getLocsAndContents(side: Side, flipDisplay: Boolean, board: BoardState): Array[(Loc,PieceName,Int)] = {
    val locs = getLocs(side,flipDisplay,board)
    var i = 0
    Units.pieces.flatMap { stats =>
      board.reinforcements(side).get(stats.name) match {
        case None => None
        case Some(count) =>
          val loc = locs(i)
          i += 1
          Some((loc,stats.name,count))
      }
    }
  }

  def getSelectedLocAndCount(side: Side, flipDisplay: Boolean, board: BoardState, pieceName: PieceName): Option[(Loc,Int)] = {
    val locs = getLocs(side,flipDisplay,board)
    var i = 0
    Units.pieces.findMap { stats =>
      board.reinforcements(side).get(stats.name) match {
        case None => None
        case Some(count) =>
          val loc = locs(i)
          if(stats.name == pieceName)
            Some((loc,count))
          else {
            i += 1
            None
          }
      }
    }
  }

  def getSelectedPiece(side: Side, flipDisplay: Boolean, board: BoardState, loc: Loc): Option[PieceName] = {
    val locs = getLocs(side,flipDisplay,board)
    val selectedIdx = locs.indexOf(loc)
    if(selectedIdx == -1) None
    else {
      var i = 0
      Units.pieces.findMap { stats =>
        board.reinforcements(side).get(stats.name) match {
          case None => None
          case Some(_) =>
            if(i == selectedIdx) Some(stats.name)
            else {
              i += 1
              None
            }
        }
      }
    }
  }

}

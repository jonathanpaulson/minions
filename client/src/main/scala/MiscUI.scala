package minionsgame.jsclient

import minionsgame.core._
import RichImplicits._

object UI {
  //How much to translate the canvas origin inward from the upper left corner.
  val translateOrigin = PixelVec(3.0 * Drawing.gridSize, 7.0 * Drawing.gridSize)

  //Positioning for text about game stats and mana
  object Info {
    def getLoc(side: Side, flipDisplay: Boolean, board: BoardState): Loc = {
      (side,flipDisplay) match {
        case (S0,false) | (S1,true) =>
          Loc(1,-4)
        case (S1,false) | (S0,true) =>
          Loc(board.tiles.xSize/2 + 4, -4)
      }
    }
  }

  //Positioning for end turn hex button
  object EndTurn {
    val loc: Loc = Loc(16, -3)
  }
  object ResignBoard {
    val loc: Loc = Loc(18, -3)
  }
  object Sidebar {
    val loc: Loc = Loc(19, 3)
  }

  //Positioning for tech line
  object Tech {
    val xOffset = 1
    val yOffset = -3

    def getLoc(techIdx: Int): Loc = {
      Loc((techIdx/2)+xOffset, yOffset + (if(techIdx%2==0) 0 else 1))
    }

    def getLocs(game: Game): Array[Loc] = {
      (0 until game.techLine.length).map { i => getLoc(i) }.toArray
    }

    def getSelectedTechIdx(game: Game, loc: Loc): Option[Int] = {
      val idx = {
        if(loc.y == yOffset) (loc.x-xOffset)*2
        else if(loc.y == yOffset+1) (loc.x-xOffset)*2 + 1
        else -1
      }
      if(idx >= 0 && idx < game.techLine.length)
        Some(idx)
      else
        None
    }

  }

  object DeadPieces {
    def getDescLoc(board: BoardState): Loc = {
      Loc(-board.tiles.ySize / 2 - 1, board.tiles.ySize + 1)
    }

    def getLocsAndContents(board: BoardState): Array[(Loc,PieceSpec,PieceName,Side)] = {
      board.killedThisTurn.reverse.zipWithIndex.map { case ((spec,pieceName,side),i) =>
        val loc = Loc(-board.tiles.ySize / 2 + i, board.tiles.ySize + 1)
        (loc,spec,pieceName,side)
      }.toArray
    }

    def getSelectedSpec(board: BoardState, loc: Loc): Option[PieceSpec] = {
      val locs = getLocsAndContents(board)
      locs.findMap { case (l,spec,_,_) => if(loc == l) Some(spec) else None }
    }

    def getSelectedLoc(board: BoardState, pieceSpec: PieceSpec): Option[Loc] = {
      val locs = getLocsAndContents(board)
      locs.findMap { case (l,spec,_,_) => if(pieceSpec == spec) Some(l) else None }
    }

    def getSelectedPiece(board: BoardState, pieceSpec: PieceSpec): Option[(PieceStats, Side)] = {
      val locs = getLocsAndContents(board)
      locs.findMap { case (_,spec,pieceName,side) => if(pieceSpec == spec) Some((Units.pieceMap(pieceName), side)) else None }
    }
  }

  //Positioning for reinforcements
  object Reinforcements {
    val unflippedLocs = Array(
      Loc(-4,7),Loc(-3,7),Loc(-2,7),
      Loc(-4,8),Loc(-3,8),Loc(-2,8),
      Loc(-5,9),Loc(-4,9),Loc(-3,9),Loc(-2,9),
      Loc(-5,10),Loc(-4,10),Loc(-3,10),Loc(-2,10),
      Loc(-6,11),Loc(-5,11),Loc(-4,11),Loc(-3,11),Loc(-2,11),
      Loc(-6,12),Loc(-5,12),Loc(-4,12),Loc(-3,12),Loc(-2,12)
    )

    def getLocs(side: Side, flipDisplay: Boolean, board: BoardState): Array[Loc] = {
      (side,flipDisplay) match {
        case (S0,false) | (S1,true) =>
          unflippedLocs
        case (S1,false) | (S0,true) =>
          unflippedLocs.map { loc => Loc(board.tiles.xSize - loc.x - 1, board.tiles.ySize - loc.y - 1) }
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

    def getSelectedPiece(flipDisplay: Boolean, board: BoardState, loc: Loc): Option[(PieceName, Side)] = {
      Side.sides.findMap { side =>
        val locs = getLocs(side,flipDisplay,board)
        val selectedIdx = locs.indexOf(loc)
        if(selectedIdx == -1) None
        else {
          var i = 0
          Units.pieces.findMap { stats =>
            board.reinforcements(side).get(stats.name) match {
              case None => None
              case Some(_) =>
                if(i == selectedIdx) Some((stats.name, side))
                else {
                  i += 1
                  None
                }
            }
          }
        }
      }
    }

  }

}

package minionsgame.jsclient

import minionsgame.core._
import RichImplicits._

object UI {
  //How much to translate the canvas origin inward from the upper left corner.
  val translateOrigin = PixelVec(4.65 * Drawing.gridSize, 10 * Drawing.gridSize)

  //One component or panel or set of controls of the UI.
  sealed trait Component {
    val origin: HexLoc
    val gridSizeScale: Double

    def hexLoc(loc: Loc): HexLoc = {
      HexLoc(loc.x.toDouble * gridSizeScale + origin.x, loc.y.toDouble * gridSizeScale + origin.y)
    }
    def getGridSizeScale(): Double = gridSizeScale

    protected def getLocAndDelta(hexLoc: HexLoc): (Loc, HexVec) = {
      HexLoc((hexLoc.x - origin.x) / gridSizeScale, (hexLoc.y - origin.y) / gridSizeScale).round()
    }
  }

  sealed trait Clickable {
    def getMouseTarget(game: Game, board: BoardState, hexLoc: HexLoc): MouseTarget
  }
}

case class UI(val flipDisplay: Boolean, val ourSide: Option[Side], val boardXSize: Int, val boardYSize: Int)
{
  object TopInfo extends UI.Component {
    val origin = HexLoc(0,0)
    val gridSizeScale = 1

    def getHexLoc(side: Side): HexLoc = {
      (side,flipDisplay) match {
        case (S0,false) | (S1,true) =>
          HexLoc(1.3,-5.9)
        case (S1,false) | (S0,true) =>
          HexLoc(boardXSize.toDouble/2 + 5.7, -5.9)
      }
    }
  }

  object TitleInfo extends UI.Component {
    val origin = HexLoc(boardXSize.toDouble * 0.5 + 1.25, -1.85)
    val gridSizeScale = 1
  }

  object PrevBoard extends UI.Component with UI.Clickable {
    val origin = HexLoc(1.25,-1.8)
    val gridSizeScale = 1
    val locs: Array[Loc] = Array(Loc(0,0),Loc(1,0))
    val hexLocs = locs.map(hexLoc(_))

    def getMouseTarget(game: Game, board: BoardState, hexLoc: HexLoc): MouseTarget = {
      val _ = (game,board)
      val (loc,_) = getLocAndDelta(hexLoc)
      if(locs.contains(loc)) MousePrevBoard
      else MouseNone
    }
  }

  object NextBoard extends UI.Component with UI.Clickable {
    val origin = HexLoc(boardXSize.toDouble + 1.25, -1.8)
    val gridSizeScale = 1
    val locs: Array[Loc] = Array(Loc(0,0),Loc(1,0))
    val hexLocs = locs.map(hexLoc(_))

    def getMouseTarget(game: Game, board: BoardState, hexLoc: HexLoc): MouseTarget = {
      val _ = (game,board)
      val (loc,_) = getLocAndDelta(hexLoc)
      if(locs.contains(loc)) MouseNextBoard
      else MouseNone
    }
  }

  object BoardInfo extends UI.Component {
    val origin = HexLoc(0,0)
    val gridSizeScale = 1

    def getHexLoc(side: Side): HexLoc = {
      (side,flipDisplay) match {
        case (S0,false) | (S1,true) =>
          HexLoc(-1,-1)
        case (S1,false) | (S0,true) =>
          HexLoc(boardXSize.toDouble - 3, -1)
      }
    }
  }

  object Terrain extends UI.Component with UI.Clickable {
    val origin = HexLoc(5.5,-1.1)
    val gridSizeScale = 0.4
    val terrains:  Array[Terrain] = Array(Earthquake(true), Water(true), Whirlwind(true), Firestorm(true))

    def getMouseTarget(game: Game, board: BoardState, hexLoc: HexLoc): MouseTarget = {
      val (loc,_) = getLocAndDelta(hexLoc)
      if(loc.y == 0 && 0<=loc.x && loc.x<terrains.length) {
        MouseTerrain(terrains(loc.x), loc)
      } else {
        MouseNone
      }
    }
  }

  //Positioning for end turn hex button
  object Actions extends UI.Component with UI.Clickable {
    val origin = HexLoc(14.35,-1.7)
    val gridSizeScale = 1.2

    override def getGridSizeScale(): Double = 1.0

    def hexLoc(i: Int): HexLoc = {
      HexLoc(i.toDouble * gridSizeScale + origin.x, origin.y)
    }
    def getMouseTarget(game: Game, board: BoardState, hexLoc: HexLoc): MouseTarget = {
      val _ = (game,board)
      val (loc,_) = getLocAndDelta(hexLoc)
      if(loc == Loc.zero) MouseEndTurn(loc)
      else if(loc == Loc(1, 0)) MouseResignBoard(loc)
      else if(loc == Loc(2, 0)) MouseRedo(loc)
      else MouseNone
    }
  }

  object Clock extends UI.Component with UI.Clickable {
    val origin = HexLoc(20.0,-5.5)
    val gridSizeScale = 1

    def getMouseTarget(game: Game, board: BoardState, hexLoc: HexLoc): MouseTarget = {
      val _ = (game,board)
      val (loc,_) = getLocAndDelta(hexLoc)
      if(loc == Loc.zero) MousePause(loc)
      else MouseNone
    }
  }

  object Options extends UI.Component with UI.Clickable {
    val origin = HexLoc(19.25, -4.0)
    val gridSizeScale = 1
    def getMouseTarget(game: Game, board: BoardState, hexLoc: HexLoc): MouseTarget = {
      val _ = (game,board)
      val (loc,_) = getLocAndDelta(hexLoc)
      if(loc == Loc.zero) MouseCoords(loc)
      else MouseNone
    }
  }

  object Sidebar extends UI.Component {
    val origin = HexLoc(20.5,-0.5)
    val gridSizeScale = 1
  }

  object Tech extends UI.Component with UI.Clickable {
    val origin = HexLoc(1,-5) + HexVec(0.05,-0.1)
    val gridSizeScale = 1

    def getLoc(techIdx: Int): Loc = {
      Loc(techIdx/2, (if(techIdx%2==0) 0 else 1))
    }

    def getHexLocs(game: Game): Array[HexLoc] = {
      (0 until game.techLine.length).map { i => hexLoc(getLoc(i)) }.toArray
    }

    def getHexLocsAndContents(game: Game): Array[(HexLoc,TechState)] = {
      game.techLine.zipWithIndex.map { case (tech,i) => (hexLoc(getLoc(i)),tech) }.toArray
    }

    def getMouseTarget(game: Game, board: BoardState, hexLoc: HexLoc): MouseTarget = {
      val _ = (board)
      val (loc,_) = getLocAndDelta(hexLoc)

      val idx = {
        if(loc.y == 0) loc.x*2
        else if(loc.y == 1) loc.x*2 + 1
        else -1
      }
      if(idx >= 0 && idx < game.techLine.length)
        MouseTech(idx, loc)
      else
        MouseNone
    }
  }

  object ExtraTechAndSpell extends UI.Component with UI.Clickable {
    val origin = HexLoc(16.55,-5.1)
    val gridSizeScale = 1

    def getMouseTarget(game: Game, board: BoardState, hexLoc: HexLoc): MouseTarget = {
      val _ = (game,board)
      val (loc,_) = getLocAndDelta(hexLoc)
      if(loc == Loc.zero) MouseExtraTechAndSpell(loc)
      else MouseNone
    }
  }

  object SpellChoice extends UI.Component with UI.Clickable {
    val origin = HexLoc(0.5,-2) + HexVec(0.425,-0.85)
    val gridSizeScale = 1
    val size = 12

    def getLoc(spellChoiceIdx: Int): Loc = {
      Loc(spellChoiceIdx,0)
    }

    def getMouseTarget(game: Game, board: BoardState, hexLoc: HexLoc): MouseTarget = {
      val _ = (game,board)
      val (loc,_) = getLocAndDelta(hexLoc)
      val idx = loc.x
      if(loc.y == 0 && idx >= 0 && idx < size) {
        val (spellId, side) = game.resolveSpellChoice(idx, ourSide)
        spellId match {
          case None => MouseNone
          case Some(spellId) => MouseSpellChoice(spellId, side, loc)
        }
      }
      else MouseNone
    }
  }

  object SpellPlayed extends UI.Component with UI.Clickable {
    val origin = HexLoc(boardYSize.toDouble / 2 - 1, boardYSize.toDouble + 1) + HexVec(-0.3,-0.6)
    val maxShown = 8
    val gridSizeScale = 1
    val descLoc: HexLoc = hexLoc(Loc(-1,0))

    def getLocs(): Array[Loc] = {
      Array.range(0, maxShown).map { i => Loc(i, 0) }
    }

    def getHexLocsAndContents(board: BoardState): Array[(HexLoc,SpellId,Side,Option[SpellOrAbilityTargets])] = {
      board.spellsPlayed.reverse.take(maxShown).zipWithIndex.map { case (info, i) =>
        (hexLoc(Loc(i,0)), info.spellId, info.side, info.targets)
      }.toArray
    }

    def getMouseTarget(game: Game, board: BoardState, hexLoc: HexLoc): MouseTarget = {
      val _ = (game)
      val (loc,_) = getLocAndDelta(hexLoc)
      val idx = loc.x
      if(loc.y == 0 && idx >= 0 && idx < maxShown) {
        if(idx < board.spellsPlayed.length) {
          MouseSpellPlayed(Some(board.spellsPlayed.reverse(idx)), loc)
        } else {
          MouseSpellPlayed(None, loc)
        }
      }
      else
        MouseNone
    }
  }

  object DeadPieces extends UI.Component with UI.Clickable {
    val origin = HexLoc(-boardYSize.toDouble / 2, boardYSize.toDouble + 1) + HexVec(-0.3,-0.6)
    val gridSizeScale = 1

    val descLoc: HexLoc = hexLoc(Loc(-1,0))

    def getHexLocsAndContents(board: BoardState): Array[(HexLoc,PieceSpec,PieceName,Side)] = {
      // -1 for spells played label
      val maxSize = (SpellPlayed.origin.x-1-origin.x).toInt
      board.killedThisTurn.reverse.zipWithIndex.map { case ((spec,pieceName,side,_),i) =>
        (hexLoc(Loc(i,0)),spec,pieceName,side)
      }.toArray.take(maxSize)
    }
    def getSelectedPiece(board: BoardState, pieceSpec: PieceSpec): Option[(PieceName, Side)] = {
      board.killedThisTurn.findMap { case (spec,pieceName,side,_) =>
        if(pieceSpec == spec) Some((pieceName, side)) else None
      }
    }

    def getMouseTarget(game: Game, board: BoardState, hexLoc: HexLoc): MouseTarget = {
      val _ = (game)
      val (loc,_) = getLocAndDelta(hexLoc)
      val idx = loc.x
      if(loc.y == 0 && idx >= 0 && idx < board.killedThisTurn.length) {
        val reversed = board.killedThisTurn.reverse
        val (spec,_,_,_) = reversed(idx)
        MouseDeadPiece(spec, loc)
      }
      else
        MouseNone
    }
  }

  object SpellHand extends UI.Component with UI.Clickable {
    val origin = HexLoc(0.5, 0)
    val gridSizeScale = 1

    val unflippedLocs = Array(
      Loc(-5,7),Loc(-4,7),Loc(-3,7),Loc(-2,7),
      Loc(-6,8),Loc(-5,8),Loc(-4,8),Loc(-3,8),Loc(-2,8),
      Loc(-6,9),Loc(-5,9),Loc(-4,9),Loc(-3,9),Loc(-2,9),
      //Loc(-6,10),Loc(-5,10),Loc(-4,10),Loc(-3,10),Loc(-2,10),
      //Loc(-7,11),Loc(-6,11),Loc(-5,11),Loc(-4,11),Loc(-3,11),Loc(-2,11),
      //Loc(-7,12),Loc(-6,12),Loc(-5,12),Loc(-4,12),Loc(-3,12),Loc(-2,12),
    )

    def getLocs(side: Side): Array[Loc] = {
      (side,flipDisplay) match {
        case (S0,false) | (S1,true) =>
          unflippedLocs
        case (S1,false) | (S0,true) =>
          unflippedLocs.map { loc => Loc(boardXSize - loc.x - 2, boardYSize - loc.y - 1) }
      }
    }

    def getMouseTarget(game: Game, board: BoardState, hexLoc: HexLoc): MouseTarget = {
      val _ = (game)
      val (loc,_) = getLocAndDelta(hexLoc)

      Side.sides.findMap { side =>
        val selectedIdx = getLocs(side).indexOf(loc)
        if(selectedIdx == -1 || selectedIdx >= board.spellsInHand(side).length) None
        else Some(MouseSpellHand(board.spellsInHand(side)(selectedIdx), side, loc))
      }.getOrElse(MouseNone)
    }
  }

  //Positioning for reinforcements
  object Reinforcements extends UI.Component with UI.Clickable {
    val origin = HexLoc(0.5,0)
    val gridSizeScale = 1

    val unflippedLocs = Array(
      Loc(-3,2),Loc(-2,2),
      Loc(-3,3),Loc(-2,3),
      Loc(-4,4),Loc(-3,4),Loc(-2,4),
      Loc(-4,5),Loc(-3,5),Loc(-2,5),
      Loc(-5,6),Loc(-4,6),Loc(-3,6),Loc(-2,6),
    )

    def getLocs(side: Side): Array[Loc] = {
      (side,flipDisplay) match {
        case (S0,false) | (S1,true) =>
          unflippedLocs
        case (S1,false) | (S0,true) =>
          unflippedLocs.map { loc => Loc(boardXSize - loc.x - 2, boardYSize - loc.y - 1) }
      }
    }

    def getHexLocsAndContents(side: Side, board: BoardState, pieces: Array[PieceName]): Array[(HexLoc,PieceName,Int)] = {
      val locs = getLocs(side)
      var i = 0
      pieces.flatMap { name =>
        board.reinforcements(side).get(name) match {
          case None => None
          case Some(count) =>
            //If the user has so many types of reinforcements that it overflows, then we just won't draw them all...
            if(i < locs.length) {
              val loc = locs(i)
              i += 1
              Some((hexLoc(loc),name,count))
            }
            else
              None
        }
      }
    }

    def getMouseTarget(game: Game, board: BoardState, hexLoc: HexLoc): MouseTarget = {
      val (loc,_) = getLocAndDelta(hexLoc)

      Side.sides.findMap { side =>
        val locs = getLocs(side)
        val selectedIdx = locs.indexOf(loc)
        if(selectedIdx == -1) None
        else {
          var i = 0
          game.pieceMap.keys.toArray.findMap { name =>
            board.reinforcements(side).get(name) match {
              case None => None
              case Some(_) =>
                if(i == selectedIdx)
                  Some(MouseReinforcement(Some(name), side, loc))
                else {
                  i += 1
                  None
                }
            }
          }.orElse(Some(MouseReinforcement(None, side, loc)))
        }
      }.getOrElse(MouseNone)
    }
  }

  //Positioning for the main board
  object MainBoard extends UI.Component with UI.Clickable {
    val origin = HexLoc(0,0)
    val gridSizeScale = 1

    private def getPiece(loc: Loc, hexDelta: HexVec, board: BoardState): Option[Piece] = {
      board.pieces(loc) match {
        case Nil => None
        case p :: Nil => Some(p)
        case p1 :: p2 :: Nil =>
          hexDelta.closestCorner() match {
            case 0 | 4 | 5 => Some(p1)
            case 1 | 2 | 3 => Some(p2)
          }
        case p1 :: p2 :: p3 :: Nil =>
          hexDelta.hexant() match {
            case 4 | 5 => Some(p1)
            case 2 | 3 => Some(p2)
            case 0 | 1 => Some(p3)
            case _ => assertUnreachable()
          }
        case _ => None
      }
    }

    override def hexLoc(loc: Loc): HexLoc = {
      if(flipDisplay)
        HexLoc((boardXSize-loc.x-1).toDouble * gridSizeScale + origin.x, (boardYSize-loc.y-1).toDouble * gridSizeScale + origin.y)
      else
        HexLoc(loc.x.toDouble * gridSizeScale + origin.x, loc.y.toDouble * gridSizeScale + origin.y)
    }

    def getMouseTarget(game: Game, board: BoardState, hexLoc: HexLoc): MouseTarget = {
      val _ = (game)
      val (unflippedLoc,hexDelta) = getLocAndDelta(hexLoc)
      val loc = {
        if(flipDisplay)
          Loc(boardXSize - unflippedLoc.x - 1, boardYSize - unflippedLoc.y - 1)
        else
          unflippedLoc
      }

      if(!board.pieces.inBounds(loc))
        MouseNone
      else {
        getPiece(loc,hexDelta,board) match {
          case Some(piece) => MousePiece(piece.spec,loc)
          case None => MouseTile(loc)
        }
      }
    }
  }

  val clickableComponents: List[UI.Clickable] = List(
    MainBoard,
    Reinforcements,
    DeadPieces,
    Tech,
    Actions,
    PrevBoard,
    NextBoard,
    ExtraTechAndSpell,
    SpellChoice,
    SpellHand,
    SpellPlayed,
    Terrain,
    Clock,
    Options,
  )

}

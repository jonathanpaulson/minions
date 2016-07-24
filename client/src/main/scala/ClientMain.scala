package minionsgame.jsclient

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport
import org.scalajs.jquery.jQuery
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.MouseEvent
import org.scalajs.dom.html.Canvas
import scala.util.{Try, Success, Failure}

import minionsgame.core._
import RichImplicits._

object ClientMain extends JSApp {

  def main(): Unit = {
    //Call setupUI once the document is ready
    jQuery(setupUI _)
    ()
  }

  def setupUI() : Unit = {
    val canvas = jQuery("#board").get(0).asInstanceOf[Canvas]
    val ctx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

    //How much to translate the canvas origin inward from the upper left corner.
    val translateOrigin = PixelVec(2.0 * Drawing.gridSize, 6.0 * Drawing.gridSize)

    val topology = HexTopology
    val board = {
      val terrain: Plane[Terrain] = Plane.create(10, 10, topology, Ground)
      terrain(0,0) = ManaSpire
      terrain(0,1) = Wall
      terrain(2,4) = Wall
      terrain(1,0) = Water
      terrain(1,1) = Spawner(S0, Units.zombie)
      terrain(2,0) = Spawner(S1, Units.zombie)

      val boardState = BoardState.create(terrain)
      boardState.spawnPieceInitial(S0, Units.zombie, Loc(2,1))
      boardState.spawnPieceInitial(S0, Units.zombie, Loc(2,2))
      boardState.spawnPieceInitial(S0, Units.zombie, Loc(2,2))

      boardState.spawnPieceInitial(S0, Units.zombie, Loc(2,3))
      boardState.spawnPieceInitial(S0, Units.zombie, Loc(2,3))
      boardState.spawnPieceInitial(S0, Units.zombie, Loc(2,3))

      boardState.spawnPieceInitial(S1, Units.zombie, Loc(3,2))
      boardState.spawnPieceInitial(S1, Units.zombie, Loc(3,4))
      boardState.spawnPieceInitial(S1, Units.zombie, Loc(1,4))
      boardState.spawnPieceInitial(S1, Units.zombie, Loc(2,5))

      Board.create(boardState)
    }

    val game = Game(mana = SideArray.create(0), tech = SideArray.create(Array.fill[TechLevel](Units.techs.size)(T0)))
    game.tech(S1) = Array.fill[TechLevel](Units.techs.size)(T0)
    game.tech(S0)(3) = T1
    game.tech(S0)(4) = T1
    game.tech(S0)(5) = T1
    game.tech(S0)(6) = T2
    game.tech(S0)(7) = T2
    game.tech(S0)(8) = T2
    game.tech(S1)(1) = T1
    game.tech(S1)(4) = T1
    game.tech(S1)(7) = T1
    game.tech(S1)(2) = T2
    game.tech(S1)(5) = T2
    game.tech(S1)(8) = T2

    var selected : Option[PieceSpec] = None
    var mouse : Loc = Loc(0, 0)
    var path : List[Loc] = List()

    def draw() = {
      Drawing.drawEverything(canvas, ctx, board.curState, game, translateOrigin, hoverLoc = Some(mouse), selected, path)
    }

    // Update path to be a shortest path from [selected] to [mouse] that
    // shares the longest prefix with the current [path]
    def updatePath() : Unit = {
      selected.flatMap { selected => board.curState.findPiece(selected) } match {
        case None => ()
        case Some(piece) =>
          val moves = board.curState.legalMoves(piece, mouse)
          val distances = moves.transform { case (_k, (d, _not_has_enemy)) => d}
          if(moves.contains(piece.loc) && moves.contains(mouse) && moves(mouse)._2) {
            if(path.size==0 || path(0)!=piece.loc) {
              path = List(piece.loc)
            }
            while(path.length > 1 && distances(piece.loc) != path.length-1 + distances(path.last)) {
              path = path.init
            }
            while(path.last != mouse) {
              for(p <- topology.adj(path.last)) {
                if(distances.contains(p) && path.length-1 + distances(path.last) == path.length + distances(p)) {
                  path = path :+ p
                }
              }
            }
          } else if(moves.contains(mouse) && !moves(mouse)._2) { // Enemy unit(s)
            while(path.length > 1 && topology.distance(path.last, mouse) < piece.curStats.attackRange)
              path = path.init
          }
      }
    }

    def mousePixel(e : MouseEvent) : PixelLoc = {
      val rect = canvas.getBoundingClientRect()
      PixelLoc(e.clientX - rect.left, e.clientY - rect.top) - translateOrigin
    }
    def mouseHexLoc(e : MouseEvent) : HexLoc = {
      HexLoc.ofPixel(mousePixel(e), Drawing.gridSize)
    }

    def mousePiece(e : MouseEvent) : Option[Piece] = {
      val hexLoc = mouseHexLoc(e)
      val loc = hexLoc.round()
      val hexDelta = hexLoc - loc
      board.curState.pieces(loc) match {
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

    def mousedown(e : MouseEvent) : Unit = {
      selected = mousePiece(e).filter(piece => piece.side == board.curState.side).map(piece => piece.spec)
      draw()
    }
    def mouseup(e : MouseEvent) : Unit = {
      def doActions(actions : List[PlayerAction]) : Unit = {
        selected = None
        path = List()
        board.doAction(PlayerActions(actions)) match {
          case Success(()) => ()
          case Failure(error) =>
            println(error)
        }
      }
      selected.flatMap(spec => board.curState.findPiece(spec)) match {
        case None =>
          selected = mousePiece(e).filter(piece => piece.side == board.curState.side).map(piece => piece.spec)
        case Some(piece) =>
          mousePiece(e) match {
            case None => doActions(List(Movements(List(Movement(StartedTurnWithID(piece.id), path.toVector)))))
            case Some(other) =>
              if(other.side == piece.side) {
                doActions(List(Movements(List(Movement(StartedTurnWithID(piece.id), path.toVector)))))
              } else {
                if(path.length > 1) {
                  doActions(List(
                    Movements(List(Movement(StartedTurnWithID(piece.id), path.toVector))),
                    Attack(StartedTurnWithID(piece.id), other.loc, StartedTurnWithID(other.id))
                  ))
                } else {
                  doActions(List(Attack(StartedTurnWithID(piece.id), other.loc, StartedTurnWithID(other.id))))
                }
              }
          }
      }
      draw()
    }
    def mousemove(e : MouseEvent) : Unit = {
      mouse = mouseHexLoc(e).round()
      updatePath()
      draw()
    }

    canvas.onmousedown = mousedown _
    canvas.onmousemove = mousemove _
    canvas.onmouseup = mouseup _

    draw()

    ()
  }

  def addClickedMessage(): Unit = {
    jQuery("body").append("<p>You clicked!</p>")
    ()
  }
}

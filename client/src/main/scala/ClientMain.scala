package minionsgame.jsclient

import scala.scalajs.js.JSApp
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

    //TODO
    val side: Side = ???

    //State of boards including our own local edits ot them
    var localBoards: Array[Board] = Array()
    //State of boards as received from the server
    var serverBoards: Array[Board] = Array()
    var numBoards: Int = 0 //Length of the boards arrays
    var curBoardIdx: Int = 0 //Currently selected board

    //Mouse movement path state
    var selectedSpec : Option[PieceSpec] = None
    var hoverLoc : Option[Loc] = None
    var hoverSpec : Option[PieceSpec] = None
    var path : List[Loc] = List()

    def curBoard() : Option[Board] = {
      if(numBoards == 0) None
      else Some(localBoards(curBoardIdx))
    }

    def draw() : Unit = {
      curBoard().foreach { board =>
        Drawing.drawEverything(canvas, ctx, board.curState, translateOrigin, hoverLoc, hoverSpec, selectedSpec, path)
      }
    }

    //Update path to be a shortest path from [selected] to [hoverLoc] that
    //shares the longest prefix with the current [path]
    def updatePath() : Unit = {
      curBoard().foreach { board =>
        //If it's not our turn, then deselect all path stuff
        if(board.curState.side != side)
          path = List()
        else {
          val selectedPiece = selectedSpec.flatMap(spec => board.curState.findPiece(spec))
          val hoverPiece = hoverSpec.flatMap(spec => board.curState.findPiece(spec))
            (selectedPiece, hoverLoc) match {
            case (None,_) | (_,None) =>
              //If no piece is selected or the mouse is nowhere to be found, then clear the path
              path = List()

            case (Some(selectedPiece), Some(hoverLoc)) =>
              val newLegalMove = {
                val bState = board.curState
                hoverPiece match {
                  case None =>
                    //Ordinary move to location
                    bState.findLegalMove(selectedPiece,pathBias=path) { loc => loc == hoverLoc }
                  case Some(hoverPiece) =>
                    //Merge into friendly swarm
                    if(hoverPiece.side == selectedPiece.side)
                      bState.findLegalMove(selectedPiece,pathBias=path) { loc => loc == hoverLoc }
                    //Attack enemy piece
                    else {
                      val spStats = selectedPiece.curStats(bState)
                      val hpStats = hoverPiece.curStats(bState)
                      if(!bState.canAttack(spStats,attackerHasMoved=false,selectedPiece.actState,hpStats))
                        None
                      else {
                        bState.findLegalMove(selectedPiece,pathBias=path) { loc =>
                          bState.tiles.topology.distance(loc, hoverLoc) <= spStats.attackRange &&
                          (loc == selectedPiece.loc || !spStats.isLumbering)
                        }
                      }
                    }
                }
              }
              newLegalMove match {
                case None => ()
                case Some(newPath) =>
                  path = newPath
              }
          }
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
      curBoard() match {
        case None => None
        case Some(board) =>
          if(!board.curState.pieces.inBounds(loc))
            None
          else {
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
      }
    }

    def mousedown(e : MouseEvent) : Unit = {
      selectedSpec = mousePiece(e).filter(piece => piece.side == side).map(piece => piece.spec)
      path = List()
      draw()
    }
    def mouseup(e : MouseEvent) : Unit = {
      curBoard().foreach { board =>
        def doActions(actions : List[PlayerAction]) : Unit = {
          board.doAction(PlayerActions(actions)) match {
            case Success(()) => ()
            case Failure(error) => println(error)
          }
        }
        selectedSpec.flatMap(spec => board.curState.findPiece(spec)) match {
          case None => ()
          case Some(piece) =>
            mousePiece(e) match {
              case None =>
                if(path.length > 1)
                  doActions(List(Movements(List(Movement(piece.spec, path.toVector)))))
              case Some(other) =>
                if(other.side == piece.side) {
                  if(path.length > 1)
                    doActions(List(Movements(List(Movement(piece.spec, path.toVector)))))
                }
                else {
                  if(path.length > 1) {
                    doActions(List(
                      Movements(List(Movement(piece.spec, path.toVector))),
                      Attack(piece.spec, other.spec)
                    ))
                  }
                  else
                    doActions(List(Attack(piece.spec, other.spec)))
                }
            }
        }
      }
      selectedSpec = None
      path = List()
      draw()
    }
    def mousemove(e : MouseEvent) : Unit = {
      hoverLoc = Some(mouseHexLoc(e).round())
      hoverSpec = mousePiece(e).map(piece => piece.spec)
      updatePath()
      draw()
    }
    def mouseout(e : MouseEvent) : Unit = {
      val _ = e
      hoverLoc = None
      hoverSpec = None
      selectedSpec = None
      updatePath()
      draw()
    }

    canvas.onmousedown = mousedown _
    canvas.onmousemove = mousemove _
    canvas.onmouseup = mouseup _
    canvas.onmouseout = mouseout _

    draw()

    ()
  }

  def addClickedMessage(): Unit = {
    jQuery("body").append("<p>You clicked!</p>")
    ()
  }
}

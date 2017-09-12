package minionsgame.jsclient

import scala.concurrent.Future
import scala.util.{Try, Success, Failure}

import scala.scalajs.js.JSApp
import org.scalajs.jquery.jQuery
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.MouseEvent
import org.scalajs.dom.html.Canvas
import org.scalajs.dom.window

import minionsgame.core._
import RichImplicits._

object ClientMain extends JSApp {

  def main(): Unit = {
    //Call setupUI once the document is ready
    jQuery(setupUI _)
    ()
  }

  val (username: String, side: Option[Side]) = {
    val params = (new java.net.URI(window.location.href)).getQuery()
    val fields = params.split("&").flatMap { piece =>
      piece.split("=").toList match {
        case Nil => None
        case _ :: Nil => None
        case k :: v :: Nil => Some((k,v))
        case _ :: _ :: _ :: _ => None
      }
    }.toMap
    val username = fields("username")
    val side = fields.get("side").map(Side.ofString)
    (username,side)
  }

  def reportError(err: String) = {
    //TODO display to user instead of printing to console
    println(err)
  }
  def reportFatalError(err: String) = {
    //TODO display to user instead of printing to console
    println(err)
  }

  def reportUserJoined(username: String, side: Option[Side]) = {
    //TODO
  }
  def reportUserLeft(username: String, side: Option[Side]) = {
    //TODO
  }

  def setupUI() : Unit = {
    val canvas = jQuery("#board").get(0).asInstanceOf[Canvas]
    val ctx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

    //How much to translate the canvas origin inward from the upper left corner.
    val translateOrigin = PixelVec(2.0 * Drawing.gridSize, 6.0 * Drawing.gridSize)

    //State of boards including our own local edits ot them
    var localBoards: Array[Option[Board]] = Array()
    //State of boards as received from the server
    var serverBoards: Array[Option[Board]] = Array()
    var numBoards: Int = 0 //Length of the boards arrays
    var curBoardIdx: Int = 0 //Currently selected board

    //Mouse movement path state
    var selectedSpec : Option[PieceSpec] = None
    var hoverLoc : Option[Loc] = None
    var hoverSpec : Option[PieceSpec] = None
    var path : List[Loc] = List()

    //Websocket connection!
    val connection = Connection(username,side)

    def sendWebsocketQuery(query: Protocol.Query): Unit = {
      //TODO
    }

    def handleWebsocketEvent(result: Try[Protocol.Response]): Unit = {
      result match {
        case Failure(exn) =>
          reportError("Websocket exn: " + exn.toString)
        case Success(event) =>
          event match {
            case Protocol.Version(version) =>
              if(CurrentVersion.version != version)
                reportFatalError("Minions client version " + CurrentVersion.version + " does not match server version " + version)
              else
                println("Running minions version " + version)
            case Protocol.QueryError(err) =>
              reportError("Error from server: " + err)
            case Protocol.UserJoined(username,side) =>
              reportUserJoined(username,side)
            case Protocol.UserLeft(username,side) =>
              reportUserLeft(username,side)

            //TODO
            case Protocol.OkBoardAction(boardIdx,newBoardSequence) =>
              val _ = (boardIdx,newBoardSequence)
            case Protocol.OkUndoBoardAction(boardIdx,newBoardSequence) =>
              val _ = (boardIdx,newBoardSequence)

            case Protocol.NumBoards(n) =>
              println("Setting numBoards to " + n)
              numBoards = n
              curBoardIdx = 0
              localBoards = Array.fill(n)(None)
              serverBoards = Array.fill(n)(None)

            //TODO deal with boardSequence properly, and localBoards
            case Protocol.ReportBoardAction(boardIdx,boardAction,newBoardSequence) =>
              val _ = newBoardSequence
              serverBoards(boardIdx).foreach { board =>
                board.doAction(boardAction) match {
                  case Success(()) => ()
                  case Failure(exn) => reportFatalError("Server sent illegal action: " + boardAction + " error: " + exn)
                }
              }

            //TODO deal with boardSequence properly, and localBoards
            case Protocol.ReportUndoBoardAction(boardIdx,newBoardSequence) =>
              val _ = newBoardSequence
              serverBoards(boardIdx).foreach { board =>
                board.undo() match {
                  case Success(()) => ()
                  case Failure(exn) => reportFatalError("Server sent illegal undo, error: " + exn)
                }
              }

            //TODO deal with boardSequence properly, and localBoards
            case Protocol.ReportBoardHistory(boardIdx,boardSummary,newBoardSequence) =>
              val _ = newBoardSequence
              serverBoards(boardIdx) = Some(Board.ofSummary(boardSummary))
          }
      }
    }

    def curBoard() : Option[Board] = {
      if(numBoards == 0) None
      else localBoards(curBoardIdx)
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
        //TODO
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

    val _ : Future[Unit] = connection.run(handleWebsocketEvent)

    ()
  }

  def addClickedMessage(): Unit = {
    jQuery("body").append("<p>You clicked!</p>")
    ()
  }
}

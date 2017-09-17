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
    val _ = (username,side)
    //TODO
  }
  def reportUserLeft(username: String, side: Option[Side]) = {
    val _ = (username,side)
    //TODO
  }

  def setupUI() : Unit = {
    val canvas = jQuery("#board").get(0).asInstanceOf[Canvas]
    val ctx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

    //How much to translate the canvas origin inward from the upper left corner.
    val translateOrigin = PixelVec(2.0 * Drawing.gridSize, 6.0 * Drawing.gridSize)

    //State of boards including our own local edits ot them
    var localBoards: Array[Board] = Array()
    var localSequence: Array[Int] = Array()
    var localActionSequence: Array[Vector[BoardActionOrUndoRedo]] = Array()
    var numActionsLocalAhead: Int = 0
    //State of boards as received from the server
    var serverBoards: Array[Board] = Array()
    var serverSequence: Array[Int] = Array()
    var serverActionSequence: Array[Vector[BoardActionOrUndoRedo]] = Array()

    var numBoards: Int = 0 //Length of the boards arrays
    var curBoardIdx: Int = 0 //Currently selected board

    var nextActionIdSuffix: Int = 0 //For generating unique action ids

    //Mouse movement path state
    var selectedSpec : Option[PieceSpec] = None
    var hoverLoc : Option[Loc] = None
    var hoverSpec : Option[PieceSpec] = None
    var path : List[Loc] = List()

    //Websocket connection!
    val connection = Connection(username,side)

    def sendWebsocketQuery(query: Protocol.Query): Unit = {
      connection.sendIfOpen(query)
    }

    //Upon receiving an update from the server, update local boards to be consistent
    def syncLocalAndServerBoards(boardIdx: Int): Unit = {
      var localActionsToReplay: Vector[BoardActionOrUndoRedo] = Vector()
      var foundDifference: Boolean = false
      for(i <- 0 until serverActionSequence(boardIdx).length) {
        //Found a difference between the server and the local history!
        if(serverActionSequence(boardIdx)(i) != localActionSequence(boardIdx)(i)) {
          //Store the local action to try to replay it afterward
          localActionsToReplay = localActionsToReplay :+ localActionSequence(boardIdx)(i)
          foundDifference = true
        }
      }
      if(foundDifference) {
        //Also grab all the extra actions afterwards locally
        for(i <- serverActionSequence(boardIdx).length until localActionSequence(boardIdx).length) {
          localActionsToReplay = localActionsToReplay :+ localActionSequence(boardIdx)(i)
        }
        localBoards(boardIdx) = serverBoards(boardIdx).copy()
        localSequence(boardIdx) = serverSequence(boardIdx)
        localActionSequence(boardIdx) = serverActionSequence(boardIdx)
        numActionsLocalAhead = 0

        localActionsToReplay.foreach { action =>
          val result = action match {
            case (a: BoardAction) => localBoards(boardIdx).doAction(a)
            case UndoAction(_) => localBoards(boardIdx).undo()
            case RedoAction(_) => localBoards(boardIdx).redo()
          }
          result match {
            case Failure(err) => reportError(err.toString)
            case Success(()) =>
              localSequence(boardIdx) = localSequence(boardIdx) + 1
              localActionSequence(boardIdx) = localActionSequence(boardIdx) :+ action
              numActionsLocalAhead = numActionsLocalAhead + 1
          }
        }
      }
    }

    //Wipe all history and restore the local boards to be the same as the server board
    def resetLocalBoards(boardIdx: Int): Unit = {
      localBoards(boardIdx) = serverBoards(boardIdx).copy()
      localSequence(boardIdx) = serverSequence(boardIdx)
      localActionSequence(boardIdx) = serverActionSequence(boardIdx)
      numActionsLocalAhead = 0
    }

    def handleResponse(response: Protocol.Response): Unit = {
      response match {
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

        case Protocol.OkBoardAction(_,_) =>
          ()
        case Protocol.OkUndoBoardAction(_,_,_) =>
          ()
        case Protocol.OkRedoBoardAction(_,_,_) =>
          ()

        case Protocol.InitializeBoards(summaries,boardSequences) =>
          println("Setting numBoards to " + summaries.length)
          numBoards = summaries.length
          curBoardIdx = 0
          serverBoards = summaries.map { summary => Board.ofSummary(summary) }
          serverSequence = boardSequences.clone()
          serverActionSequence = Array.fill(summaries.length)(Vector())
          localBoards = serverBoards.map { board => board.copy() }
          localSequence = serverSequence.clone()
          localActionSequence = Array.fill(summaries.length)(Vector())
          numActionsLocalAhead = 0

        case Protocol.ReportBoardAction(boardIdx,boardAction,newBoardSequence) =>
          serverBoards(boardIdx).doAction(boardAction) match {
            case Success(()) => ()
            case Failure(exn) => reportFatalError("Server sent illegal action: " + boardAction + " error: " + exn)
          }
          serverSequence(boardIdx) = newBoardSequence
          serverActionSequence(boardIdx) = serverActionSequence(boardIdx) :+ boardAction.asInstanceOf[BoardActionOrUndoRedo]
          syncLocalAndServerBoards(boardIdx)

        case Protocol.ReportUndoBoardAction(boardIdx,actionId,newBoardSequence) =>
          serverBoards(boardIdx).undo() match {
            case Success(()) => ()
            case Failure(exn) => reportFatalError("Server sent illegal undo, error: " + exn)
          }
          serverSequence(boardIdx) = newBoardSequence
          serverActionSequence(boardIdx) = serverActionSequence(boardIdx) :+ UndoAction(actionId)
          syncLocalAndServerBoards(boardIdx)

        case Protocol.ReportRedoBoardAction(boardIdx,actionId,newBoardSequence) =>
          serverBoards(boardIdx).redo() match {
            case Success(()) => ()
            case Failure(exn) => reportFatalError("Server sent illegal redo, error: " + exn)
          }
          serverSequence(boardIdx) = newBoardSequence
          serverActionSequence(boardIdx) = serverActionSequence(boardIdx) :+ RedoAction(actionId)
          syncLocalAndServerBoards(boardIdx)

        case Protocol.ReportBoardHistory(boardIdx,boardSummary,newBoardSequence) =>
          serverBoards(boardIdx) = Board.ofSummary(boardSummary)
          serverSequence(boardIdx) = newBoardSequence
          serverActionSequence(boardIdx) = Vector()
          resetLocalBoards(boardIdx)
      }
    }

    def handleWebsocketEvent(result: Try[Protocol.Response]): Unit = {
      result match {
        case Failure(exn) => reportError("Websocket exn: " + exn.toString)
        case Success(response) => handleResponse(response)
      }
    }

    def makeActionId(): String = {
      nextActionIdSuffix = nextActionIdSuffix + 1
      username + nextActionIdSuffix.toString
    }

    def doActionOnCurBoard(action : BoardAction) : Unit = {
      localBoards(curBoardIdx).doAction(action) match {
        case Failure(error) => reportError(error.toString)
        case Success(()) =>
          localSequence(curBoardIdx) = localSequence(curBoardIdx) + 1
          localActionSequence(curBoardIdx) = localActionSequence(curBoardIdx) :+ action.asInstanceOf[BoardActionOrUndoRedo]
          numActionsLocalAhead = numActionsLocalAhead + 1
          sendWebsocketQuery(Protocol.DoBoardAction(curBoardIdx,action))
      }
    }
    // def undoOnCurBoard() : Unit = {
    //   localBoards(curBoardIdx).undo() match {
    //     case Failure(error) => reportError(error.toString)
    //     case Success(()) =>
    //       val actionId = makeActionId()
    //       localSequence(curBoardIdx) = localSequence(curBoardIdx) + 1
    //       localActionSequence(curBoardIdx) = localActionSequence(curBoardIdx) :+ UndoAction(actionId)
    //       numActionsLocalAhead = numActionsLocalAhead + 1
    //       sendWebsocketQuery(Protocol.UndoBoardAction(curBoardIdx,actionId,localSequence(curBoardIdx)))
    //   }
    // }
    // def redoOnCurBoard() : Unit = {
    //   localBoards(curBoardIdx).redo() match {
    //     case Failure(error) => reportError(error.toString)
    //     case Success(()) =>
    //       val actionId = makeActionId()
    //       localSequence(curBoardIdx) = localSequence(curBoardIdx) + 1
    //       localActionSequence(curBoardIdx) = localActionSequence(curBoardIdx) :+ RedoAction(actionId)
    //       numActionsLocalAhead = numActionsLocalAhead + 1
    //       sendWebsocketQuery(Protocol.RedoBoardAction(curBoardIdx,actionId,localSequence(curBoardIdx)))
    //   }
    // }

    def curLocalBoard() : Option[Board] = {
      if(numBoards == 0) None
      else Some(localBoards(curBoardIdx))
    }

    def draw() : Unit = {
      curLocalBoard().foreach { board =>
        Drawing.drawEverything(canvas, ctx, board.curState, translateOrigin, hoverLoc, hoverSpec, selectedSpec, path)
      }
    }

    //Update path to be a shortest path from [selected] to [hoverLoc] that
    //shares the longest prefix with the current [path]
    def updatePath() : Unit = {
      curLocalBoard().foreach { board =>
        //If it's not our turn, then deselect all path stuff
        if(side != Some(board.curState.side))
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
      curLocalBoard() match {
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
      selectedSpec = side match {
        case None => None
        case Some(side) => mousePiece(e).filter(piece => piece.side == side).map(piece => piece.spec)
      }
      path = List()
      draw()
    }
    def mouseup(e : MouseEvent) : Unit = {
      def doActions(actions: List[PlayerAction]): Unit = {
        doActionOnCurBoard(PlayerActions(actions,makeActionId()))
      }

      curLocalBoard().foreach { board =>
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

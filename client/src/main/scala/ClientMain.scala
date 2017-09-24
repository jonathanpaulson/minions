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
    val translateOrigin = PixelVec(3.0 * Drawing.gridSize, 6.0 * Drawing.gridSize)

    //State of boards including our own local edits ot them
    var localBoards: Array[Board] = Array()
    var localSequence: Array[Int] = Array()
    var localActionSequence: Array[Vector[BoardAction]] = Array()
    var numActionsLocalAhead: Int = 0
    //State of boards as received from the server
    var serverBoards: Array[Board] = Array()
    var serverSequence: Array[Int] = Array()
    var serverActionSequence: Array[Vector[BoardAction]] = Array()

    var numBoards: Int = 0 //Length of the boards arrays
    var curBoardIdx: Int = 0 //Currently selected board

    var nextActionIdSuffix: Int = 0 //For generating unique action ids

    val flipDisplay: Boolean = side == Some(S1) //Flip so that 0,0 is in the lower right

    //Mouse movement path state
    val mouseState = MouseState()

    //Websocket connection!
    val connection = Connection(username,side)

    def sendWebsocketQuery(query: Protocol.Query): Unit = {
      connection.sendIfOpen(query)
    }

    //Upon receiving an update from the server, update local boards to be consistent
    def syncLocalAndServerBoards(boardIdx: Int): Unit = {
      var localActionsToReplay: Vector[BoardAction] = Vector()
      var foundDifference: Boolean = false
      //Look for difference between the server and the local history
      for(i <- 0 until serverActionSequence(boardIdx).length) {
        if(i >= localActionSequence(boardIdx).length)
          foundDifference = true
        else if(serverActionSequence(boardIdx)(i) != localActionSequence(boardIdx)(i)) {
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
          }
          result match {
            case Failure(err) => reportError(err.toString)
            case Success(()) =>
              localSequence(boardIdx) = localSequence(boardIdx) + 1
              localActionSequence(boardIdx) = localActionSequence(boardIdx) :+ action
              numActionsLocalAhead = numActionsLocalAhead + 1
          }
        }
        draw()
      }
    }

    //Wipe all history and restore the local boards to be the same as the server board
    def resetLocalBoards(boardIdx: Int): Unit = {
      localBoards(boardIdx) = serverBoards(boardIdx).copy()
      localSequence(boardIdx) = serverSequence(boardIdx)
      localActionSequence(boardIdx) = serverActionSequence(boardIdx)
      numActionsLocalAhead = 0
      draw()
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
        case Protocol.OkHeartbeat(_) =>
          ()
        case Protocol.UserJoined(username,side) =>
          reportUserJoined(username,side)
        case Protocol.UserLeft(username,side) =>
          reportUserLeft(username,side)

        case Protocol.OkBoardAction(_,_) =>
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
          serverActionSequence(boardIdx) = serverActionSequence(boardIdx) :+ boardAction
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
          localActionSequence(curBoardIdx) = localActionSequence(curBoardIdx) :+ action
          numActionsLocalAhead = numActionsLocalAhead + 1
          sendWebsocketQuery(Protocol.DoBoardAction(curBoardIdx,action))
      }
    }

    def curLocalBoard() : Option[Board] = {
      if(numBoards == 0) None
      else Some(localBoards(curBoardIdx))
    }

    def draw() : Unit = {
      curLocalBoard().foreach { board =>
        Drawing.drawEverything(canvas, ctx, board.curState, translateOrigin, mouseState, flipDisplay)
      }
    }

    def mousePixel(e : MouseEvent) : PixelLoc = {
      val rect = canvas.getBoundingClientRect()
      PixelLoc(e.clientX - rect.left, e.clientY - rect.top) - translateOrigin
    }

    def withBoardForMouse(f: Board => Unit): Unit = {
      curLocalBoard() match {
        case None => mouseState.clear()
        case Some(board) => f(board)
      }
    }

    def mousedown(e: MouseEvent) : Unit = {
      withBoardForMouse { board =>
        val pixelLoc = mousePixel(e)
        mouseState.handleMouseDown(pixelLoc,board,flipDisplay,side)
      }
      draw()
    }
    def mouseup(e : MouseEvent) : Unit = {
      def doActions(actions: List[PlayerAction]): Unit = {
        if(actions.length > 0)
          doActionOnCurBoard(PlayerActions(actions,makeActionId()))
      }
      withBoardForMouse { board =>
        val pixelLoc = mousePixel(e)
        mouseState.handleMouseUp(pixelLoc,board,flipDisplay)(doActions)
      }
      draw()
    }
    def mousemove(e : MouseEvent) : Unit = {
      withBoardForMouse { board =>
        val pixelLoc = mousePixel(e)
        mouseState.handleMouseMove(pixelLoc,board,flipDisplay,side)
      }
      draw()
    }
    def mouseout(e : MouseEvent) : Unit = {
      val _ = e
      mouseState.clear()
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

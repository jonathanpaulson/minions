package minionsgame.jsclient

import scala.concurrent.Future
import scala.util.{Try, Success, Failure}

import scala.scalajs.js.{JSApp, Dictionary}
import org.scalajs.jquery.{JQuery,jQuery,JQueryEventObject}
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.{MouseEvent, KeyboardEvent}
import org.scalajs.dom.html.{Canvas, TextArea}
import org.scalajs.dom.window

import scala.concurrent.ExecutionContext.Implicits.global

import minionsgame.core._
import RichImplicits._

object ClientMain extends JSApp {

  def main(): Unit = {
    //Call setupUI once the document is ready
    jQuery { () => new Client().init() }
    ()
  }
}

@scala.scalajs.js.native
trait Offset extends scala.scalajs.js.Object {
  def left : Double = scala.scalajs.js.native
  def top : Double = scala.scalajs.js.native
}

class Client() {
  val (username: String, password: Option[String], ourSide: Option[Side]) = {
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
    val password = fields.get("password")
    val ourSide = fields.get("side").map(Side.ofString)
    (username,password,ourSide)
  }
  def init(): Unit = {
    val jcanvas = jQuery(canvas)
    val jmessages = jQuery(messages)
    jmessages.height(canvas.height*0.5)
    jmessages.width(canvas.width*0.3)
    val canvas_offset = jcanvas.offset.asInstanceOf[Offset]
    val left = canvas_offset.left + jcanvas.width - jmessages.outerWidth(true)
    val top = canvas_offset.top + jcanvas.height - jmessages.outerHeight(true)
    val _ = jmessages.offset(Dictionary("left"->left, "top"->top))
  }

  var gotFatalError: Boolean = false

  def scrollMessages() = {
    messages.scrollTop = messages.scrollHeight.toDouble
  }

  def scrollMessagesIfAtEnd() = {
    if(messages.scrollTop + messages.clientHeight + 1 >= messages.scrollHeight)
      scrollMessages()
  }

  def reportMessage(msg: String) = {
    messages.value += msg + "\n"
    scrollMessagesIfAtEnd()
  }
  def reportError(err: String) = {
    reportMessage(err)
    scrollMessages()
  }
  def reportFatalError(err: String) = {
    reportMessage("FATAL ERROR: " + err)
    scrollMessages()
    gotFatalError = true
  }

  def reportUserJoined(username: String, side: Option[Side]) = {
    val sideStr = side match {
      case None => "as a spectator"
      case Some(side) => "team " + side.toColorName
    }
    reportMessage(username + " joined " + sideStr + "!")
  }
  def reportUserLeft(username: String, side: Option[Side]) = {
    val sideStr = side match {
      case None => "as a spectator"
      case Some(side) => "team " + side.toColorName
    }
    reportMessage(username + " left " + sideStr)
  }

  def addClickedMessage(): Unit = {
    jQuery("body").append("<p>You clicked!</p>")
    ()
  }

  val canvas = jQuery("#board").get(0).asInstanceOf[Canvas]
  val ctx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
  val messages = jQuery("#messages").get(0).asInstanceOf[TextArea]

  //State of game, as far as we can tell from the server
  var game: Option[Game] = None

  //State of boards including our own local edits ot them
  var localBoards: Array[Board] = Array()
  var localSequence: Array[Int] = Array()
  var localActionSequence: Array[Vector[BoardAction]] = Array()
  var numActionsLocalAhead: Int = 0
  //State of boards as received from the server
  var serverBoards: Array[Board] = Array()
  var serverSequence: Array[Int] = Array()
  var serverActionSequence: Array[Vector[BoardAction]] = Array()
  var serverBoardNames: Array[String] = Array()

  var numBoards: Int = 0 //Length of the boards arrays
  var curBoardIdx: Int = 0 //Currently selected board

  var nextActionIdSuffix: Int = 0 //For generating unique action ids

  //TODO Ctrl-click should perform BuyReinforcementUndo and LocalPieceUndo?
  //TODO Shift-click should allow performing swaps and triangle rotations of pieces?
  //Keyboard controls
  var shiftPressed: Boolean = false
  var showCoords: Boolean = false

  val flipDisplay: Boolean = ourSide == Some(S1) //Flip so that 0,0 is in the lower right

  //Mouse movement path state
  val mouseState = MouseState(ourSide,flipDisplay,this)

  //Websocket connection!
  val connection = Connection(username,password,ourSide)

  def sendWebsocketQuery(query: Protocol.Query): Unit = {
    connection.sendIfOpen(query)
  }

  //Timing mechanism
  private def getNow(): Double = {
    (new scala.scalajs.js.Date()).getTime() / 1000.0
  }
  var estimatedTurnEndTime: Option[Double] = None
  def updateEstimatedTurnEndTime(serverTimeLeft: Option[Double]) = {
    //Compute estimate from the most recent server time left message
    val newEstimatedEndTime = serverTimeLeft.map { serverTimeLeft => getNow() + serverTimeLeft }

    //Server updates can only push the end time earlier within a given turn.
    //That way, we always use the min-lag update to compute the time.
    estimatedTurnEndTime =
      newEstimatedEndTime match {
        case None => None
        case Some(newEstimatedEndTime) =>
          estimatedTurnEndTime match {
            case None => Some(newEstimatedEndTime)
            case Some(endTime) => Some(Math.min(endTime,newEstimatedEndTime))
          }
      }
  }

  //TODO if numActionsLocalAhead remains ahead of the server for too long (say, it never
  //decreases in a certain number of seconds), warn the user, since that may mean the server
  //never got the actions or the connection is bad.

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
          case Failure(err) => reportError(err.getLocalizedMessage)
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
    if(curBoardIdx == boardIdx) {
      mouseState.clear()
      draw()
    }
  }

  def handleResponse(response: Protocol.Response): Unit = {
    response match {
      case Protocol.Version(version) =>
        if(CurrentVersion.version != version)
          reportFatalError("Minions client version " + CurrentVersion.version + " does not match server version " + version)
        else
          println("Running minions version " + version)
      case Protocol.ClientHeartbeatRate(_) =>
        //Handled in connection.scala
        ()
      case Protocol.QueryError(err) =>
        reportError(err)
      case Protocol.OkHeartbeat(_) =>
        ()
      case Protocol.UserJoined(username,side) =>
        reportUserJoined(username,side)
      case Protocol.UserLeft(username,side) =>
        reportUserLeft(username,side)

      case Protocol.OkBoardAction(_,_) =>
        ()
      case Protocol.OkGameAction(_) =>
        ()

      case Protocol.Initialize(startGame,summaries,boardNames,boardSequences) =>
        println("Setting numBoards to " + summaries.length)
        numBoards = summaries.length
        curBoardIdx = 0
        game = Some(startGame)
        serverBoards = summaries.map { summary => Board.ofSummary(summary) }
        serverSequence = boardSequences.clone()
        serverActionSequence = Array.fill(summaries.length)(Vector())
        serverBoardNames = boardNames.clone()
        localBoards = serverBoards.map { board => board.copy() }
        localSequence = serverSequence.clone()
        localActionSequence = Array.fill(summaries.length)(Vector())
        numActionsLocalAhead = 0

      case Protocol.ReportGameAction(gameAction,_) =>
        println("Received game action " + gameAction)
        game.get.doAction(gameAction) match {
          case Failure(exn) => reportFatalError("Server sent illegal action: " + gameAction + " error: " + exn)
          case Success(()) =>
            draw()
        }
        gameAction match {
          case (_: PerformTech) | (_: UndoTech) | (_: SetBoardDone) | (_: PayForReinforcement) | (_: UnpayForReinforcement) => ()
          case ResignBoard(boardIdx) =>
            reportMessage("Team " + game.get.curSide.toColorName + " resigned board " + boardIdx + "!")
            game.get.winner.foreach { winner =>
              reportMessage("Team " + winner.toColorName + " won the game!")
            }

          case AddWin(side,boardIdx) =>
            reportMessage("Team " + side.toColorName + " won board " + boardIdx + "!")
        }

      case Protocol.ReportBoardAction(boardIdx,boardAction,newBoardSequence) =>
        println("Received board " + boardIdx + " action " + boardAction)
        serverBoards(boardIdx).doAction(boardAction) match {
          case Failure(exn) => reportFatalError("Server sent illegal action: " + boardAction + " error: " + exn)
          case Success(()) => ()
        }
        serverSequence(boardIdx) = newBoardSequence
        serverActionSequence(boardIdx) = serverActionSequence(boardIdx) :+ boardAction
        syncLocalAndServerBoards(boardIdx)

      case Protocol.ReportBoardHistory(boardIdx,boardSummary,newBoardSequence) =>
        serverBoards(boardIdx) = Board.ofSummary(boardSummary)
        serverSequence(boardIdx) = newBoardSequence
        serverActionSequence(boardIdx) = Vector()
        resetLocalBoards(boardIdx)

      case Protocol.ReportResetBoard(boardIdx,necroNames, canMove) =>
        serverBoards(boardIdx).resetBoard(necroNames, canMove)
        resetLocalBoards(boardIdx)

      case Protocol.ReportNewTurn(newSide) =>
        val mana = serverBoards.foldLeft(0) { case (sum,board) =>
          sum + board.curState.manaThisRound(newSide)
        }
        game.get.addMana(newSide,mana)
        game.get.endTurn()

        serverBoards.foreach { board => board.endTurn() }
        for(i <- 0 until numBoards)
          resetLocalBoards(i)

        mouseState.clear()

        if(game.get.curSide != newSide)
          throw new Exception("Server reported side is not the same as game side")
        serverBoards.foreach { board =>
          if(board.curState.side != newSide)
            throw new Exception("Server reported side is not the same as game side")
        }

        //At each new turn, clear the time left so that it can be refreshed by the next server update
        estimatedTurnEndTime = None

        game.get.winner match {
          case Some(winner) =>
            reportMessage("Team " + winner.toColorName + " won the game!")
          case None =>
            reportMessage("Beginning " + newSide.toColorName + " team turn (turn #" + game.get.turnNumber + ")")
            game.get.newTechsThisTurn.foreach { case (side,tech) =>
              reportMessage("Team " + side.toColorName + " acquired new tech: " + tech.displayName)
            }
        }

      case Protocol.ReportTimeLeft(timeLeft) =>
        updateEstimatedTurnEndTime(timeLeft)
    }
  }

  def handleWebsocketEvent(result: Try[Protocol.Response]): Unit = {
    result match {
      case Failure(exn) => reportError("Websocket exn: " + exn.getLocalizedMessage)
      case Success(response) => handleResponse(response)
    }
  }

  def makeActionId(): String = {
    nextActionIdSuffix = nextActionIdSuffix + 1
    username + nextActionIdSuffix.toString
  }

  def doActionOnCurBoard(action: BoardAction): Unit = {
    if(!gotFatalError) {
      //For general board actions, send it directly to the server for confirmation, don't do anything locally,
      //because the dependence on global state might make it illegal, and then when the server reports the
      //action we will make it during syncLocalAndServerBoards.
      action match {
        case (_: DoGeneralBoardAction) =>
          sendWebsocketQuery(Protocol.DoBoardAction(curBoardIdx,action))
        case (_: PlayerActions) | (_: LocalPieceUndo) | (_: SpellUndo) | (_: BuyReinforcementUndo) =>
          if(game.exists { game => game.winner.nonEmpty })
            reportError("Game is over")
          localBoards(curBoardIdx).doAction(action) match {
            case Failure(error) => reportError(error.getLocalizedMessage)
            case Success(()) =>
              localSequence(curBoardIdx) = localSequence(curBoardIdx) + 1
              localActionSequence(curBoardIdx) = localActionSequence(curBoardIdx) :+ action
              numActionsLocalAhead = numActionsLocalAhead + 1
              sendWebsocketQuery(Protocol.DoBoardAction(curBoardIdx,action))
          }
      }
    }
  }

  def doGameAction(action: GameAction): Unit = {
    if(!gotFatalError) {
      //Send it directly to the server
      sendWebsocketQuery(Protocol.DoGameAction(action))
    }
  }

  def curLocalBoard() : Option[Board] = {
    if(numBoards == 0) None
    else Some(localBoards(curBoardIdx))
  }

  def draw() : Unit = {
    withBoardForMouse { board =>
      mouseState.refresh(game.get,board.curState)
    }
    curLocalBoard().foreach { _ =>
      val timeLeft = estimatedTurnEndTime.map { estimatedTurnEndTime => estimatedTurnEndTime - getNow() }
      Drawing.drawEverything(canvas, ctx, game.get, localBoards, serverBoardNames, curBoardIdx, mouseState,
        flipDisplay, mouseState.undoing, showCoords, timeLeft)
    }
  }

  def mousePixel(e : MouseEvent) : PixelLoc = {
    val rect = canvas.getBoundingClientRect()
    PixelLoc(e.clientX - rect.left, e.clientY - rect.top) - UI.translateOrigin
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
      val undo = e.altKey || e.button==2;
      mouseState.handleMouseDown(pixelLoc,game.get,board.curState, undo)
    }
    draw()
  }
  def mouseup(e : MouseEvent) : Unit = {
    withBoardForMouse { board =>
      val pixelLoc = mousePixel(e)
      val undo = e.altKey || e.button==2; // alt or right-click
      mouseState.handleMouseUp(pixelLoc,game.get,board.curState,curBoardIdx, undo)
    }
    draw()
  }
  def mousemove(e : MouseEvent) : Unit = {
    withBoardForMouse { board =>
      val pixelLoc = mousePixel(e)
      mouseState.handleMouseMove(pixelLoc,game.get,board.curState)
    }
    draw()
  }
  def mouseout(e : MouseEvent) : Unit = {
    val _ = e
    mouseState.clear()
    draw()
  }

  def ignore(e : Any) : Unit = {
    val _ = e
    ()
  }

  //Prevents double click on canvas from selecting text
  def selectStart(e : Any) : Boolean = {
    ignore(e)
    false
  }

  def keydown(e : KeyboardEvent) : Unit = {
    //Page up
    if(e.keyCode == 33) {
      e.preventDefault()
      if(curBoardIdx > 0) {
        mouseState.clear()
        curBoardIdx -= 1
        draw()
      }
    }
    //Page up
    else if(e.keyCode == 34) {
      e.preventDefault()
      if(curBoardIdx < numBoards - 1) {
        mouseState.clear()
        curBoardIdx += 1
        draw()
      }
    }
    else if(e.keyCode == 16) {
      shiftPressed = true
      draw()
    }
    //'c'
    else if(e.keyCode == 67) {
      showCoords = !showCoords
      draw()
    }
  }
  def keyup(e : KeyboardEvent) : Unit = {
    if(e.keyCode == 16) {
      shiftPressed = false
      draw()
    }
  }

  def onBlur(e : Any) : Unit = {
    val _ = e
    shiftPressed = false
  }

  def showResignConfirm(): Unit = {
    if(game.exists { game => game.winner.isEmpty }) {
      if(game.get.wins(game.get.curSide.opp) >= game.get.targetNumWins - 1)
        jQuery("#resign-message").text("Really resign this board? (Last point, will cause the other team to win)")
      else
        jQuery("#resign-message").text("Really resign this board?")

      val (_: JQuery) = jQuery("#resign-confirm").removeClass("invisible")
    }
  }
  def hideResignConfirm(): Unit = {
    val (_: JQuery) = jQuery("#resign-confirm" ).addClass("invisible")
  }
  jQuery("#resign-cancel").click { (_: JQueryEventObject) =>
    hideResignConfirm()
  }
  jQuery("#resign-ok").click { (_: JQueryEventObject) =>
    hideResignConfirm()
    doGameAction(ResignBoard(curBoardIdx))
  }

  window.addEventListener("blur", onBlur)
  window.addEventListener("keydown", keydown)
  window.addEventListener("keyup", keyup)
  canvas.onmousedown = mousedown _
  canvas.onmousemove = mousemove _
  canvas.onmouseup = mouseup _
  canvas.onmouseout = mouseout _
  canvas.onselectstart = selectStart _

  draw()

  scala.scalajs.js.timers.setInterval(200) {
    draw()
  }

  val closed : Future[Unit] = connection.run(handleWebsocketEvent)
  closed.onComplete {
    case Success(()) => reportFatalError("Connection to server was closed")
    case Failure(err) => reportFatalError("Error connecting to server: " + err)
  }
  ()

}

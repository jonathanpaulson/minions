package minionsgame.jsclient

import scala.concurrent.Future
import scala.util.{Try, Success, Failure}

import scala.scalajs.js.{JSApp, Dictionary}
import org.scalajs.jquery.{JQuery,jQuery,JQueryEventObject}
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.{MouseEvent, KeyboardEvent}
import org.scalajs.dom.html.{Canvas, TextArea, Input, Button}
import org.scalajs.dom.raw.HTMLAudioElement
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

/*object MainPageMain extends JSApp {
  def main(): Unit = {
    jQuery { () => new MainPageClient().init() }
    ()
  }
}

class MainPageClient() {
  val username = {
    val params = (new java.net.URI(window.location.href)).getQuery()
    val fields = params.split("&").flatMap { piece =>
      piece.split("=").toList match {
        case Nil => None
        case _ :: Nil => None
        case k :: v :: Nil => Some((k,v))
        case _ :: _ :: _ :: _ => None
      }
    }.toMap
    fields("username")
  }

  def init(): Unit = {
    ()
  }

  var messages: List[String] = Nil
  var numMessagesSeen: Int = 0
  var unread = false
}*/

class Client() {
  val (gameid: String, username: String, password: Option[String], ourSide: Option[Side], beeps: Array[Int]) = {
    val params = (new java.net.URI(window.location.href)).getQuery()
    val fields = params.split("&").flatMap { piece =>
      piece.split("=").toList match {
        case Nil => None
        case _ :: Nil => None
        case k :: v :: Nil => Some((k,v))
        case _ :: _ :: _ :: _ => None
      }
    }.toMap
    val gameid = fields("game")
    val username = fields("username")
    val password = fields.get("password")
    val ourSide = fields.get("side").map(Side.ofString)
    val beeps = fields.getOrElse("beeps", "60,30,15,5,4,3,2,1").split(",").map(_.toInt).sorted.reverse
    (gameid, username,password,ourSide, beeps)
  }
  def init(): Unit = {
    ()
  }

  var gotFatalError: Boolean = false

  // Whether we are viewing the all chat or the team chat
  var allChat = true
  var numMessagesSeen: Int = 0
  var allMessages: List[String] = Nil
  var teamMessages: List[String] = Nil
  var allUnread = false
  var teamUnread = false

  def setAllUnread(b: Boolean): Unit = {
    allUnread = b
    if(allUnread)
      jQuery("#global-chat-button").addClass("unread")
    else
      jQuery("#global-chat-button").removeClass("unread")
    ()
  }
  def setTeamUnread(b: Boolean): Unit = {
    teamUnread = b
    if(teamUnread)
      jQuery("#team-chat-button").addClass("unread")
    else
      jQuery("#team-chat-button").removeClass("unread")
    ()
  }

  def scrollMessages(): Unit = {
    messages.scrollTop = messages.scrollHeight.toDouble
    if(allChat) setAllUnread(false)
    else setTeamUnread(false)
  }

  def scrollMessagesIfAtEnd(): Unit = {
    if(messages.scrollHeight - messages.scrollTop <= messages.clientHeight + 50) {
      scrollMessages()
    }
  }

  def reportMessage(msg: String): Unit = {
    messages.value += msg + "\n"
    scrollMessagesIfAtEnd()
  }
  def reportError(err: String): Unit = {
    reportMessage("ERROR: " + err)
    scrollMessages()
  }
  def reportFatalError(err: String): Unit = {
    reportMessage("FATAL ERROR: " + err)
    scrollMessages()
    jQuery("#board").addClass("errorborder")
    gotFatalError = true
  }
  def updateChat(): Unit = {
    val visibleMessages = if(allChat) allMessages else teamMessages
    if(numMessagesSeen > visibleMessages.length)
      resetChat()
    else {
      for(i <- numMessagesSeen until visibleMessages.length) {
        reportMessage(visibleMessages(i))
      }
      numMessagesSeen = visibleMessages.length
      scrollMessagesIfAtEnd()
    }
  }
  def resetChat(): Unit = {
    messages.value = ""
    val visibleMessages = if(allChat) allMessages else teamMessages
    visibleMessages.foreach { s => reportMessage(s) }
    numMessagesSeen = visibleMessages.length
    scrollMessages()
  }

  def setAllChat(b: Boolean): Unit = {
    allChat = b
    if(allChat) {
      jQuery("#global-chat-button").addClass("active")
      jQuery("#team-chat-button").removeClass("active")
    }
    else {
      jQuery("#global-chat-button").removeClass("active")
      jQuery("#team-chat-button").addClass("active")
    }
    resetChat()
  }

  def resetPlayers(players: SideArray[List[String]], spectators: List[String]): Unit = {
    val blueMessage = "BLUE TEAM: " + players(S0).mkString(",")
    val redMessage = "RED TEAM: " + players(S1).mkString(",")
    val spectatorMessage = "SPECTATORS: " + spectators.mkString(",")

    playersBox.value = ""
    playersBox.value += blueMessage + "\n"
    playersBox.value += redMessage + "\n"
    if(!spectators.isEmpty) {
      playersBox.value += spectatorMessage + "\n"
    }
  }

  val canvas = jQuery("#board").get(0).asInstanceOf[Canvas]
  val ctx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
  val messages = jQuery("#messages").get(0).asInstanceOf[TextArea]
  val playersBox = jQuery("#players").get(0).asInstanceOf[TextArea]
  val chat = jQuery("#chat-input").get(0).asInstanceOf[Input]

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
  var externalInfo: ExternalInfo = ExternalInfo.create()

  var numBoards: Int = 0 //Length of the boards arrays
  var curBoardIdx: Int = 0 //Currently selected board

  var nextActionIdSuffix: Int = 0 //For generating unique action ids

  //TODO Ctrl-click should perform BuyReinforcementUndo and LocalPieceUndo?
  //TODO Shift-click should allow performing swaps and triangle rotations of pieces?
  //Keyboard controls
  var showCoords: Boolean = false

  val flipDisplay: Boolean = ourSide == Some(S1) //Flip so that 0,0 is in the lower right

  //UI layout
  var ui: Option[UI] = None
  //Mouse movement path state
  var mouseState: Option[MouseState] = None

  //Websocket connection!
  val connection = Connection(gameid, username,password,ourSide)

  def sendWebsocketQuery(query: Protocol.Query): Unit = {
    connection.sendIfOpen(query)
  }

  //Timing mechanism
  private def getNow(): Double = {
    (new scala.scalajs.js.Date()).getTime() / 1000.0
  }
  var isPaused: Boolean = true
  var turnTimeLeft: Option[Double] = None
  var now: Double = getNow()

  var currentBeep = 0
  val beep = org.scalajs.dom.document.createElement("audio").asInstanceOf[HTMLAudioElement]
  beep.src = "img/beep.wav"
  def updateTimeLeft(): Unit = {
    val newNow = getNow()
    if(!isPaused) {
      turnTimeLeft match {
        case None => ()
        case Some(left) =>
          if(Some(game.get.curSide) == ourSide && currentBeep < beeps.size && left <= beeps(currentBeep)) {
            beep.play()
            currentBeep += 1
          }
          turnTimeLeft = Some(left - (newNow - now))
      }
    }
    now = newNow
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
          case (a: BoardAction) => localBoards(boardIdx).doAction(a,externalInfo)
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
      mouseState.get.clear()
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
      case Protocol.UserJoined(_, _) =>
        ()
      case Protocol.UserLeft(_, _) =>
        ()
      case Protocol.OkBoardAction(_,_) =>
        ()
      case Protocol.OkGameAction(_) =>
        ()

      case Protocol.Initialize(startGame,summaries,boardNames,boardSequences) =>
        println("Setting numBoards to " + summaries.length)
        numBoards = summaries.length
        curBoardIdx = 0
        game = Some(startGame)
        serverBoards = summaries.map { summary => Board.ofSummary(summary,externalInfo) }
        serverSequence = boardSequences.clone()
        serverActionSequence = Array.fill(summaries.length)(Vector())
        serverBoardNames = boardNames.clone()
        localBoards = serverBoards.map { board => board.copy() }
        localSequence = serverSequence.clone()
        localActionSequence = Array.fill(summaries.length)(Vector())
        numActionsLocalAhead = 0

        ui = Some(UI(flipDisplay,ourSide,serverBoards(0).initialState.tiles.xSize,serverBoards(0).initialState.tiles.ySize))
        mouseState = Some(MouseState(ourSide,ui.get,this))

      case Protocol.ReportGameAction(gameAction,_) =>
        println("Received game action " + gameAction)
        game.get.doAction(gameAction) match {
          case Failure(exn) => reportFatalError("Server sent illegal action: " + gameAction + " error: " + exn)
          case Success(()) =>
            draw()
        }

      case Protocol.ReportBoardAction(boardIdx,boardAction,newBoardSequence) =>
        println("Received board " + boardIdx + " action " + boardAction)
        serverBoards(boardIdx).doAction(boardAction,externalInfo) match {
          case Failure(exn) => reportFatalError("Server sent illegal action: " + boardAction + " error: " + exn)
          case Success(()) => ()
        }
        serverSequence(boardIdx) = newBoardSequence
        serverActionSequence(boardIdx) = serverActionSequence(boardIdx) :+ boardAction
        syncLocalAndServerBoards(boardIdx)

      case Protocol.ReportBoardHistory(boardIdx,boardSummary,newBoardSequence) =>
        serverBoards(boardIdx) = Board.ofSummary(boardSummary,externalInfo)
        serverSequence(boardIdx) = newBoardSequence
        serverActionSequence(boardIdx) = Vector()
        resetLocalBoards(boardIdx)

      case Protocol.ReportResetBoard(boardIdx,necroNames, canMove, reinforcements) =>
        serverBoards(boardIdx).resetBoard(necroNames, canMove, reinforcements)
        resetLocalBoards(boardIdx)

      case Protocol.ReportNewTurn(newSide) =>
        val souls = serverBoards.foldLeft(game.get.extraSoulsPerTurn(newSide)) { case (sum,board) =>
          sum + board.curState.soulsThisRound(newSide)
        }
        game.get.addSouls(newSide,souls)
        game.get.endTurn()

        serverBoards.foreach { board => board.endTurn() }
        for(i <- 0 until numBoards)
          resetLocalBoards(i)

        mouseState.get.clear()

        if(game.get.curSide != newSide)
          throw new Exception("Server reported side is not the same as game side")
        serverBoards.foreach { board =>
          if(board.curState.side != newSide)
            throw new Exception("Server reported side is not the same as game side")
        }

        //At each new turn, clear the time left so that it can be refreshed by the next server update
        turnTimeLeft = None
        currentBeep = 0

      case Protocol.ReportRevealSpells(spellIdsAndNames) =>
        externalInfo.revealSpells(spellIdsAndNames)

      case Protocol.ReportTimeLeft(serverTurnTimeLeft) =>
        turnTimeLeft = Some(serverTurnTimeLeft)
        updateTimeLeft()

      case Protocol.ReportPause(newIsPaused) =>
        updateTimeLeft()
        isPaused = newIsPaused

      case Protocol.Players(players,spectators) =>
        resetPlayers(players,spectators)

      case Protocol.Messages(all, team) =>
        if(all.length != allMessages.length)
          setAllUnread(true)
        if(team.length != teamMessages.length)
          setTeamUnread(true)
        allMessages = all
        teamMessages = team
        updateChat()
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
        case (_: PlayerActions) | (_: LocalPieceUndo) | (_: SpellUndo) | (_: BuyReinforcementUndo) | (_: GainSpellUndo) | (_: Redo) =>
          if(game.exists { game => game.winner.nonEmpty }) {
            reportError("Game is over")
          } else {
            localBoards(curBoardIdx).doAction(action,externalInfo) match {
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
    ui.foreach { ui =>
      withBoardForMouse { case (mouseState, board) =>
        mouseState.refresh(game.get,board.curState)
        updateTimeLeft()

        if(canvas.width != jQuery("#main").get(0).clientWidth)
          canvas.width = jQuery("#main").get(0).clientWidth
        if(canvas.height != jQuery("#main").get(0).clientHeight)
          canvas.height = jQuery("#main").get(0).clientHeight
        Drawing.drawEverything(canvas, ctx, game.get, externalInfo, localBoards, serverBoardNames, curBoardIdx, ui, mouseState,
          mouseState.undoing, turnTimeLeft, this)
      }
    }
  }

  def mousePixel(e : MouseEvent) : PixelLoc = {
    val rect = canvas.getBoundingClientRect()
    PixelLoc(e.clientX - rect.left, e.clientY - rect.top) - UI.translateOrigin
  }

  def withBoardForMouse(f: (MouseState,Board) => Unit): Unit = {
    mouseState.foreach { mouseState =>
      curLocalBoard() match {
        case None => mouseState.clear()
        case Some(board) => f(mouseState,board)
      }
    }
  }

  def mousedown(e: MouseEvent) : Unit = {
    withBoardForMouse { case (mouseState, board) =>
      val pixelLoc = mousePixel(e)
      val undo = e.button==2; // Right click
      mouseState.handleMouseDown(pixelLoc,game.get,board.curState, undo)
    }
    draw()
  }
  def mouseup(e : MouseEvent) : Unit = {
    withBoardForMouse { case (mouseState, board) =>
      val pixelLoc = mousePixel(e)
      val undo = e.button==2; // Right click
      mouseState.handleMouseUp(pixelLoc,game.get,board.curState,curBoardIdx, undo)
    }
    draw()
  }
  def mousemove(e : MouseEvent) : Unit = {
    withBoardForMouse { case (mouseState, board) =>
      val pixelLoc = mousePixel(e)
      mouseState.handleMouseMove(pixelLoc,game.get,board.curState)
    }
    draw()
  }
  def mouseout(e : MouseEvent) : Unit = {
    val _ = e
    withBoardForMouse { case (mouseState, _) =>
      mouseState.clear()
    }
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
    val _ = jQuery("#chat-input").focus()

    // Enter
    if(e.keyCode == 13) {
      if(chat.value != "") {
        sendWebsocketQuery(Protocol.Chat(username, ourSide, allChat, chat.value))
        chat.value = ""
      }
    }
    //Page up
    if(e.keyCode == 33) {
      e.preventDefault()
      if(curBoardIdx > 0) {
        mouseState.foreach(_.clear())
        curBoardIdx -= 1
        draw()
      }
    }
    //Page up
    else if(e.keyCode == 34) {
      e.preventDefault()
      if(curBoardIdx < numBoards - 1) {
        mouseState.foreach(_.clear())
        curBoardIdx += 1
        draw()
      }
    }
    //'c'
    else if(e.keyCode == 67 && e.altKey) {
      toggleCoords()
    }
    //tab
    else if(e.keyCode == 9) {
      e.preventDefault()
      setAllChat(!allChat)
      draw()
    }
  }

  def keyup(e : KeyboardEvent) : Unit = {
    val _ = e
  }

  def onBlur(e : Any) : Unit = {
    val _ = e
  }

  def pause(): Unit = {
    if(!gotFatalError) {
      //Send it directly to the server
      sendWebsocketQuery(Protocol.RequestPause(!isPaused))
    }
  }
  def toggleCoords(): Unit = {
    showCoords = !showCoords
    draw()
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

  jQuery("#global-chat-button").click { (_: JQueryEventObject) =>
    setAllChat(true)
  }
  jQuery("#team-chat-button").click { (_: JQueryEventObject) =>
    setAllChat(false)
  }

  jQuery("#messages").scroll { (_: JQueryEventObject) =>
    if(messages.scrollHeight - messages.scrollTop <= messages.clientHeight + 20) {
      if(allChat)
        setAllUnread(false)
      else
        setTeamUnread(false)
    }
  }

  window.addEventListener("blur", onBlur)
  window.addEventListener("keydown", keydown)
  window.addEventListener("keyup", keyup)
  canvas.onmousedown = mousedown _
  canvas.onmousemove = mousemove _
  canvas.onmouseup = mouseup _
  canvas.onmouseout = mouseout _
  canvas.onselectstart = selectStart _

  setAllChat(allChat)
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

package minionsgame.server

import scala.util.{Try,Success,Failure}
import scala.concurrent.Future
import java.util.concurrent.atomic.AtomicInteger
import com.typesafe.config.ConfigFactory
import java.util.Calendar
import java.text.SimpleDateFormat
import java.security.SecureRandom
import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets
import scala.collection.JavaConverters._

import akka.actor.{ActorSystem, Actor, ActorRef, Cancellable, Terminated, Props, Status}
import akka.stream.{ActorMaterializer,OverflowStrategy}
import akka.stream.scaladsl.{Flow,Sink,Source}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{ContentTypes,HttpEntity,StatusCodes}
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.ws.{Message,TextMessage}
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.Directives._
import akka.event.Logging

import minionsgame.core._
import RichImplicits._

import akka.http.scaladsl.Http
import play.api.libs.json._
import akka.stream.scaladsl.Keep

object AppPaths {
  val applicationConf = "./application.conf"
  val mainPage = "./web/index.html"
  val favicon = "./web/img/favicon.jpg"
  val webjs = "./web/js/"
  val webimg = "./web/img/"
}

object ServerMain extends App {
  //----------------------------------------------------------------------------------
  //LOAD STUFF

  val config = ConfigFactory.parseFile(new java.io.File(AppPaths.applicationConf))

  implicit val actorSystem = ActorSystem("gameSystem",config)
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = actorSystem.dispatcher

  val cwd = new java.io.File(".").getCanonicalPath
  Log.log("Running in " + cwd)

  val interface = config.getString("app.interface")
  val port = config.getInt("app.port")
  val rundir = config.getString("app.rundir")

  //GAME ACTOR - singleton actor that maintains the state of the game being played

  sealed trait GameActorEvent
  case class UserJoined(val sessionId: Int, val username: String, val side: Option[Side], val out: ActorRef) extends GameActorEvent
  case class UserLeft(val sessionId: Int) extends GameActorEvent
  case class QueryStr(val sessionId: Int, val queryStr: String) extends GameActorEvent
  case class ShouldEndTurn(val turnNumberToEnd: Int) extends GameActorEvent
  case class ShouldReportTimeLeft() extends GameActorEvent
  case class StartGame() extends GameActorEvent
  case class ResetTime() extends GameActorEvent

  private class GameActor(state: GameState, gameid: String) extends Actor {
    //TIME LIMITS
    def getNow(): Double = {
      System.currentTimeMillis.toDouble / 1000.0
    }
    //Server reports time left every this often
    val reportTimePeriod = 5.0
    var now: Double = getNow()
    var endTurnJob: Option[Cancellable] = None
    var turnTimeLeft: Double = state.currentSideSecondsPerTurn()

    def updateTime(): Unit = {
      val newNow = getNow()
      if(!state.isPaused) {
        turnTimeLeft -= (newNow - now)
      }
      now = newNow

      val path = Paths.get(rundir, gameid)
      val state_str = Json.stringify(Json.toJson(state))
      val _ = Files.write(path, state_str.getBytes(StandardCharsets.UTF_8))
    }

    private def getTimeLeftEvent(): Option[Protocol.Response] = {
      updateTime()
      if(state.game.winner.isEmpty) {
        Some(Protocol.ReportTimeLeft(turnTimeLeft))
      }
      else {
        val (_: Boolean) = timeReportJob.cancel()
        None
      }
    }

    private def maybeBroadcastTimeLeft(): Unit = {
      getTimeLeftEvent().foreach { response =>
        state.broadcastAll(response)
      }
    }

    private def scheduleEndOfTurn(reason: ScheduleReason): Unit = {
      import scala.concurrent.duration._
      import scala.language.postfixOps
      endTurnJob match {
        case Some(job) =>
          job.cancel()
          ()
        case None => ()
      }
      updateTime()
      reason match {
        case NewTurn | NewLimits =>
          turnTimeLeft = state.currentSideSecondsPerTurn()
        case Pause(isPaused) =>
          state.isPaused = isPaused
          state.broadcastAll(Protocol.ReportPause(state.isPaused))
      }

      if(!state.isPaused) {
        val turnNumber = state.game.turnNumber
        endTurnJob = Some(actorSystem.scheduler.scheduleOnce(turnTimeLeft seconds) {
          //Make sure to do this via sending event to self, rather than directly, to
          //take advantage of the actor's synchronization
          self ! ShouldEndTurn(turnNumber)
        })
      } else {
        endTurnJob = None
      }
      maybeBroadcastTimeLeft()
    }

    val timeReportJob: Cancellable = {
      import scala.concurrent.duration._
      import scala.language.postfixOps
      actorSystem.scheduler.schedule(reportTimePeriod seconds, reportTimePeriod seconds) {
        self ! ShouldReportTimeLeft()
      }
    }

    override def receive: Receive = {
      case UserJoined(sessionId, username, side, out) =>
        state.handleUserJoined(sessionId, username, side, out)
        getTimeLeftEvent().foreach { response => out ! response }

      case UserLeft(sessionId) =>
        state.handleUserLeft(sessionId)

      case QueryStr(sessionId, queryStr) =>
        if(state.usernameOfSession.contains(sessionId)) {
          val out = state.userOuts(sessionId)
          import play.api.libs.json._
          Try(Json.parse(queryStr)) match {
            case Failure(err) => out ! Protocol.QueryError("Could not parse as json: " + err.getLocalizedMessage)
            case Success(json) =>
              json.validate[Protocol.Query] match {
                case (e: JsError) => out ! Protocol.QueryError("Could not parse as query: " + JsError.toJson(e).toString())
                case (s: JsSuccess[Protocol.Query]) =>
                  val query = s.get
                  state.handleQuery(query, out, sessionId, scheduleEndOfTurn)
              }
          }
        }
      case ShouldEndTurn(turnNumberToEnd) =>
        if(state.game.turnNumber == turnNumberToEnd && state.game.winner.isEmpty) {
          state.doEndOfTurn(scheduleEndOfTurn)
        }

      case ShouldReportTimeLeft() =>
        maybeBroadcastTimeLeft()

      case StartGame() =>
        state.refillUpcomingSpells()
        state.game.startGame()
        state.broadcastAll(Protocol.ReportNewTurn(S0))

      case ResetTime() =>
        scheduleEndOfTurn(NewLimits)

    }
  }

  var games: Map[String, (ActorRef, GameState)] = Map()
  var globalChat: List[String] = List()

  def chooseGameName(prefix: String): String = {
    var suffix = 0
    while(games.contains(prefix + suffix)) {
      suffix += 1
    }
    val ret = prefix + suffix
    assert(!games.contains(ret))
    return ret
  }

  //----------------------------------------------------------------------------------
  //WEBSOCKET MESSAGE HANDLING

  val nextSessionId = new AtomicInteger()

  def websocketMessageFlow(gameid: String, username: String, sideStr: Option[String]) : Flow[Message, Message, _] = {
    val side: Option[Side] = sideStr match {
      case Some("0") => Some(S0)
      case Some("1") => Some(S1)
      case Some(s) => throw new Exception("Invalid side: " + s)
      case None => None
    }
    val (gameActor,_) = games(gameid)
    val sessionId = nextSessionId.getAndIncrement()

    //Create output stream for the given user
    val responseBufferSize = 128 //Buffer messages to the user before failing

    //Specifies a sink where the values are made by a flow of Messages
    //and mapping them and then feeding them to the GameActor
    val in: Sink[Message,_] = {
      Flow[Message].collect { message: Message =>
        message match {
          case TextMessage.Strict(text) =>
            Future.successful(text)
          case TextMessage.Streamed(textStream) =>
            textStream.runFold("")(_ + _)
        }
      } .mapAsync(1)((str:Future[String]) => str)
        .map { (str: String) => QueryStr(sessionId,str): GameActorEvent }
        .to(Sink.actorRef[GameActorEvent](gameActor, onCompleteMessage = UserLeft(sessionId)))
    }

    //Specifies a source made by materializing an Actor, where the source's values are those that
    //are fed to the Actor, followed by a map that converts them to text messages
    val out: Source[Message,_] = {
      Source.actorRef[Protocol.Response](responseBufferSize, OverflowStrategy.fail)
        .mapMaterializedValue(actorRef => gameActor ! UserJoined(sessionId,username,side,actorRef))
        .map { response: Protocol.Response =>
          import play.api.libs.json._
          TextMessage(Json.stringify(Json.toJson(response))) : Message
        }
    }

    Flow.fromSinkAndSource(in, out)
  }

  def addAI(gameState: GameState, gameid: String, doTutorial: Boolean, aiNum: Int, sideNum: Int) = {
    val (actorRef, pub) = Source.actorRef[Protocol.Query](128, OverflowStrategy.fail).toMat(Sink.asPublisher(false))(Keep.both).run()
    val source = Source.fromPublisher(pub)
    var ai = actorSystem.actorOf(Props(classOf[AIActor], actorRef, gameState, doTutorial, sideNum))
    if (aiNum == 1) {
      ai = actorSystem.actorOf(Props(classOf[AIActor2], actorRef, gameState, doTutorial, sideNum))
    }
    var side = Side.ofString(sideNum.toString)

    val sink: Sink[Message,_] = {
      Flow[Message].collect { message: Message =>
        message match {
          case TextMessage.Strict(text) =>
            Future.successful(text)
          case TextMessage.Streamed(textStream) =>
            textStream.runFold("")(_ + _)
        }
      } .mapAsync(1)((str:Future[String]) => str)
        .map { (str: String) =>
          val json = Json.parse(str)
          json.validate[Protocol.Response] match {
            case (s: JsSuccess[Protocol.Response]) => s.get
            case (e: JsError) => Protocol.QueryError("Could not parse as query: " + JsError.toJson(e).toString())
          }
        }
          .to(Sink.actorRef[Protocol.Response](ai, onCompleteMessage = Protocol.UserLeft("igor", Some(side))))
    }


    val flow = websocketMessageFlow(gameid,"igor",Some(sideNum.toString))
    source.map { query => TextMessage(Json.stringify(Json.toJson(query))) }.via(flow).to(sink).run()
    gameState.broadcastAll(Protocol.ReportNewTurn(S0))
  }


  //----------------------------------------------------------------------------------
  //DEFINE WEB SERVER ROUTES

  val route = get {
    pathEndOrSingleSlash {
      val html = new StringBuilder
      html ++= s"""
<link rel="icon" href="/img/favicon.png?v=4" />
<style>
  body {
    font-family: "Inter var", sans-serif;
    margin: 0;
  }
  .button {
    background-color: #4caf50; /* Green */
    border: none;
    color: white;
    padding: 15px 32px;
    text-align: center;
    text-decoration: none;
    display: inline-block;
    font-size: 16px;
  }

  table {
    font-family: "Trebuchet MS", Arial, Helvetica, sans-serif;
    border-collapse: collapse;
    width: 100%;
  }

  td,
  th {
    border: 1px solid #ddd;
    padding: 8px;
  }

  tr:nth-child(even) {
    background-color: #f2f2f2;
  }

  tr:hover {
    background-color: #ddd;
  }

  th {
    padding-top: 12px;
    padding-bottom: 12px;
    text-align: left;
    background-color: #4caf50;
    color: white;
  }

  .container {
    max-width: 800px;
    margin: 0 auto;
    padding: 0 16px;
  }
  .title {
    text-align: center;
    font-weight: 800;
    margin-bottom: 0;
    padding-top: 3rem;
  }
  .subtitle {
    text-align: center;
    margin: 0;
    padding-top: 0.75rem;
  }
  .links {
    text-align: center;
    margin: 0;
    padding-top: 0.25rem;
  }
  .buttons {
    padding-top: 1.5rem;
    padding-bottom: 1.5rem;
    display: flex;
    justify-content: center;
    margin: -8px;
    flex-wrap: wrap;
  }
  .buttons > a {
    margin: 8px;
  }
</style>
<link
  rel="preload"
  href="https://rsms.me/inter/inter.css"
  as="style"
  onload="this.onload=null;this.rel='stylesheet'"
/>
      
<div class="container">
  <h1 class="title">Minions Game</h1>
  <p class="subtitle">
    Minions is a multiplayer team tactics hex grid open source game.
  </p>
  <p class="links">
    <a href="https://discord.gg/BsQVky">Discord</a> &middot;
    <a href="https://github.com/jonathanpaulson/minions/">Github</a>
  </p>

  <div class="buttons">
    <a href="/newGame" class="button">New Game</a>
    <a href="/ai?aiNum=0&difficulty=10" class="button">Vs Igor (1v1)</a>
    <a href="/ai?aiNum=1&difficulty=10" class="button">Vs Leslie (1v1)</a>
    <a href="/ai?aiNum=0&difficulty=10&side=0" class="button">Igor Vs You (1v1)</a>
    <a href="/ai?aiNum=0&difficulty=10&side=2" class="button">Leslie Vs Igor (1v1)</a>
    <a href="/ai?difficulty=0&tutorial=true" class="button">Tutorial</a>
  </div>
      """
      
      if(!games.isEmpty) {
        html ++= "<table border=1><tr><th>Game</th><th>Access</th><th>Boards</th><th>Blue Team</th><th>Red Team</th><th>Spectators</th></tr>"
        for((game, (_,state)) <- games) {
          val hasPassword = if(state.password.isEmpty) "Public" else "Password"
          val nBoards = state.numBoards
          def teamString(side : Option[Side]) : String = {
            var players = List[String]()
            for((sid, sid_side) <- state.userSides) {
              if(sid_side == side) {
                players = players :+ state.usernameOfSession(sid)
              }
            }
            return players.mkString(",")
          }
          val blue = teamString(Some(S0))
          val red = teamString(Some(S1))
          val spectators = teamString(None)

          html ++= s"""<tr>
  <td>$game <a href='/edit?game=$game'>Edit</a></td>
  <td>$hasPassword</td>
  <td>$nBoards</td>
  <td><a href='/play?game=$game&side=0'>Join</a> $blue</td>
  <td><a href='/play?game=$game&side=1'>Join</a> $red</td>
  <td><a href='/play?game=$game'>Spectate</a> $spectators</td>
  </tr>"""
        }
        html ++= "</table>"
      }
      html ++= "</div>"
      complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, html.toString))
    } ~
    path("show") {
      parameter("game".?) { gameid_opt =>
        gameid_opt match {
          case None => complete("Please provide 'game=' in URL")
          case Some(gameid) =>
            val (_,state) = games(gameid)
            val state_str = Json.stringify(Json.toJson(state))
            complete(state_str)
        }
      }
    } ~
    path("play") {
      respondWithHeaders(
        RawHeader("CacheControl", "no-cache, no-store, must-revalidate"),
        RawHeader("Expires", "0"),
        RawHeader("Pragma", "no-cache")
      ) {
      parameter("game".?) { gameid_opt =>
        parameter("username".?) { username =>
          parameter("password".?) { password =>
            gameid_opt match {
              case None => complete("Please provide 'game=' in URL")
              case Some(gameid) =>
                username match {
                  case None =>
                    val html = """
<link rel="icon" href="/img/favicon.png?v=4" />
<script type="text/javascript">
var username = window.prompt('Username?', '')
if(!username || username.length == 0) {
  window.history.back()
} else {
  window.location = window.location.href + '&username=' + username
}
</script>
"""
                    complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, html))
                  case Some(_) =>
                    games.get(gameid) match {
                      case None => complete(s"Game $gameid does not exist")
                      case Some((_,state)) =>
                        (password, state.password) match {
                          case (None, Some(_)) => complete(gameid + " is password-protected; please provide 'password=' in URL")
                          case (_, None) => getFromFile(AppPaths.mainPage)
                          case (Some(x), Some(y)) =>
                            if(x==y) getFromFile(AppPaths.mainPage)
                            else complete(s"Wrong password for $gameid")
                        }
                    }
                }
            }
          }
        }
      }
      }
    } ~
    pathPrefix("js") {
      getFromDirectory(AppPaths.webjs)
    } ~
    pathPrefix("img") {
      getFromDirectory(AppPaths.webimg)
    }
  } ~
  path("ai") {
    parameter("tutorial" ? false) { doTutorial =>
      parameter("difficulty" ? 10) { difficulty =>
        parameter("aiNum" ? 0) { aiNum =>
          parameter("side" ? 1) {side =>
            val secondsPerTurn = SideArray.create(10.0)
            val startingSouls = SideArray.createTwo(0, 6)
            var extraSoulsPerTurn = SideArray.createTwo(difficulty, difficulty) 
            if (side == 1) extraSoulsPerTurn = SideArray.createTwo(0, difficulty)
            else if (side == 0) extraSoulsPerTurn = SideArray.createTwo(difficulty, 0)
            val targetWins = 1
            val techSouls = 4
            val maps_opt = None
            val seed_opt = None

            val gameid = chooseGameName("ai")
            val gameState = GameState.createNormal(secondsPerTurn, startingSouls, extraSoulsPerTurn, targetWins, techSouls, maps_opt, seed_opt, None, false)
            val gameActor = actorSystem.actorOf(Props(classOf[GameActor], gameState, gameid))
            games = games + (gameid -> ((gameActor, gameState)))
            gameActor ! StartGame()

            if (side < 2) {
              addAI(gameState, gameid, doTutorial, aiNum, side)
              redirect(s"/play?game=$gameid&side=" + (1-side).toString, StatusCodes.SeeOther)
            } else {
              addAI(gameState, gameid, doTutorial, 0, 0)
              addAI(gameState, gameid, doTutorial, 1, 1)
              redirect(s"/play?game=$gameid&side=" + (0).toString, StatusCodes.SeeOther)
            }
          }
        }
      }
    }
  } ~
  path("playGame") {
    parameter("username") { username =>
      parameter("game") { gameid =>
        parameter("side".?) { side =>
          Try(websocketMessageFlow(gameid,username,side)) match {
            case Failure(exn) => complete(exn.getLocalizedMessage)
            case Success(flow) => handleWebSocketMessages(flow)
          }
        }
      }
    }
  } ~
  path("newGame") {
    get {
      val game = chooseGameName("game")
      val blueSecondsPerTurn = config.getDouble("app.s0SecondsPerTurn")
      val redSecondsPerTurn = config.getDouble("app.s1SecondsPerTurn")
      val targetNumWins = config.getInt("app.targetNumWins")
      val blueStartingSouls = config.getInt("app.s0StartingSoulsPerBoard")
      val redStartingSouls = config.getInt("app.s1StartingSoulsPerBoard")
      val blueSoulsPerTurn = config.getInt("app.s0ExtraSoulsPerTurn")
      val redSoulsPerTurn = config.getInt("app.s1ExtraSoulsPerTurn")
      val extraTechCost = config.getInt("app.extraTechCostPerBoard")

      val vacuum_html = {
        val attrs = List("Name", "Cost", "Rebate", "Attack", "Health", "Speed", "Range", "NumAttacks")
        val bools = List("Swarm", "Lumbering", "Spawn", "Persistent", "Flying", "Blink")
        val text_html =
          attrs.map { attr =>
            s"""<div class="field">
            <label>$attr</label>
            <input type="text" name="blue$attr" autocomplete="off">
            <input type="text" name="red$attr" autocomplete="off">
          </div>"""}
        val bool_html =
          bools.map { attr =>
            s"""<div class="field">
            <label>$attr</label>
            <label class="checkbox"><input type=checkbox name="blue$attr" value="true"><span>Blue</span></label>
            <label class="checkbox"><input type=checkbox name="red$attr" value="true"><span>Red</span></label>
          </div>"""}
          val ability_options = Abilities.abilityMap.keys.map { name =>
            s"""<option value="$name">$name</option>"""
          }.mkString("\n")
        val ability_html = List(s"""<div class="field">
          <label>Ability</label>
          <select name="blueAbility"><option value=""></option>$ability_options</select>
          <select name="redAbility"><option value=""></option>$ability_options</select>
        </div>""")
        (text_html ++ bool_html ++ ability_html).mkString("\n")
      }
      val map_html =
        (BoardMaps.basicMaps.toList ++ BoardMaps.advancedMaps.toList).map { case (mapName, _) =>
          s"""<label class="checkbox"><input type=checkbox name=map value="$mapName"></input><span>$mapName</span></label>"""
        }.mkString("\n")
      complete(HttpEntity(ContentTypes.`text/html(UTF-8)`,
        s"""
        <head>
          <link rel="icon" href="/img/favicon.png?v=4" />
          <style type="text/css">
            * {
              box-sizing: border-box;
            }

            
            body {
              font-family: "Inter var", sans-serif;
              margin: 0;
            }

            .container {
              padding: 3rem 1rem;
            }
            .title {
              text-align: center;
              font-weight: 800;
              margin-bottom: 0;
              padding-bottom: 1.5rem;
            }
            form {
              margin: 0 auto;
              width: 636px; /* 220px + 8px + 200px + 8px + 200px */
            }
            .field {
              display: flex;
              align-items: center;
            }
            .field > label, .field > input, .field > select {
              width: 200px;
            }
            .field > label:first-child {
              flex-shrink: 0;
              text-align: right;
              padding-right: 8px;
              width: 220px;
            }
            .field:not(:first-of-type) {
              margin-top: 0.5rem;
            }
            .field > label {
              color: rgb(55,65,81);
              font-size: 0.875rem;
            }
            .field > input, .field > select {
              background-color: #fff;
              border: 1px solid rgb(209,213,219);
              border-radius: .375rem;
              padding-top: .25rem;
              padding-right: .5rem;
              padding-bottom: .25rem;
              padding-left: .5rem;
              font-size: 0.875rem;
              color: inherit;
              font-family: inherit;
              line-height: 1.25rem;
              box-shadow: rgba(0, 0, 0, 0.05) 0px 1px 2px 0px;
            }
            .field > *+* {
              margin-left: 0.5rem;
            }

            .accordion {
              margin: 1.5rem 0;
            }
            .accordion h3 {
              display: flex;
              margin: 0;
              margin-bottom: 0.75rem;
            }
            .accordion h3::before {
              /* Thanks Bootstrap! */
              flex-shrink: 0;
              width: 1.25rem;
              height: 1.25rem;
              margin-right: 0.75rem;
              content: "";
              background-image: url("data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 16 16' fill='%23212529'%3e%3cpath fill-rule='evenodd' d='M1.646 4.646a.5.5 0 0 1 .708 0L8 10.293l5.646-5.647a.5.5 0 0 1 .708.708l-6 6a.5.5 0 0 1-.708 0l-6-6a.5.5 0 0 1 0-.708z'/%3e%3c/svg%3e");
              background-repeat: no-repeat;
              background-size: 1.25rem;
              transition: transform .2s ease-in-out;
            }
            .accordion h3.ui-accordion-header-active::before {
              background-image: url("data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 16 16' fill='%230c63e4'%3e%3cpath fill-rule='evenodd' d='M1.646 4.646a.5.5 0 0 1 .708 0L8 10.293l5.646-5.647a.5.5 0 0 1 .708.708l-6 6a.5.5 0 0 1-.708 0l-6-6a.5.5 0 0 1 0-.708z'/%3e%3c/svg%3e");
              transform: rotate(-180deg);
            }
            
            .checkbox {
              display: flex;
              padding: 0.25rem 0;
              align-items: center;
            }
            input[type="checkbox"] {
              -webkit-appearance: none;
              -moz-appearance: none;
              appearance: none;
              padding: 0;
              -webkit-print-color-adjust: exact;
              color-adjust: exact;
              display: inline-block;
              vertical-align: middle;
              background-origin: border-box;
              -webkit-user-select: none;
              -moz-user-select: none;
              -ms-user-select: none;
              user-select: none;
              flex-shrink: 0;
              height: 1rem;
              width: 1rem;
              color: #2563eb;
              background-color: #fff;
              border: 1px solid rgb(209,213,219);
              border-radius: .25rem;
              box-shadow: 0 1px 2px 0 rgba(0, 0, 0, 0.05);
            }
            input[type="checkbox"]:checked {
              border-color: transparent;
              background-color: currentColor;
              background-size: 100% 100%;
              background-position: center;
              background-repeat: no-repeat;
              background-image: url("data:image/svg+xml,%3csvg viewBox='0 0 16 16' fill='white' xmlns='http://www.w3.org/2000/svg'%3e%3cpath d='M12.207 4.793a1 1 0 010 1.414l-5 5a1 1 0 01-1.414 0l-2-2a1 1 0 011.414-1.414L6.5 9.086l4.293-4.293a1 1 0 011.414 0z'/%3e%3c/svg%3e");
            }
            .checkbox > span {
              margin-left: 0.5rem;
            }

            .button {
              background-color: #4caf50; /* Green */
              border: none;
              color: white;
              padding: 15px 32px;
              text-align: center;
              text-decoration: none;
              display: inline-block;
              font-size: 16px;
            }
          </style>
          <link
            rel="preload"
            href="https://rsms.me/inter/inter.css"
            as="style"
            onload="this.onload=null;this.rel='stylesheet'"
          />
          <script src="https://code.jquery.com/jquery-1.12.4.js"></script>
          <script src="https://code.jquery.com/ui/1.12.1/jquery-ui.js"></script>
          <script>
            $$(document).ready(function() {
              $$(".accordion").accordion({
                collapsible: true,
                active: false
              });
            });
          </script>
        </head>
        <body>
          <div class="container">
            <h1 class="title">Create a Game</h1>
            <form method=post>
              <div class="field">
                <label>Game Name</label>
                <input type="text" name="game" value="$game"></input>
              </div>
              <div class="field">
                <label>Password (optional)</label>
                <input type="text" name="password"></input>
              </div>
              <div class="field">
                <label>Seconds Per Turn (Blue / Red)</label>
                <input type="text" name=blueSeconds value="$blueSecondsPerTurn"></input>
                <input type="text" name=redSeconds value="$redSecondsPerTurn">
              </div>
              <div class="field">
                <label>Points to win</label>
                <input type="text" name=targetWins value="$targetNumWins"></input>
              </div>


              <div class="accordion">
                <h3>Maps</h3>
                <div>
                  $map_html
                </div>
              </div>

              <div class="accordion">
                <h3>Vacuum Test</h3>
                <div>
                  <table>$vacuum_html</table>
                </div>
              </div>

              <div class="accordion">
              <h3>Advanced Options</h3>
                <div>
                  <div class="field">
                    <label>Random Seed (optional)</label>
                    <input type="text" name="seed">
                  </div>
                  <div class="field">
                    <label>Starting Souls per Board</label>
                    <input type="text" name=blueSouls value=$blueStartingSouls>
                    <input type="text" name=redSouls value=$redStartingSouls>
                  </div>
                  <div class="field">
                    <label>Extra Souls per Turn</label>
                    <input type=text name=blueSoulsPerTurn value=$blueSoulsPerTurn>
                    <input type=text name=redSoulsPerTurn value=$redSoulsPerTurn>
                  </div>
                  <div class="field">
                    <label>Tech Cost per Board</label>
                    <input type=text name=techSouls value=$extraTechCost>
                  </div>
                </div>
              </div>

              <button class="button" type="submit">Start Game</button>
            </form>
          </div>
        </body>
        """
        ))
      } ~ post {
        formFields(('game, 'password, 'seed)) { (gameid, password, seed) =>
        formFields(('blueSeconds.as[Double], 'redSeconds.as[Double], 'targetWins.as[Int])) { (blueSeconds, redSeconds, targetWins) =>
        formFields(('blueSouls.as[Int], 'redSouls.as[Int], 'techSouls.as[Int], 'map.*)) { (blueSouls, redSouls, techSouls, maps) =>
        formFields(('blueSoulsPerTurn.as[Int], 'redSoulsPerTurn.as[Int])) { (blueSoulsPerTurn, redSoulsPerTurn) =>
        formFields(('blueName, 'redName, 'blueAttack, 'redAttack, 'blueHealth, 'redHealth, 'blueSpeed, 'redSpeed, 'blueRange, 'redRange, 'blueCost, 'redCost, 'blueRebate, 'redRebate, 'blueNumAttacks, 'redNumAttacks, 'blueAbility, 'redAbility)) { (blueName, redName, blueAttack, redAttack, blueHealth, redHealth, blueSpeed, redSpeed, blueRange, redRange, blueCost, redCost, blueRebate, redRebate, blueNumAttacks, redNumAttacks, blueAbility, redAbility) =>
        formFields(('blueSwarm.?, 'redSwarm.?, 'blueLumbering.?, 'redLumbering.?, 'blueSpawn.?, 'redSpawn.?, 'bluePersistent.?, 'redPersistent.?, 'blueFlying.?, 'redFlying.?, 'blueBlink.?, 'redBlink.?)) { (blueSwarm, redSwarm, blueLumbering, redLumbering, blueSpawn, redSpawn, bluePersistent, redPersistent, blueFlying, redFlying, blueBlink, redBlink) =>
          games.get(gameid) match {
            case Some(_) =>
              complete(s"""A game named "$gameid" already exists; pick a different name""")
            case None =>
              val seed_opt = if(seed=="") None else Some(seed.toLong)
              val maps_opt = if(maps.isEmpty) None else Some(maps.toList)
              val passwordOpt = if(password == "") None else Some(password)
              val startingSouls = SideArray.createTwo(blueSouls, redSouls)
              val secondsPerTurn = SideArray.createTwo(blueSeconds, redSeconds)
              val extraSoulsPerTurn = SideArray.createTwo(blueSoulsPerTurn, redSoulsPerTurn)

              val blueUnit : Option[PieceStats] = Units.fromForm(blueName, blueAttack, blueHealth, blueSpeed, blueRange, blueCost, blueRebate, blueNumAttacks, blueSwarm, blueLumbering, blueSpawn, bluePersistent, blueFlying, blueBlink, blueAbility)
              val redUnit : Option[PieceStats] = Units.fromForm(redName, redAttack, redHealth, redSpeed, redRange, redCost, redRebate, redNumAttacks, redSwarm, redLumbering, redSpawn, redPersistent, redFlying, redBlink, redAbility)
              val gameState =
                (blueUnit, redUnit) match {
                  case (Some(u1), Some(u2)) =>
                    GameState.createVacuum(secondsPerTurn, startingSouls, extraSoulsPerTurn, targetWins, techSouls, maps_opt, seed_opt, passwordOpt, u1, u2)
                  case (_,_) =>
                    GameState.createNormal(secondsPerTurn, startingSouls, extraSoulsPerTurn, targetWins, techSouls, maps_opt, seed_opt, passwordOpt, false)
                }
              val gameActor = actorSystem.actorOf(Props(classOf[GameActor], gameState, gameid))
              gameActor ! StartGame()
              games = games + (gameid -> ((gameActor, gameState)))
              println("Created game " + gameid)
              complete(HttpEntity(ContentTypes.`text/html(UTF-8)`,
                s"""
                <link rel="icon" href="/img/favicon.png?v=4" />
                <pre>
                  Created game $gameid

                  password=$password
                  seed=$seed_opt
                  blueSeconds=$blueSeconds
                  redSeconds=$redSeconds
                  targetWins=$targetWins
                  techSouls=$techSouls
                  maps=$maps_opt
                  seed=$seed_opt

                  blueSouls=$blueSouls
                  redSouls=$redSouls
                  blueSoulsPerTurn=$blueSoulsPerTurn
                  redSoulsPerTurn=$redSoulsPerTurn
                </pre>

                <a href="/play?game=$gameid&side=0">Join blue</a><br>
                <a href="/play?game=$gameid&side=1">Join red</a><br>
                <a href="/play?game=$gameid">Spectate</a><br>
                <a href="/">Back</a>
                """
                      ))
                }
                  }
                }
              }
            }
          }
        }
      }
  } ~
  path("edit") {
    get {
    parameter("game".?) { gameid_opt =>
      gameid_opt match {
        case None => complete("Please provide 'game=' in URL")
        case Some(gameid) =>
          games.get(gameid) match {
            case None => complete(s"Cannot edit nonexistent game $gameid")
            case Some((actor@_, game)) =>
              val blueSecondsPerTurn = game.secondsPerTurn(S0)
              val redSecondsPerTurn = game.secondsPerTurn(S1)
              complete(HttpEntity(ContentTypes.`text/html(UTF-8)`,
                s"""
                <head>
                  <link rel="icon" href="/img/favicon.png?v=4" />
                  <style type="text/css">
                    form  { display: table;      }
                    p     { display: table-row;  }
                    label { display: table-cell; }
                    input { display: table-cell; }
                  </style>
                </head>
                <body>
                  <form method=post>
                    <p><label>Game name </label><input type="text" name="gameid" value=$gameid readonly></input>
                    <p><label>Blue seconds per turn </label><input type="text" name=blueSeconds value=$blueSecondsPerTurn></input><br>
                    <p><label>Red seconds per turn </label><input type="text" name=redSeconds value=$redSecondsPerTurn></input><br>
                    <p><input type="submit" value="Edit Game"></input>
                  </form>
                </body>
                """
                ))
          }

      }
    }
  } ~ post {
      formFields(('gameid, 'blueSeconds.as[Double], 'redSeconds.as[Double])) { (gameid, blueSeconds, redSeconds) =>
        val (actor, game) = games(gameid)
        game.secondsPerTurn = SideArray.createTwo(blueSeconds, redSeconds)
        actor ! ResetTime()
        complete(HttpEntity(ContentTypes.`text/html(UTF-8)`,
          s"""
          <link rel="icon" href="/img/favicon.png?v=4" />
          <pre>
            Edited game $gameid
            blueSeconds=$blueSeconds
            redSeconds=$redSeconds
          </pre>

          <a href="/play?game=$gameid&side=0">Join blue</a><br>
          <a href="/play?game=$gameid&side=1">Join red</a><br>
          <a href="/play?game=$gameid">Spectate</a><br>
          <a href="/">Back</a>
          """
          ))
      }
    }
  }

  //----------------------------------------------------------------------------------
  //HERE WE GO!

  val binding = Http().bindAndHandle(route, interface, port)


  val javaStream = Files.walk(Paths.get("."))
  // javaStream: java.util.stream.Stream[java.nio.file.Path] = java.util.stream.ReferencePipeline$3@51b1d486
  // javaStream.toScala(LazyList)

  val old_games = Files.list(Paths.get(rundir)).iterator().asScala
  old_games.foreach { filename =>
    val oldGameStr = Files.readAllLines(filename).get(0)
    val oldGameid = filename.getFileName().toString
    val oldGameState = Json.parse(oldGameStr).as[GameState]
    val oldActor = actorSystem.actorOf(Props(classOf[GameActor], oldGameState, oldGameid))
    games = games + (oldGameid -> ((oldActor, oldGameState)))
    if(oldGameid.contains("ai")) {
      addAI(oldGameState, oldGameid, false, 0, 1)
    }
  }

  // Create test game
  val secondsPerTurn = SideArray.create(120.0)
  val startingSouls = SideArray.createTwo(0, 6)
  val extraSoulsPerTurn = SideArray.createTwo(0, 10)
  val targetWins = 2
  val techSouls = 4
  val maps_opt = Some(List("MegaPuddles"))
  val seed_opt = None

  val gameid = chooseGameName("ai_test")
  val gameState = GameState.createNormal(secondsPerTurn, startingSouls, extraSoulsPerTurn, targetWins, techSouls, maps_opt, seed_opt, None, true)
  val gameActor = actorSystem.actorOf(Props(classOf[GameActor], gameState, gameid))
  games = games + (gameid -> ((gameActor, gameState)))
  gameActor ! StartGame()

  val (actorRef, pub) = Source.actorRef[Protocol.Query](128, OverflowStrategy.fail).toMat(Sink.asPublisher(false))(Keep.both).run()
  val source = Source.fromPublisher(pub)
  val ai = actorSystem.actorOf(Props(classOf[AIActor], actorRef, gameState, false, 1))

  val sink: Sink[Message,_] = {
    Flow[Message].collect { message: Message =>
      message match {
        case TextMessage.Strict(text) =>
          Future.successful(text)
        case TextMessage.Streamed(textStream) =>
          textStream.runFold("")(_ + _)
      }
    } .mapAsync(1)((str:Future[String]) => str)
      .map { (str: String) =>
        val json = Json.parse(str)
        json.validate[Protocol.Response] match {
          case (s: JsSuccess[Protocol.Response]) => s.get
          case (e: JsError) => Protocol.QueryError("Could not parse as query: " + JsError.toJson(e).toString())
        }
      }
        .to(Sink.actorRef[Protocol.Response](ai, onCompleteMessage = Protocol.UserLeft("igor", Some(S1))))
  }


  val flow = websocketMessageFlow(gameid,"igor",Some("1"))
  source.map { query => TextMessage(Json.stringify(Json.toJson(query))) }.via(flow).to(sink).run()

  binding.onComplete {
    case Failure(e) =>
      Log.log(s"Server http binding failed ${e.getMessage}")
      actorSystem.terminate()
    case Success(binding) =>
      val localAddress = binding.localAddress
      Log.log(s"Server is listening on ${localAddress.getHostName}:${localAddress.getPort}")
      scala.io.StdIn.readLine()
      Log.log("Done")
      actorSystem.terminate()
  }
}

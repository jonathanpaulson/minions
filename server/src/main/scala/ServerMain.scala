package minionsgame.server

import scala.util.{Try,Success,Failure}
import scala.concurrent.Future
import java.util.concurrent.atomic.AtomicInteger
import com.typesafe.config.ConfigFactory
import java.util.Calendar
import java.text.SimpleDateFormat
import java.security.SecureRandom

import akka.actor.{ActorSystem, Actor, ActorRef, Cancellable, Terminated, Props, Status}
import akka.stream.{ActorMaterializer,OverflowStrategy}
import akka.stream.scaladsl.{Flow,Sink,Source}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{ContentTypes,HttpEntity,StatusCodes}
import akka.http.scaladsl.model.ws.{Message,TextMessage}
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.Directives._
import akka.event.Logging

import minionsgame.core._
import RichImplicits._

import akka.http.scaladsl.Http
import play.api.libs.json._
import akka.stream.scaladsl.Keep

object Paths {
  val applicationConf = "./application.conf"
  val mainPage = "./web/index.html"
  val webjs = "./web/js/"
  val webimg = "./web/img/"
}

object ServerMain extends App {
  //----------------------------------------------------------------------------------
  //LOAD STUFF

  val config = ConfigFactory.parseFile(new java.io.File(Paths.applicationConf))

  implicit val actorSystem = ActorSystem("gameSystem",config)
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = actorSystem.dispatcher

  val cwd = new java.io.File(".").getCanonicalPath
  Log.log("Running in " + cwd)

  val interface = config.getString("app.interface")
  val port = config.getInt("app.port")

  //GAME ACTOR - singleton actor that maintains the state of the game being played

  sealed trait GameActorEvent
  case class UserJoined(val sessionId: Int, val username: String, val side: Option[Side], val out: ActorRef) extends GameActorEvent
  case class UserLeft(val sessionId: Int) extends GameActorEvent
  case class QueryStr(val sessionId: Int, val queryStr: String) extends GameActorEvent
  case class ShouldEndTurn(val turnNumberToEnd: Int) extends GameActorEvent
  case class ShouldReportTimeLeft() extends GameActorEvent
  case class StartGame() extends GameActorEvent



  private class GameActor(state: GameState) extends Actor {
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
        case NewTurn =>
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
                  state.handleQuery(query, out, state.userSides(sessionId), scheduleEndOfTurn)
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
    }
  }

  var games: Map[String, (ActorRef, GameState)] = Map()
  var globalChat: List[String] = List()

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

  //----------------------------------------------------------------------------------
  //DEFINE WEB SERVER ROUTES

  val route = get {
    pathEndOrSingleSlash {
      val html = new StringBuilder
      html ++= """
<style>
.button {
    background-color: #4CAF50; /* Green */
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

td, th {
    border: 1px solid #ddd;
    padding: 8px;
}

tr:nth-child(even){background-color: #f2f2f2;}

tr:hover {background-color: #ddd;}

th {
    padding-top: 12px;
    padding-bottom: 12px;
    text-align: left;
    background-color: #4CAF50;
    color: white;
}
</style>
      """
      html ++= "<a href=\"/newGame\" class=\"button\">New Game</a><p>"
      html ++= "<a href=\"/ai?difficulty=10\" class=\"button\">Vs AI (1v1)</a><p>"
      html ++= "<a href=\"/ai?difficulty=0&tutorial=true\" class=\"button\">Tutorial</a><p>"
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
  <td>$game</td>
  <td>$hasPassword</td>
  <td>$nBoards</td>
  <td><a href='/play?game=$game&side=0'>Join</a> $blue</td>
  <td><a href='/play?game=$game&side=1'>Join</a> $red</td>
  <td><a href='/play?game=$game'>Spectate</a> $spectators</td>
  </tr>"""
        }
        html ++= "</table>"
      }
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
      parameter("game".?) { gameid_opt =>
        parameter("username".?) { username =>
          parameter("password".?) { password =>
            gameid_opt match {
              case None => complete("Please provide 'game=' in URL")
              case Some(gameid) =>
                username match {
                  case None =>
                    val html = """<script type="text/javascript">
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
                          case (_, None) => getFromFile(Paths.mainPage)
                          case (Some(x), Some(y)) =>
                            if(x==y) getFromFile(Paths.mainPage)
                            else complete(s"Wrong password for $gameid")
                        }
                    }
                }
            }
          }
        }
      }
    } ~
    pathPrefix("js") {
      getFromDirectory(Paths.webjs)
    } ~
    pathPrefix("img") {
      getFromDirectory(Paths.webimg)
    }
  } ~
  path("ai") {
    parameter("tutorial" ? false) { doTutorial =>
      parameter("difficulty" ? 10) { difficulty =>
        val secondsPerTurn = SideArray.create(120.0)
        val startingMana = SideArray.createTwo(0, 5)
        val extraManaPerTurn = SideArray.createTwo(0, difficulty)
        val targetWins = 1
        val techMana = 4
        val maps_opt = None
        val seed_opt = None

        val gameState = GameState.create(secondsPerTurn, startingMana, extraManaPerTurn, targetWins, techMana, maps_opt, seed_opt, None)
        val gameActor = actorSystem.actorOf(Props(classOf[GameActor], gameState))
        val gameid = "ai" + games.size.toString
        games = games + (gameid -> ((gameActor, gameState)))
        gameActor ! StartGame()

        val (actorRef, pub) = Source.actorRef[Protocol.Query](128, OverflowStrategy.fail).toMat(Sink.asPublisher(false))(Keep.both).run()
        val source = Source.fromPublisher(pub)
        val ai = actorSystem.actorOf(Props(classOf[AIActor], actorRef, gameState, doTutorial))

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

        redirect(s"/play?game=$gameid&side=0", StatusCodes.SeeOther)
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
      val game = "game" + (games.size.toString)
      val blueSecondsPerTurn = config.getDouble("app.s0SecondsPerTurn")
      val redSecondsPerTurn = config.getDouble("app.s1SecondsPerTurn")
      val targetNumWins = config.getInt("app.targetNumWins")
      val blueStartingMana = config.getInt("app.s0StartingManaPerBoard")
      val redStartingMana = config.getInt("app.s1StartingManaPerBoard")
      val blueManaPerTurn = config.getInt("app.s0ExtraManaPerTurn")
      val redManaPerTurn = config.getInt("app.s1ExtraManaPerTurn")
      val extraTechCost = config.getInt("app.extraTechCostPerBoard")

      val map_html =
        (BoardMaps.basicMaps.toList ++ BoardMaps.advancedMaps.toList).map { case (mapName, _) =>
          s"""<p><label>$mapName</label><input type=checkbox name=map value="$mapName"></input><br>"""
        }.mkString("\n")
      complete(HttpEntity(ContentTypes.`text/html(UTF-8)`,
        s"""
        <head>
          <style type="text/css">
            form  { display: table;      }
            p     { display: table-row;  }
            label { display: table-cell; }
            input { display: table-cell; }
          </style>
        </head>
        <body>
          <form method=post>
            <p><label>Game name </label><input type="text" name="game" value=$game></input>
            <p><label>Password (optional) </label><input type="text" name="password"></input>

            <p><label>Blue seconds per turn </label><input type="text" name=blueSeconds value=$blueSecondsPerTurn></input><br>
            <p><label>Red seconds per turn </label><input type="text" name=redSeconds value=$redSecondsPerTurn></input><br>

            <p><label>Points to win </label><input type="text" name=targetWins value=$targetNumWins></input><br>


            <p>&nbsp
            <p><h3>Maps (optional)</h3>
            $map_html

            <p>&nbsp
            <p><h3>Advanced Options</h3>
            <p><label>Random seed (optional) </label><input type="text" name="seed"></input><br>
            <p><label>Blue starting mana per board&nbsp&nbsp </label><input type="text" name=blueMana value=$blueStartingMana></input><br>
            <p><label>Red starting mana per board </label><input type="text" name=redMana value=$redStartingMana></input><br>
            <p><label>Blue extra mana per turn&nbsp&nbsp </label><input type="text" name=blueManaPerTurn value=$blueManaPerTurn></input><br>
            <p><label>Red extra mana per turn</label><input type="text" name=redManaPerTurn value=$redManaPerTurn></input><br>
            <p><label>Tech cost per board </label><input type="text" name=techMana value=$extraTechCost></input><br>

            <p><input type="submit" value="Start Game"></input>
          </form>
        </body>
        """
        ))
      } ~ post {
        formFields(('game, 'password, 'seed)) { (gameid, password, seed) =>
          formFields(('blueSeconds.as[Double], 'redSeconds.as[Double], 'targetWins.as[Int])) { (blueSeconds, redSeconds, targetWins) =>
            formFields(('blueMana.as[Int], 'redMana.as[Int], 'techMana.as[Int], 'map.*)) { (blueMana, redMana, techMana, maps) =>
              formFields(('blueManaPerTurn.as[Int], 'redManaPerTurn.as[Int])) { (blueManaPerTurn, redManaPerTurn) =>
                games.get(gameid) match {
                  case Some(_) =>
                    complete(s"""A game named "$gameid" already exists; pick a different name""")
                  case None =>
                    val seed_opt = if(seed=="") None else Some(seed.toLong)
                    val maps_opt = if(maps.isEmpty) None else Some(maps.toList)
                    val passwordOpt = if(password == "") None else Some(password)
                    val startingMana = SideArray.createTwo(blueMana, redMana)
                    val secondsPerTurn = SideArray.createTwo(blueSeconds, redSeconds)
                    val extraManaPerTurn = SideArray.createTwo(blueManaPerTurn, redManaPerTurn)
                    val gameState = GameState.create(secondsPerTurn, startingMana, extraManaPerTurn, targetWins, techMana, maps_opt, seed_opt, passwordOpt)
                    val gameActor = actorSystem.actorOf(Props(classOf[GameActor], gameState))
                    gameActor ! StartGame()
                    games = games + (gameid -> ((gameActor, gameState)))
                    println("Created game " + gameid)
                    complete(HttpEntity(ContentTypes.`text/html(UTF-8)`,
                      s"""
<pre>
Created game $gameid

password=$password
seed=$seed_opt
blueSeconds=$blueSeconds
redSeconds=$redSeconds
targetWins=$targetWins
techMana=$techMana
maps=$maps_opt
seed=$seed_opt

blueMana=$blueMana
redMana=$redMana
blueManaPerTurn=$blueManaPerTurn
redManaPerTurn=$redManaPerTurn
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

  //----------------------------------------------------------------------------------
  //HERE WE GO!

  val binding = Http().bindAndHandle(route, interface, port)

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

package minionsgame.server

import scala.util.{Try,Success,Failure}
import scala.concurrent.Future
import java.util.concurrent.atomic.AtomicInteger

import akka.actor.{ActorSystem, Actor, ActorRef, Terminated, Props, Status}
import akka.stream.{ActorMaterializer,OverflowStrategy}
import akka.stream.scaladsl.{Flow,Sink,Source}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.ws.{Message,TextMessage}
import akka.http.scaladsl.server.Directives._

import minionsgame.core._

object ServerMain extends App {
  implicit val actorSystem = ActorSystem()
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = actorSystem.dispatcher

  val config = actorSystem.settings.config
  val interface = config.getString("app.interface")
  val port = config.getInt("app.port")

  val numBoards = config.getInt("app.numBoards")

  val boards: Array[Board] = {
    val topology = HexTopology
    val boards = (0 until numBoards).toArray.map { _ =>
      val plane: Plane[Terrain] = Plane.create(12, 12, topology, Ground)
      plane(1,0) = Water

      val state = BoardState.create(plane)
      state.spawnPieceInitial(S0, Units.zombie.name, Loc(2,1))
      state.spawnPieceInitial(S0, Units.zombie.name, Loc(2,2))
      state.spawnPieceInitial(S0, Units.zombie.name, Loc(2,2))

      state.spawnPieceInitial(S0, Units.zombie.name, Loc(2,3))
      state.spawnPieceInitial(S0, Units.zombie.name, Loc(2,3))
      state.spawnPieceInitial(S0, Units.zombie.name, Loc(2,3))

      state.spawnPieceInitial(S1, Units.zombie.name, Loc(3,2))
      state.spawnPieceInitial(S1, Units.zombie.name, Loc(3,4))
      state.spawnPieceInitial(S1, Units.zombie.name, Loc(1,4))
      state.spawnPieceInitial(S1, Units.zombie.name, Loc(2,5))

      Board.create(state)
    }
    boards
  }
  val boardSequences: Array[Int] = (0 until numBoards).toArray.map { _ => 0}

  sealed trait GameActorEvent
  case class PlayerJoined(val sessionId: Int, val username: String, val side: Side, val out: ActorRef) extends GameActorEvent
  case class PlayerLeft(val sessionId: Int, val username: String, val side: Side) extends GameActorEvent
  case class QueryStr(val sessionId: Int, val username: String, val side: Side, val queryStr: String) extends GameActorEvent

  private class GameActor extends Actor {
    //The actor refs are basically the writer end of a pipe where we can stick messages to go out
    //to the players logged into the server
    var playerSessionIds: Map[String,Int] = Map()
    val players: SideArray[Map[String,ActorRef]] = SideArray.create(Map())

    private def broadcast(response: Protocol.Response, side: Side): Unit = {
      players(side).foreach { case (_,out) =>
        out ! response
      }
    }
    private def broadcastBoth(response: Protocol.Response): Unit = {
      broadcast(response,S0)
      broadcast(response,S1)
    }

    private def handleQuery(query: Protocol.Query, out: ActorRef): Unit = {
      query match {
        case Protocol.RequestGeneralState =>
          out ! Protocol.QueryError("Not implemented yet")

        case Protocol.RequestBoardHistory(boardIdx) =>
          if(boardIdx < 0 || boardIdx >= numBoards)
            out ! Protocol.QueryError("Invalid boardIdx")
          else {
            out ! Protocol.ReportBoardHistory(
              boardIdx,
              boards(boardIdx).toSummary(),
              boardSequences(boardIdx)
            )
          }

        case Protocol.DoBoardAction(boardIdx,boardAction,boardSequence) =>
          if(boardIdx < 0 || boardIdx >= numBoards)
            out ! Protocol.QueryError("Invalid boardIdx")
          else {
            if(boardSequence != boardSequences(boardIdx))
              out ! Protocol.QueryError("Client board out-of-sync with server board, try refreshing?")
            else {
              boards(boardIdx).doAction(boardAction) match {
                case Failure(e) =>
                  out ! Protocol.QueryError("Illegal action: " + e.toString)
                case Success(()) =>
                  boardSequences(boardIdx) += 1
                  out ! Protocol.OkBoardAction(boardIdx,boardSequences(boardIdx))
                  broadcastBoth(Protocol.ReportBoardAction(boardIdx,boardAction,boardSequences(boardIdx)))
              }
            }
          }
        case Protocol.UndoBoardAction(boardIdx,boardSequence) =>
          if(boardIdx < 0 || boardIdx >= numBoards)
            out ! Protocol.QueryError("Invalid boardIdx")
          else {
            if(boardSequence != boardSequences(boardIdx))
              out ! Protocol.QueryError("Client board out-of-sync with server board, try refreshing?")
            else {
              boards(boardIdx).undo() match {
                case Failure(e) =>
                  out ! Protocol.QueryError("Illegal undo: " + e.toString)
                case Success(()) =>
                  boardSequences(boardIdx) += 1
                  out ! Protocol.OkUndoBoardAction(boardIdx,boardSequences(boardIdx))
                  broadcastBoth(Protocol.ReportUndoBoardAction(boardIdx,boardSequences(boardIdx)))
              }
            }
          }
      }
    }

    override def receive: Receive = {
      case PlayerJoined(sessionId, username, side, out) =>
        if(playerSessionIds.contains(username)) {
          out ! Protocol.QueryError("Username already taken: " + username)
          out ! Status.Success("")
        }
        else {
          playerSessionIds = playerSessionIds + (username -> sessionId)
          players(side) = players(side) + (username -> out)
          out ! Protocol.Version(CurrentVersion.version)
          out ! Protocol.NumBoards(numBoards)
          broadcastBoth(Protocol.PlayerJoined(username,side))
          println("PlayerJoined: " + username)
        }
      case PlayerLeft(sessionId, username, side) =>
        if(playerSessionIds.contains(username) && playerSessionIds(username) == sessionId) {
          broadcastBoth(Protocol.PlayerLeft(username,side))
          val out = players(side)(username)
          playerSessionIds = playerSessionIds - username
          players(side) = players(side) - username
          out ! Status.Success("")
          println("PlayerLeft: " + username)
        }
      case QueryStr(sessionId, username, side, queryStr) =>
        if(playerSessionIds.contains(username) && playerSessionIds(username) == sessionId) {
          val out = players(side)(username)
          import play.api.libs.json._
          Try(Json.parse(queryStr)) match {
            case Failure(err) => out ! Protocol.QueryError("Could not parse as json: " + err.toString)
            case Success(json) =>
              json.validate[Protocol.Query] match {
                case (e: JsError) => out ! Protocol.QueryError("Could not parse as query: " + JsError.toJson(e).toString())
                case (s: JsSuccess[Protocol.Query]) =>
                  val query = s.get
                  handleQuery(query, out)
              }
          }
        }
    }

  }

  val gameActor = actorSystem.actorOf(Props(classOf[GameActor]))
  val nextSessionId = new AtomicInteger()

  def websocketMessageFlow(username: String, sideStr: String) : Flow[Message, Message, _] = {
    val side: Side = sideStr match {
      case "0" => S0
      case "1" => S1
      case _ => throw new Exception("Invalid side: " + sideStr)
    }

    val sessionId = nextSessionId.getAndIncrement()

    //Create output stream for the given player
    val responseBufferSize = 16 //Buffer messages to the player before failing

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
        .map { (str: String) => QueryStr(sessionId,username,side,str): GameActorEvent }
        .to(Sink.actorRef[GameActorEvent](gameActor, onCompleteMessage = PlayerLeft(sessionId,username,side)))
    }

    //Specifies a source made by materializing an Actor, where the source's values are those that
    //are fed to the Actor, followed by a map that converts them to text messages
    val out: Source[Message,_] = {
      Source.actorRef[Protocol.Response](responseBufferSize, OverflowStrategy.fail)
        .mapMaterializedValue(actorRef => gameActor ! PlayerJoined(sessionId,username,side,actorRef))
        .map { response: Protocol.Response =>
          import play.api.libs.json._
          TextMessage(Json.stringify(Json.toJson(response))) : Message
        }
    }

    Flow.fromSinkAndSource(in, out)
  }

  val route = get {
    pathEndOrSingleSlash {
      complete("Hello world")
    }
  } ~
  path("playGame") {
    parameter("username") { username =>
      parameter("side") { side =>
        Try(websocketMessageFlow(username,side)) match {
          case Failure(exn) => complete(exn.toString)
          case Success(flow) =>
            handleWebSocketMessages(flow)
        }
      }
    }
  }

  val binding = Http().bindAndHandle(route, interface, port)

  binding.onComplete {
    case Failure(e) =>
      println(s"Server http binding failed ${e.getMessage}")
      actorSystem.terminate()
    case Success(binding) =>
      val localAddress = binding.localAddress
      println(s"Server is listening on ${localAddress.getHostName}:${localAddress.getPort}")
      scala.io.StdIn.readLine()
      println("Done")
      actorSystem.terminate()
  }

}

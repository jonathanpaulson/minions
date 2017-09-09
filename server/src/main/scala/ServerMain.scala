package minionsgame.server

import scala.util.{Try,Success,Failure}
import scala.concurrent.Future

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
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
  val boardLocks: Array[Object] = (0 until numBoards).toArray.map { _ => new Object()}

  def handleQuery(query: Protocol.Query): Protocol.Response = {
    query match {
      case Protocol.RequestGeneralState =>
        Protocol.QueryError("Not implemented yet")

      case Protocol.RequestBoardHistory(boardIdx) =>
        if(boardIdx < 0 || boardIdx >= numBoards)
          Protocol.QueryError("Invalid boardIdx")
        else boardLocks(boardIdx).synchronized {
          Protocol.ReportBoardHistory(
            boardIdx,
            boards(boardIdx).toSummary(),
            boardSequences(boardIdx)
          )
        }

      case Protocol.DoBoardAction(boardIdx,boardAction,boardSequence) =>
        if(boardIdx < 0 || boardIdx >= numBoards)
          Protocol.QueryError("Invalid boardIdx")
        else boardLocks(boardIdx).synchronized {
          if(boardSequence != boardSequences(boardIdx))
            Protocol.QueryError("Client board out-of-sync with server board, try refreshing?")
          else {
            val _ = (boardAction,boardSequence)
            Protocol.QueryError("Not implemented yet")
          }
        }
      case Protocol.UndoBoardAction(boardIdx,boardSequence) =>
        if(boardIdx < 0 || boardIdx >= numBoards)
          Protocol.QueryError("Invalid boardIdx")
        else boardLocks(boardIdx).synchronized {
          if(boardSequence != boardSequences(boardIdx))
            Protocol.QueryError("Client board out-of-sync with server board, try refreshing?")
          else {
            val _ = (boardSequence)
            Protocol.QueryError("Not implemented yet")
          }
        }
    }
  }

  val gameService: Flow[Message, Message, _] = {
    import play.api.libs.json._
    Flow[Message].collect {
      case TextMessage.Strict(text)             => Future.successful(text)
      case TextMessage.Streamed(textStream)     => textStream.runFold("")(_ + _)
    } .mapAsync(1)(x => x)
      .map { txt =>
        val response: Protocol.Response =
          Try(Json.parse(txt)) match {
            case Failure(err) => Protocol.QueryError("Could not parse as json: " + err.toString)
            case Success(json) =>
              json.validate[Protocol.Query] match {
                case (e: JsError) => Protocol.QueryError("Could not parse as query: " + JsError.toJson(e).toString())
                case (s: JsSuccess[Protocol.Query]) =>
                  val query = s.get
                  handleQuery(query)
              }
          }
        TextMessage(Json.stringify(Json.toJson(response)))
      }
  }

  val route = get {
    pathEndOrSingleSlash {
      complete("Hello world")
    }
  } ~
  path("playGame") {
    parameter("userName") { userName =>
      parameter("side") { side =>
        handleWebSocketMessages(gameService)
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

package minionsgame.server

import scala.util.{Try,Success,Failure}

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Flow
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.ws.{Message,TextMessage}
import akka.http.scaladsl.server.Directives._

import minionsgame.core._

object ServerMain {
  implicit val actorSystem = ActorSystem()
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = actorSystem.dispatcher

  val config = actorSystem.settings.config
  val interface = config.getString("app.interface")
  val port = config.getInt("app.port")

  val numBoards = config.getInt("app.numBoards")

  val boards = {
    val topology = HexTopology
    val _ = Plane.create(12, 12, topology, Ground)

  }


  val echoService: Flow[Message, Message, _] = Flow[Message].map {
    case TextMessage.Strict(txt) => TextMessage("ECHO: " + txt)
    case _ => TextMessage("Message type unsupported")
  }

  val route = get {
    pathEndOrSingleSlash {
      complete("Hello world")
    }
  } ~
  path("playGame") {
    parameter("userName") { userName =>
      parameter("side") { side =>
        handleWebSocketMessages(echoService)
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

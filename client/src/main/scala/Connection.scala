package minionsgame.jsclient

import scala.util.{Try,Success,Failure}
import scala.concurrent.{Future,Promise}

import org.scalajs.dom.WebSocket
import org.scalajs.dom.Event
import org.scalajs.dom.ErrorEvent
import org.scalajs.dom.MessageEvent
import org.scalajs.dom.window

import scala.concurrent.ExecutionContext.Implicits.global

import play.api.libs.json._

import minionsgame.core._
import RichImplicits._

object Connection {
  def apply(
    username: String,
    side: Option[Side]
  ): Connection = {
    new Connection(username,side)
  }
}

class Connection private (
  username: String,
  side: Option[Side]
) {
  private var socketId: Int = 0
  private var openSocket: Option[WebSocket] = None

  private def encode(s: String) =
    scala.scalajs.js.URIUtils.encodeURIComponent(s)

  val uri = "ws://" +
    (new java.net.URI(window.location.href)).getAuthority() +
    "/playGame" +
    "?" +
    "username=" + encode(username) +
    (side match { case None => "" case Some(side) => "&side=" + side.int })

  def run(f:Try[Protocol.Response] => Unit): Future[Unit] = {
    val done: Promise[Unit] = Promise()

    socketId = socketId + 1
    val id = socketId

    val socket = new WebSocket(uri)

    var heartbeatIdx = 0
    val heartbeatLoop = scala.scalajs.js.timers.setInterval(5000) {
      val query: Protocol.Query = Protocol.Heartbeat(heartbeatIdx)
      socket.send(Json.stringify(Json.toJson(query)))
      heartbeatIdx += 1
    }

    socket.onopen = { (_: Event) => () }
    socket.onerror = { (event: ErrorEvent) => f(Failure(new Exception(event.message))) }
    socket.onmessage = { (event: MessageEvent) =>
      val message = Try(Json.fromJson[Protocol.Response](Json.parse(event.data.toString)))
      message match {
        case Failure(exn) => f(Failure(exn))
        case Success(e:JsError) => f(Failure(new Exception("Error parsing message: " + e)))
        case Success(s:JsSuccess[Protocol.Response]) => f(Success(s.get))
      }
    }
    socket.onclose = { (_: Event) =>
      if(socketId == id)
        openSocket = None
      scala.scalajs.js.timers.clearInterval(heartbeatLoop)
      done.success(())
    }

    openSocket = Some(socket)
    done.future
  }

  def sendIfOpen(query: Protocol.Query): Unit = {
    openSocket.foreach { socket =>
      socket.send(Json.stringify(Json.toJson(query)))
    }
  }
}

package minionsgame.jsclient

import scala.util.{Try,Success,Failure}
import scala.concurrent.{Future,Promise}

import org.scalajs.dom.WebSocket
import org.scalajs.dom.Event
import org.scalajs.dom.ErrorEvent
import org.scalajs.dom.MessageEvent
import org.scalajs.dom.window
import java.net.URLEncoder

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

  private def encode(s: String) = URLEncoder.encode(s, "UTF-8")
  val uri = "ws://" +
    (new java.net.URI(window.location.href)).getAuthority() +
    "/playGame" +
    "?" +
    "username=" + encode(username) +
    (side match { case None => "" case Some(side) => "&side=" + side.int })

  def run(f:Try[Protocol.Response] => Unit): Future[Unit] = {
    val done: Promise[Unit] = Promise()

    val socket = new WebSocket(uri)
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
    socket.onclose = { (_: Event) => done.success(()) }

    done.future
  }
}

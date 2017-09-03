package minionsgame.jsclient

import org.scalajs.dom.WebSocket
import org.scalajs.dom.Event
import org.scalajs.dom.ErrorEvent
import org.scalajs.dom.MessageEvent

import play.api.libs.json._

import minionsgame.core._
import RichImplicits._

object Connection {
  def apply(
    baseUri: String,
    gameHash: String,
    side: Side,
    username: String
  ): Connection = {
    new Connection(baseUri,gameHash,side,username)
  }
}

class Connection private (
  baseUri: String,
  gameHash: String,
  side: Side,
  username: String
) {

  //TODO proper url encoding
  val uri = baseUri + "?" + "gameHash=" + gameHash + "side=" + side.int + "username=" + username
  val socket = new WebSocket(uri)

  socket.onopen = { (event: Event) =>
  }

  socket.onerror = { (event: ErrorEvent) =>
  }

  socket.onmessage = { (event: MessageEvent) =>
    val message = Json.fromJson[Protocol.Message](Json.parse(event.data.toString))
    message match {
      case e:JsError => println("Error parsing message: " + e)
      case s:JsSuccess[Protocol.Message] =>
        s.get match {
          case Protocol.Version(version) =>
            println(version)
          case Protocol.NumBoards(numBoards) =>
            println(numBoards)
          case Protocol.ReportBoardAction(boardIdx,boardAction,boardSequence) =>
            println(boardIdx)
            println(boardSequence)
            println(boardAction)
          case Protocol.ReportBoardState(boardIdx,boardState,boardSequence) =>
            println(boardIdx)
            println(boardSequence)
            println(boardState)
        }
    }
  }

  socket.onclose = { (event: Event) =>
  }

}

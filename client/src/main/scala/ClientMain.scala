import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport
import org.scalajs.jquery.jQuery
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.MouseEvent
import org.scalajs.dom.html
import scala.util.Random
import scala.scalajs.js
import scala.util.{Try, Success, Failure}

/**
  * Point:
  * The location of a pixel in normal canvas coordinates.
  * As usual, (0,0) is the upper left corner, x is to the right, y is downward.
  *
  * Note: As these are doubles and capable of representing subpixel positions, these are actually locations of the
  * corners of pixels. For example, (14,52) is the upper left corner of a pixel, (14.5,52) is the midpoint of the top edge,
  * (14.5,52.5) is the center of the pixel, etc.
  */
case class Point(x: Double, y: Double){
  def +(p: Point) = Point(x + p.x, y + p.y)
  def /(d: Double) = Point(x / d, y / d)
}

/*
 Coordinate system for Loc (from CommonTypes.scala):
 x is to the right
 y is to down and to the right

  x  0   1   2   3   4
 y   / \ / \ / \ / \ / \
  0 |   |   |   |   |   |
     \ / \ / \ / \ / \ / \
    1 |   |   |   |   |   |
       \ / \ / \ / \ / \ / \
      2 |   |   |   |   |   |
         \ / \ / \ / \ / \ / \
        3 |   |   |   |   |   |
           \ / \ / \ / \ / \ / \
          4 |   |   |   |   |   |
             \ / \ / \ / \ / \ /

 This is consistent with HexTopology in CommonTypes.scala having (x-1,y-1) and (x+1,y+1) not be adjacent to (x,y),
 but all other (x + {-1,0,1}, y + {-1,0,1}) be adjacent.
*/

//TODO dwu: At some point we probably want to split things up into multiple files. Maybe a reasonable separation would be
//to factor out the drawing logic from the user-input-handling.

object ClientMain extends JSApp {
  val size = 30.0

  def move(ctx : CanvasRenderingContext2D, point : Point) : Unit = {
    ctx.moveTo(Math.floor(point.x), Math.floor(point.y));
  }
  def line(ctx : CanvasRenderingContext2D, point : Point) : Unit = {
    ctx.lineTo(Math.floor(point.x), Math.floor(point.y));
  }
  def text(ctx : CanvasRenderingContext2D, text : String, point : Point, color : String) : Unit = {
    ctx.globalAlpha = 1.0
    ctx.fillStyle = color
    ctx.textAlign = "center"
    ctx.fillText(text, Math.floor(point.x), Math.floor(point.y))
  }

  //TODO from dwu to jpaulson: Camel-case this and other functions and variable names, to match the usual Scala style conventions?

  //Returns the Loc for the hex that contains a given Point.
  def hexRound(pos : Point) : Loc = {
    val x = pos.x
    val z = pos.y
    val y = -(x+z)
    val rx = Math.round(x)
    val ry = Math.round(y)
    val rz = Math.round(z)
    val dx = Math.abs(x - rx)
    val dy = Math.abs(y - ry)
    val dz = Math.abs(z - rz)
    if(dx >= dy && dx >= dz) {
      Loc(-(ry+rz).toInt, rz.toInt)
    } else if(dy >= dx && dy >= dz) {
      Loc(rx.toInt, rz.toInt)
    } else {
      // assert (dz >= dx && dz>=dy);
      Loc(rx.toInt, -(rx+ry).toInt)
    }
  }

  def hexCenter(pos : Loc, origin : Point) : Point = {
    Point(
      origin.x + size * Math.sqrt(3) * (pos.x.toDouble + pos.y.toDouble/2.0),
      origin.y + size * 3.0/2.0 * pos.y.toDouble
    )
  }

  def hexCorner(center : Point, size : Double, corner : Int) : Point = {
    val angleDeg = 60*corner + 30
    val angleRad = Math.PI/180 * angleDeg
    Point(center.x+size*Math.cos(angleRad), center.y+size*Math.sin(angleRad))
  }

  def drawHex(ctx : CanvasRenderingContext2D, center : Point, color : String, size : Double) : Unit = {
    ctx.globalAlpha = 0.2
    ctx.fillStyle = color
    ctx.beginPath()
    move(ctx, hexCorner(center, size, 0))
    line(ctx, hexCorner(center, size, 1))
    line(ctx, hexCorner(center, size, 2))
    line(ctx, hexCorner(center, size, 3))
    line(ctx, hexCorner(center, size, 4))
    line(ctx, hexCorner(center, size, 5))
    line(ctx, hexCorner(center, size, 0))
    ctx.fill();
    ctx.closePath();
  }

  def main(): Unit = {
    //TODO from dwu to jpaulson: I'm not sure I understand what "setupUI _" means. Could you explain to me?
    jQuery(setupUI _)
    ()
  }

  def setupUI() : Unit = {
    val canvas = jQuery("#board").get(0).asInstanceOf[html.Canvas]
    val ctx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    val origin = Point(2.0*size, 2.0*size)

    val board = BoardState.create(Plane.create(10, 10, HexTopology, Ground))

    var selected : Option[Piece] = None
    var mouse = Loc(0, 0)
    var path = List(Loc(0, 0))

    // Update path to be a shortest path from [selected] to [mouse] that
    // shares the longest prefix with the current [path]
    def updatePath() : Unit = {
      def distance(x : Loc, y : Loc) : Int = {
        board.tiles.topology.distance(x, y)
      }
      selected match {
        case None => ()
        case Some(p) =>
          val loc = p.loc
          //TODO from dwu to jpaulson: In scala, ".equals" is the same as "==", so just use "==". Unlike ocaml, "==" in Scala will call out
          //to the object-specific comparison method so it's right even for things like Maps with non-semantic internal structure.
          if(path.size==0 || (!path(0).equals(loc))) {
            path = List(loc)
          }
          while(path.size > 0 && distance(loc, mouse) != path.size-1 + distance(path(path.size-1), mouse)) {
            path = path.init
          }
          if(path(path.size-1) != mouse) {
            for(p <- board.tiles.topology.adj(path(path.size-1))) {
              if(path.size-1 + distance(path(path.size-1), mouse) == path.size + distance(p, mouse)) {
                path = path :+ p
              }
            }
          }
      }
    }

    def showBoard(board : BoardState) : Unit = {
      ctx.clearRect(0.0, 0.0, canvas.width.toDouble, canvas.height.toDouble)
      drawHex(ctx, hexCenter(mouse, origin), "purple", size-1.0)
      selected match {
        case None => ()
        case Some(p) =>
          drawHex(ctx, hexCenter(p.loc, origin), "red", size-1.0)
      }
      board.tiles.iteri {case ((x, y), tile) =>
        val center = hexCenter(Loc(x,y), origin)
        tile.terrain match {
          case Wall => drawHex(ctx, center, "black", size-1.0)
          case Ground => drawHex(ctx, center, "green", size-1.0)
          case Water => drawHex(ctx, center, "blue", size-1.0)
          case ManaSpire => drawHex(ctx, center, "orange", size-1.0)
          case Spawner(S0, _) =>
            drawHex(ctx, center, "gray", size-1.0)
            drawHex(ctx, center, "red", size-10.0)
          case Spawner(S1, _) =>
            drawHex(ctx, center, "gray", size-1.0)
            drawHex(ctx, center, "blue", size-10.0)
        }
      }
      board.pieces.iteri {case ((x,y), pieces) =>
        def color(p : Piece) : String = {
          selected match {
            case None => "blue"
            case Some(piece) =>
              if (piece == p) "red" else "blue"
          }
        }
        val center = hexCenter(Loc(x,y), origin)
        pieces match {
          case Nil => ()
          case p :: Nil =>
            drawHex(ctx, center, color(p), size-5.0)
            text(ctx, p.id.toString, center, "black")
          case p1 :: p2 :: Nil  =>
            val c1 = Point(center.x, center.y-size/2)
            val c2 = Point(center.x, center.y+size/2)
            drawHex(ctx, c1, color(p1), size/2-0.5)
            text(ctx, p1.id.toString, c1, "black")
            drawHex(ctx, c2, color(p2), size/2-0.5)
            text(ctx, p2.id.toString, c2, "black")
          case p1 :: p2 :: p3 :: Nil  => ()
            val c1 = Point(center.x, center.y-size/2)
            val c2 = (center+hexCorner(center, size, 2))/2
            val c3 = (center+hexCorner(center, size, 0))/2
            drawHex(ctx, c1, color(p1), size/2-0.5)
            text(ctx, p1.id.toString, c1, "black")
            drawHex(ctx, c2, color(p2), size/2-0.5)
            text(ctx, p2.id.toString, c2, "black")
            drawHex(ctx, c3, color(p3), size/2-0.5)
            text(ctx, p3.id.toString, c3, "black")
          case _ => ()
        }
      }
      if(path.size > 0) {
        ctx.globalAlpha = 1.0
        ctx.fillStyle = "black"
        ctx.setLineDash(js.Array(5.0, 10.0))
        ctx.beginPath()
        move(ctx, hexCenter(path(0), origin))
        for(i <- 1 to path.size-1) {
          line(ctx, hexCenter(path(i), origin))
        }
        ctx.stroke()
        ctx.closePath()
      }
    }

    def mouseToHex(e : MouseEvent) : Point = {
      val rect = canvas.getBoundingClientRect()
      val pixelPos = Point(e.clientX - rect.left - origin.x, e.clientY - rect.top - origin.y)
      Point((pixelPos.x * Math.sqrt(3)/3 - pixelPos.y/3) / size, pixelPos.y * 2.0/3.0 / size)
    }

    def mouseUnit(e : MouseEvent) : Option[Piece] = {
      val hexPoint = mouseToHex(e)
      val hexLoc = hexRound(hexPoint)
      val hexDelta = Point(hexPoint.x - hexLoc.x, hexPoint.y - hexLoc.y)
      println(hexDelta.x+" "+hexDelta.y)
      board.pieces(hexLoc) match {
        case Nil => None
        case p :: Nil => Some(p)
        case p1 :: p2 :: Nil =>
          if(hexDelta.y < 0) {
            Some(p1)
          } else {
            Some(p2)
          }
        case p1 :: p2 :: p3 :: Nil =>
          if(hexDelta.y < 0) {
            Some(p1)
          } else if(hexDelta.x < 0) {
            Some(p2)
          } else {
            Some(p3)
          }
        case _ => None
      }
    }

    def mousePos(e : MouseEvent) : Loc = {
      hexRound(mouseToHex(e))
    }

    def mousedown(e : MouseEvent) : Unit = {
      selected match {
        case None =>
          selected = mouseUnit(e)
        case Some(piece) =>
          val action = Movements(List(Movement(StartedTurnWithID(piece.id), path.toArray)))
          val result = board.doAction(action)
          selected = None
          path = List()
          result match {
            case Success(_) => ()
            case Failure(error) =>
              println(error)
              selected = mouseUnit(e)
          }
      }
      showBoard(board)
    }
    def mousemove(e : MouseEvent) : Unit = {
      mouse = mousePos(e)
      updatePath()
      showBoard(board)
    }

    canvas.onmousedown = mousedown _
    canvas.onmousemove = mousemove _


    val zombie = PieceStats(
      name = "zombie",
      displayName = "Zombie",
      attackEffect = Some(Damage(1)),
      defense = 2,
      moveRange = 100,
      attackRange = 1,
      cost = 2,
      rebate = 0,
      isNecromancer = false,
      isFlying = false,
      isLumbering = true,
      isPersistent = false,
      isEldritch = false,
      isWailing = false,
      hasFlurry = false,
      hasBlink = false,
      canHurtNecromancer = true,
      swarmMax = 3, //Swarming 3 for testing
      spawnRange = 0,
      extraMana = 0,
      deathSpawn = None,
      freeSpawn = None,
      abilities = Map.empty
    )

    //TODO from dwu to jpaulson: board.tiles(x,y) = z   -  syntatic sugar converts this into .update
    //TODO from dwu to jpaulson: Don't mutate directly, instead modify the terrain Plane to be what you want first
    //and then create the BoardState with the desired terain.
    board.tiles.update(0, 0, Tile.create(ManaSpire))
    board.tiles.update(0, 1, Tile.create(Wall))
    board.tiles.update(1, 0, Tile.create(Water))
    board.tiles.update(1, 1, Tile.create(Spawner(S0, zombie)))
    board.tiles.update(2, 0, Tile.create(Spawner(S1, zombie)))

    board.spawnPieceInitial(S0, zombie, Loc(2, 1))
    board.spawnPieceInitial(S0, zombie, Loc(2, 2))
    board.spawnPieceInitial(S0, zombie, Loc(2, 2))

    board.spawnPieceInitial(S0, zombie, Loc(2, 3))
    board.spawnPieceInitial(S0, zombie, Loc(2, 3))
    board.spawnPieceInitial(S0, zombie, Loc(2, 3))

    showBoard(board)
    ()
  }

  def addClickedMessage(): Unit = {
    jQuery("body").append("<p>You clicked!</p>")
    ()
  }
}

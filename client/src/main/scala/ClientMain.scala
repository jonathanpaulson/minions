import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport
import org.scalajs.jquery.jQuery
import org.scalajs.dom
import org.scalajs.dom.html
import scala.util.Random
import scala.scalajs.js
import scala.util.{Try, Success, Failure}

case class Point(x: Double, y: Double){
  def +(p: Point) = Point(x + p.x, y + p.y)
  def /(d: Double) = Point(x / d, y / d)
  def toLoc = Loc(x.round.toInt, y.round.toInt)
}

object ClientMain extends JSApp {
  val size = 30.0

  def move(ctx : dom.CanvasRenderingContext2D, point : Point) : Unit = {
    ctx.moveTo(Math.floor(point.x), Math.floor(point.y));
  }
  def line(ctx : dom.CanvasRenderingContext2D, point : Point) : Unit = {
    ctx.lineTo(Math.floor(point.x), Math.floor(point.y));
  }
  def text(ctx : dom.CanvasRenderingContext2D, text : String, point : Point, color : String) : Unit = {
    ctx.globalAlpha = 1.0
    ctx.fillStyle = color
    ctx.textAlign = "center"
    ctx.fillText(text, Math.floor(point.x), Math.floor(point.y))
  }

  def hex_round(pos : Point) : Loc = {
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
      assert(dz >= dx && dz>=dy);
      Loc(rx.toInt, -(rx+ry).toInt)
    }
  }

  // Coordinate system: plane s.t. x+y+z = 0
  // x is up and to the right
  // y is up and to the left
  // z is down
  // We store Point(x, z); we can recover y as -(x+z)
  def hex_center(pos : Loc, origin : Point) : Point = {
    Point(
      origin.x + size * Math.sqrt(3) * (pos.x.toDouble + pos.y.toDouble/2.0),
      origin.y + size * 3.0/2.0 * pos.y.toDouble
    )
  }

  def hex_corner(center : Point, size : Double, corner : Int) : Point = {
    val angle_deg = 60*corner + 30
    val angle_rad = Math.PI/180 * angle_deg
    Point(center.x+size*Math.cos(angle_rad), center.y+size*Math.sin(angle_rad))
  }

  def draw_hex(ctx : dom.CanvasRenderingContext2D, center : Point, color : String, size : Double) : Unit = {
    ctx.globalAlpha = 0.2
    ctx.fillStyle = color
    ctx.beginPath()
    move(ctx, hex_corner(center, size, 0))
    line(ctx, hex_corner(center, size, 1))
    line(ctx, hex_corner(center, size, 2))
    line(ctx, hex_corner(center, size, 3))
    line(ctx, hex_corner(center, size, 4))
    line(ctx, hex_corner(center, size, 5))
    line(ctx, hex_corner(center, size, 0))
    ctx.fill();
    ctx.closePath();
  }

  def main(): Unit = {
    jQuery(setupUI _)
    ()
  }

  def setupUI() : Unit = {
    val canvas = jQuery("#board").get(0).asInstanceOf[html.Canvas]
    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    val origin = Point(2.0*size, 2.0*size)

    val board = BoardState.create(Plane.create(10, 10, HexTopology, Ground))

    var selected : Option[Piece] = None
    var mouse : Loc = Loc(0, 0)
    var path : List[Loc] = List()

    // Update path to be a shortest path from [selected] to [mouse] that
    // shares the longest prefix with the current [path]
    def update_path() : Unit = {
      def distance(x : Loc, y : Loc) : Int = {
        board.tiles.topology.distance(x, y)
      }
      selected match {
        case None => ()
        case Some(p) =>
          val loc = p.loc
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

    def pixel_location(piece : Piece) : Point = {
      val loc = piece.loc
      val center = hex_center(piece.loc, origin)
      board.pieces(loc) match {
        case Nil => sys.error("")
        case p :: Nil => hex_center(piece.loc, origin)
        case p1 :: p2 :: Nil =>
          if(piece == p1) {
            Point(center.x, center.y-size/2)
          } else {
            assert(piece == p2)
            Point(center.x, center.y+size/2)
          }
        case p1 :: p2 :: p3 :: Nil =>
          if(piece == p1) {
            Point(center.x, center.y-size/2)
          } else if(piece == p2) {
            (center+hex_corner(center, size, 2))/2
          } else {
            assert(piece == p3)
            (center+hex_corner(center, size, 0))/2
          }
        case _ => sys.error("")
      }
    }

    def show_board(board : BoardState) : Unit = {
      ctx.clearRect(0.0, 0.0, canvas.width.toDouble, canvas.height.toDouble)
      board.tiles.iteri {case ((x, y), tile) =>
        val center = hex_center(Loc(x,y), origin)
        tile.terrain match {
          case Wall => draw_hex(ctx, center, "black", size-1.0)
          case Ground => draw_hex(ctx, center, "green", size-1.0)
          case Water => draw_hex(ctx, center, "blue", size-1.0)
          case ManaSpire => draw_hex(ctx, center, "orange", size-1.0)
          case Spawner(S0, _) =>
            draw_hex(ctx, center, "gray", size-1.0)
            draw_hex(ctx, center, "red", size-10.0)
          case Spawner(S1, _) =>
            draw_hex(ctx, center, "gray", size-1.0)
            draw_hex(ctx, center, "blue", size-10.0)
        }
      }
      board.pieces.iteri {case ((x,y), pieces) =>
        def color(piece : Piece) : String = {
          val is_selected =
            selected match {
              case None => false
              case Some(p) => (p == piece)
            }
          (is_selected, piece.side) match {
              case (true, S0) => "aqua"
              case (false, S0) => "blue"
              case (true, S1) => "lightcoral"
              case (false, S1) => "red"
          }
        }
        pieces match {
          case Nil => ()
          case p :: Nil =>
            draw_hex(ctx, pixel_location(p), color(p), size-5.0)
            text(ctx, p.id.toString, pixel_location(p), "black")
          case p1 :: p2 :: Nil  =>
            draw_hex(ctx, pixel_location(p1), color(p1), size/2-0.5)
            text(ctx, p1.id.toString, pixel_location(p1), "black")
            draw_hex(ctx, pixel_location(p2), color(p2), size/2-0.5)
            text(ctx, p2.id.toString, pixel_location(p2), "black")
          case p1 :: p2 :: p3 :: Nil  => ()
            draw_hex(ctx, pixel_location(p1), color(p1), size/2-0.5)
            text(ctx, p1.id.toString, pixel_location(p1), "black")
            draw_hex(ctx, pixel_location(p2), color(p2), size/2-0.5)
            text(ctx, p2.id.toString, pixel_location(p2), "black")
            draw_hex(ctx, pixel_location(p3), color(p3), size/2-0.5)
            text(ctx, p3.id.toString, pixel_location(p3), "black")
          case _ => ()
        }
      }
      if(path.size > 0) {
        ctx.globalAlpha = 1.0
        ctx.fillStyle = "black"
        ctx.setLineDash(js.Array(5.0, 10.0))
        ctx.beginPath()
        selected match {
          case None => sys.error("")
          case Some(piece) =>
            move(ctx, pixel_location(piece))
        }
        for(i <- 1 to path.size-1) {
          line(ctx, hex_center(path(i), origin))
        }
        ctx.stroke()
        ctx.closePath()
      }
    }

    def mouse_to_hex(e : dom.MouseEvent) : Point = {
      val rect = canvas.getBoundingClientRect()
      val pixel_pos = Point(e.clientX - rect.left - origin.x, e.clientY - rect.top - origin.y)
      Point((pixel_pos.x * Math.sqrt(3)/3 - pixel_pos.y/3) / size, pixel_pos.y * 2.0/3.0 / size)
    }

    def mouse_unit(e : dom.MouseEvent) : Option[Piece] = {
      val hex_point = mouse_to_hex(e)
      val hex_loc = hex_round(hex_point)
      val hex_delta = Point(hex_point.x - hex_loc.x, hex_point.y - hex_loc.y)
      println(hex_loc.x+" "+hex_loc.y)
      board.pieces(hex_loc) match {
        case Nil => None
        case p :: Nil => Some(p)
        case p1 :: p2 :: Nil =>
          if(hex_delta.y < 0) {
            Some(p1)
          } else {
            Some(p2)
          }
        case p1 :: p2 :: p3 :: Nil =>
          if(hex_delta.y < 0) {
            Some(p1)
          } else if(hex_delta.x < 0) {
            Some(p2)
          } else {
            Some(p3)
          }
        case _ => None
      }
    }

    def mouse_pos(e : dom.MouseEvent) : Loc = {
      hex_round(mouse_to_hex(e))
    }

    def mousedown(e : dom.MouseEvent) : Unit = {
      selected match {
        case None =>
          selected = mouse_unit(e)
        case Some(piece) =>
          val action = Movements(List(Movement(StartedTurnWithID(piece.id), path.toArray)))
          val result = board.doAction(action)
          selected = None
          path = List()
          result match {
            case Success(_) => ()
            case Failure(error) =>
              println(error)
              selected = mouse_unit(e)
          }
      }
      show_board(board)
    }
    def mousemove(e : dom.MouseEvent) : Unit = {
      mouse = mouse_pos(e)
      update_path()
      show_board(board)
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

    board.spawnPieceInitial(S1, zombie, Loc(3, 2))

    show_board(board)
    ()
  }

  def addClickedMessage(): Unit = {
    jQuery("body").append("<p>You clicked!</p>")
    ()
  }
}

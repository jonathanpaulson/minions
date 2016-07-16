import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport
import org.scalajs.jquery.jQuery
import org.scalajs.dom
import org.scalajs.dom.html
import scala.util.Random
import scala.scalajs.js

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
      // assert (dz >= dx && dz>=dy);
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

    var mouse = Loc(0, 0)
    var selected = Loc(2, 0)
    var path = List(Loc(0, 0))

    // Update path to be a shortest path from [selected] to [mouse] that
    // shares the longest prefix with the current [path]
    def update_path() : Unit = {
      def distance(x : Loc, y : Loc) : Int = {
        board.tiles.topology.distance(x, y)
      }
      if(path.size==0 || (!path(0).equals(selected))) {
        path = List(selected)
      }
      while(path.size > 0 && distance(selected, mouse) != path.size-1 + distance(path(path.size-1), mouse)) {
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

    def show_board(board : BoardState) : Unit = {
      ctx.clearRect(0.0, 0.0, canvas.width.toDouble, canvas.height.toDouble)
      draw_hex(ctx, hex_center(mouse, origin), "purple", size-1.0)
      draw_hex(ctx, hex_center(selected, origin), "red", size-1.0)
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
        val center = hex_center(Loc(x,y), origin)
        pieces match {
          case Nil => ()
          case p :: Nil =>
            draw_hex(ctx, center, "blue", size-5.0)
          case p1 :: p2 :: Nil  =>
            val c1 = Point(center.x, center.y-size/2)
            val c2 = Point(center.x, center.y+size/2)
            draw_hex(ctx, c1, "blue", size/2-0.5)
            draw_hex(ctx, c2, "blue", size/2-0.5)
          case p1 :: p2 :: p3 :: Nil  => ()
            val c1 = Point(center.x, center.y-size/2)
            val c2 = (center+hex_corner(center, size, 2))/2
            val c3 = (center+hex_corner(center, size, 0))/2
            draw_hex(ctx, c1, "blue", size/2-0.5)
            draw_hex(ctx, c2, "blue", size/2-0.5)
            draw_hex(ctx, c3, "blue", size/2-0.5)
          case _ => ()
        }
      }
      if(path.size > 0) {
        ctx.globalAlpha = 1.0
        ctx.fillStyle = "black"
        ctx.setLineDash(js.Array(5.0, 10.0))
        ctx.beginPath()
        move(ctx, hex_center(path(0), origin))
        for(i <- 1 to path.size-1) {
          line(ctx, hex_center(path(i), origin))
        }
        ctx.stroke()
        ctx.closePath()
      }
    }

    def mouse_pos(e : dom.MouseEvent) : Loc = {
      val rect = canvas.getBoundingClientRect()
      val pixel_pos = Point(e.clientX - rect.left - origin.x, e.clientY - rect.top - origin.y)
      val hex_pos_f = Point((pixel_pos.x * Math.sqrt(3)/3 - pixel_pos.y/3) / size, pixel_pos.y * 2.0/3.0 / size)
      val hex_pos = hex_round(hex_pos_f)
      hex_pos
    }

    def mousedown(e : dom.MouseEvent) : Unit = {
      if(board.pieces(selected).length == 1) {
        val piece = board.pieces(selected)(0)
        val action = Movements(List(Movement(StartedTurnWithID(piece.id), path.toArray)))
        val result = board.doAction(action)
        println(result)
      }
      selected = mouse_pos(e)
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

    //TODO from dwu to jpaulson: Don't mutate the board directly, instead use spawnPieceInternal. Don't create Pieces directly either.
    board.spawnPieceInitial(S0, zombie, Loc(2, 1))

    board.spawnPieceInitial(S0, zombie, Loc(2, 2))
    board.spawnPieceInitial(S0, zombie, Loc(2, 2))

    board.spawnPieceInitial(S0, zombie, Loc(2, 3))
    board.spawnPieceInitial(S0, zombie, Loc(2, 3))
    board.spawnPieceInitial(S0, zombie, Loc(2, 3))

    show_board(board)
    ()
  }

  def addClickedMessage(): Unit = {
    jQuery("body").append("<p>You clicked!</p>")
    ()
  }
}

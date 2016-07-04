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
}

object ClientMain extends JSApp {
  val size = 30

  def move(ctx : dom.CanvasRenderingContext2D, point : Point) : Unit = {
    ctx.moveTo(Math.floor(point.x), Math.floor(point.y));
  }
  def line(ctx : dom.CanvasRenderingContext2D, point : Point) : Unit = {
    ctx.lineTo(Math.floor(point.x), Math.floor(point.y));
  }

  // Coordinate system: plane s.t. x+y+z = 0
  // x is up and to the right
  // y is up and to the left
  // z is down
  // We store Point(x, z); we can recover y as -(x+z)
  def hex_center(pos : Point, origin : Point) : Point = {
    Point(
      origin.x + size * Math.sqrt(3) * (pos.x + pos.y/2),
      origin.y + size * 3.0/2.0 * pos.y
    )
  }

  def hex_corner(center : Point, size : Double, corner : Int) : Point = {
    val angle_deg = 60*corner + 30
    val angle_rad = Math.PI/180 * angle_deg
    Point(center.x+size*Math.cos(angle_rad), center.y+size*Math.sin(angle_rad))
  }

  def draw_hex(ctx : dom.CanvasRenderingContext2D, center : Point, color : String) : Unit = {
    ctx.globalAlpha = 0.2
    ctx.fillStyle = color
    ctx.beginPath()
    move(ctx, hex_corner(center, size-1.0, 0))
    line(ctx, hex_corner(center, size-1.0, 1))
    line(ctx, hex_corner(center, size-1.0, 2))
    line(ctx, hex_corner(center, size-1.0, 3))
    line(ctx, hex_corner(center, size-1.0, 4))
    line(ctx, hex_corner(center, size-1.0, 5))
    line(ctx, hex_corner(center, size-1.0, 0))
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

    val zombie = PieceStats(
      name = "Zombie",
      attackEffect = Some(Damage(1)),
      defense = 2,
      moveRange = 1,
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
      swarmMax = 2,
      spawnRange = 0,
      extraMana = 0,
      deathSpawn = None,
      freeSpawn = None,
      abilities = Map.empty
    )

    val origin = Point(2.0*size, 2.0*size)

    val board = Board.create(tiles = Plane.create(10, 10, HexTopology, new Tile(terrain=Ground, modsWithDuration=List())))
    board.tiles.update(0, 0, new Tile(terrain=ManaSpire, modsWithDuration=List()))
    board.tiles.update(0, 1, new Tile(terrain=Wall, modsWithDuration=List()))
    board.tiles.update(1, 0, new Tile(terrain=Water, modsWithDuration=List()))
    board.tiles.update(1, 1, new Tile(terrain=Spawner(S0, zombie), modsWithDuration=List()))

    board.tiles.iteri {case ((x, y), tile) =>
      val color = tile.terrain match {
        case Wall => "black"
        case Ground => "green"
        case Water => "blue"
        case ManaSpire => "orange"
        case Spawner(_, _) => "red"
      }
      val center = hex_center(Point(x.toDouble,y.toDouble), origin)
      draw_hex(ctx, center, color)
    }
    board.pieces.iteri {case ((x,y), pieces) =>
      for (piece <- pieces) {
        // TODO(jpaulson): Draw pieces
      }
    }
    ()
  }

  def addClickedMessage(): Unit = {
    jQuery("body").append("<p>You clicked!</p>")
    ()
  }
}


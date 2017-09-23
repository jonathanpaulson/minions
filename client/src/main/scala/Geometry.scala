package minionsgame.jsclient

import scala.util.{Try, Success, Failure}

import minionsgame.core._
import RichImplicits._

//TODO dwu: Consider having an implicit HexLoc => Loc

/**
  * PixelLoc, PixelVec:
  * The location of a pixel in normal canvas coordinates, and vectors for offsets between those locations.
  * As usual, (0,0) is the upper left corner, x is to the right, y is downward.
  *
  * Note: As these are doubles and capable of representing subpixel positions, these are actually locations of the
  * corners of pixels. For example, (14,52) is the upper left corner of a pixel, (14.5,52) is the midpoint of the top edge,
  * (14.5,52.5) is the center of the pixel, etc.
  */
object PixelLoc {
  def midpoint(a: PixelLoc, b: PixelLoc): PixelLoc = PixelLoc((a.x+b.x)/2.0,(a.y+b.y)/2.0)

  def ofLoc(loc : Loc, gridSize: Double) : PixelLoc = {
    val x = loc.x.toDouble
    val y = loc.y.toDouble
    PixelLoc(
      gridSize * Math.sqrt(3) * (x + y/2.0),
      gridSize * 3.0/2.0 * y
    )
  }
  def ofHexLoc(hexLoc : HexLoc, gridSize: Double) : PixelLoc = {
    val x = hexLoc.x
    val y = hexLoc.y
    PixelLoc(
      gridSize * Math.sqrt(3) * (x + y/2.0),
      gridSize * 3.0/2.0 * y
    )
  }
}
case class PixelLoc(x: Double, y: Double){
  def +(d: PixelVec): PixelLoc = PixelLoc(x + d.dx, y + d.dy)
  def -(d: PixelVec): PixelLoc = PixelLoc(x - d.dx, y - d.dy)
  def -(p: PixelLoc): PixelVec = PixelVec(x - p.x, y - p.y)
}
case class PixelVec(dx: Double, dy: Double){
  def +(p: PixelLoc): PixelLoc = PixelLoc(dx + p.x, dy + p.y)
  def +(d: PixelVec): PixelVec = PixelVec(dx + d.dx, dy + d.dy)
  def -(d: PixelVec): PixelVec = PixelVec(dx - d.dx, dy - d.dy)
}

/**
  * HexLoc, HexVec:
  * A floating point version of Loc and Vec from CommonTypes.scala. Integer coordinates correspond to the centers of hexes.
  *
  * x is up and to the right /
  * y is down |
  * z is up and to the left \
  *
  *        z 0  -1  -2   -3   -4
  *
  *    x 0   1   2   3   4      -5
  *  y   / \ / \ / \ / \ / \ y
  *   0 |   |   |   |   |   | 0   -6
  *      \ / \ / \ / \ / \ / \
  *     1 |   |   |   |   |   | 1   -7
  *        \ / \ / \ / \ / \ / \
  *       2 |   |   |   |   |   | 2   -8
  *          \ / \ / \ / \ / \ / \
  *         3 |   |   |   |   |   | 3
  *            \ / \ / \ / \ / \ / \
  *           4 |   |   |   |   |   | 4
  *              \ / \ / \ / \ / \ /
  *              x 0   1   2   3   4
  *
  *  This is consistent with HexTopology in CommonTypes.scala having (x-1,y-1) and (x+1,y+1) not be adjacent to (x,y),
  *  but all other (x + {-1,0,1}, y + {-1,0,1}) be adjacent.
  *
  *  Corner 0 = maximal x
  *  Corner 2 = maximal y
  *  Corner 4 = maximal z
  *  Hexant n = the triangle extending from the center out between corner n and corner n+1 mod 6
  *
  *  (x,y,z)
  *                            (corner 5)
  *                          (1/3,-2/3,1/3)
  *                                .
  *                            .       .
  *   (corner 4)           .       .       .           (corner 0)
  * (-1/3,-1/3,2/3)    .                       .     (2/3,-1/3,-1/3)
  *                .          4    .     5         .
  *                    .                       .
  *                .       .       .       .       .
  *                            .       .
  *                .     3         .         0     .
  *                            .       .
  *                .       .       .       .       .
  *                    .                       .
  *                .          2    .    1          .
  * (-2/3,1/3/1/3)     .                       .     (1/3,1/3,-2/3)
  *   (corner 3)           .       .       .           (corner 1)
  *                            .       .
  *                                .
  *                          (-1/3,2/3,-1/3)
  *                             (corner 2)
  */

object HexLoc {
  def midpoint(a: HexLoc, b: HexLoc): HexLoc = HexLoc((a.x+b.x)/2.0,(a.y+b.y)/2.0)

  def ofLoc(loc: Loc, flipDisplay: Boolean, board: BoardState): HexLoc = {
    if(flipDisplay && board.tiles.inBounds(loc))
      HexLoc((board.tiles.xSize - loc.x - 1).toDouble, (board.tiles.ySize - loc.y - 1).toDouble)
    else
      HexLoc(loc.x.toDouble,loc.y.toDouble)
  }

  def ofPixel(p: PixelLoc, gridSize: Double): HexLoc = {
    HexLoc((p.x * Math.sqrt(3)/3 - p.y/3) / gridSize, p.y * 2.0/3.0 / gridSize)
  }
}
case class HexLoc(x: Double, y: Double) {
  def z: Double = -(x+y)
  def +(d: HexVec): HexLoc = HexLoc(x + d.dx, y + d.dy)
  def -(d: HexVec): HexLoc = HexLoc(x - d.dx, y - d.dy)
  def -(p: HexLoc): HexVec = HexVec(x - p.x, y - p.y)
  def -(p: Loc): HexVec = HexVec(x - p.x, y - p.y)

  //Find the Loc for the hex that contains this HexLoc, taking into account the hexagon shape of a cell,
  //And also whether or not we're flipping the board display around.
  //Also returns the extra offset within the cell.
  def round(flipDisplay: Boolean, board: BoardState): (Loc,HexVec) = {
    /*
     This algorithm is magic. Here's how it works:
     Given a hexagon H, let H' be the inscribed hexagon formed by connecting the midpoints of the sides of H.
     Let H'x be the rhombus formed by adjoining equilateral trinagles on to the sides of H' that are furthest
     in the +x and -x directions, similarly define H'y and H'z.

     Note that H' is the intersection of H'x, H'y, H'z.

     Drawing a picture should make it evident that the borders of these rhombuses are the lines corresponding to
     x,y,z = 0.5 mod 1, and further that within H'x, y and z are rounded correctly, within H'y, x and z are rounded
     correctl, and within H'z, x and y are rounded correctly.

     Case 1: Loc is within H': Then Loc is within the intersection of all three rhombuses. Then all roundings are correct.
     Therefore regardless of which branch of the if/else is taken below, the value will be correct.

     Case 2: Loc is within H but not within H':
     Assume by symmetry that WOLOG, Loc is within the triangular sliver of (H - H') in the +x direction. Note that
     dx, dy, dz are linear on this triangle (i.e. none of them cross a rounding boundary or a Math.abs kink).

     The corners of this triangle are (x,y,z) = { A: (+2/3,-1/3,-1/3), B: (1/2,-1/2,0), C: (1/2,0,-1/2) }
     By inspection, line AB satisfies dx = dy, with dx >= dy if you move inward.
     And line BC satifies dx = dz, with dx >= dz if you move inward.
     By linearity of dx,dy,dz on the triangle, the entire triangular silver will fall into the (dx >= dy && dx >= dz) case.
     Also note that the entire trianglar sliver is a subset of H'x, on which the rounding of y and z are correct.
     Since the algorithm relies only on the rounding of y and z in this case it will be correct.
     */

    val rx = Math.round(x)
    val ry = Math.round(y)
    val rz = Math.round(z)
    val dx = Math.abs(x - rx)
    val dy = Math.abs(y - ry)
    val dz = Math.abs(z - rz)

    val loc = {
      if(dx >= dy && dx >= dz)
        Loc(-(ry+rz).toInt, ry.toInt) //create loc from y and z
      else if(dz >= dx && dz >= dy)
        Loc(rx.toInt, ry.toInt)       //create loc from x and y
      else {
        assert(dy >= dx && dy>=dz);
        Loc(rx.toInt, -(rx+rz).toInt) //create loc from x and z
      }
    }

    val delta = this - loc
    if(flipDisplay && board.tiles.inBounds(loc))
      (Loc(board.tiles.xSize - loc.x - 1, board.tiles.ySize - loc.y - 1), delta)
    else
      (loc, delta)
  }
}

object HexVec {
  val corners: Array[HexVec] = {
    val third: Double = 1.0 / 3.0
    Array(
      HexVec( 2*third, -1*third),
      HexVec( 1*third,  1*third),
      HexVec(-1*third,  2*third),
      HexVec(-2*third,  1*third),
      HexVec(-1*third, -1*third),
      HexVec( 1*third, -2*third)
    )
  }
}
case class HexVec(dx: Double, dy: Double){
  def dz: Double = -(dx+dy)
  def +(p: HexLoc): HexLoc = HexLoc(dx + p.x, dy + p.y)
  def +(d: HexVec): HexVec = HexVec(dx + d.dx, dy + d.dy)
  def -(d: HexVec): HexVec = HexVec(dx - d.dx, dy - d.dy)
  def *(c: Double): HexVec = HexVec(dx*c,dy*c)

  //Determine which hexant this belongs to
  def hexant(): Int = {
    val moreLeftThanRight = dz > dx
    val moreURThanDL = dx > dy
    val moreDRthanUL = dy > dz

    (moreLeftThanRight, moreURThanDL, moreDRthanUL) match {
      case (false,false,false) => 0 //Zero vector
      case (false,false,true) => 1
      case (false,true,false) => 5
      case (false,true,true) => 0
      case (true,false,false) => 3
      case (true,false,true) => 2
      case (true,true,false) => 4
      case (true,true,true) => 0 //Shouldn't happen
    }
  }

  //Determine which hexant this belongs to
  def closestCorner(): Int = {
    (dx > 0, dy > 0, dz > 0) match {
      case (false,false,false) => 0 //Zero vector
      case (false,false,true) => 4
      case (false,true,false) => 2
      case (false,true,true) => 3
      case (true,false,false) => 0
      case (true,false,true) => 5
      case (true,true,false) => 1
      case (true,true,true) => 0 //Shouldn't happen
    }
  }
}

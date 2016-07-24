package minionsgame.jsclient

import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html.Canvas

import minionsgame.core._
import RichImplicits._

object Drawing {
  //The euclidean distance from the center of a hexagon to the corner in pixels.
  val gridSize = 30.0
  val tileScale = 29.0 / gridSize
  val pieceScale = 25.0 / gridSize
  val techScale = 20.0 / gridSize
  val smallPieceScale = 14.0 / gridSize
  val smallPieceOffset = 15.0 / gridSize

  private def move(ctx : CanvasRenderingContext2D, loc : Loc) : Unit = {
    move(ctx,HexLoc.ofLoc(loc))
  }
  private def move(ctx : CanvasRenderingContext2D, hexLoc : HexLoc) : Unit = {
    val pixel = PixelLoc.ofHexLoc(hexLoc,gridSize)
    ctx.moveTo(Math.floor(pixel.x), Math.floor(pixel.y));
  }
  private def line(ctx : CanvasRenderingContext2D, loc : Loc) : Unit = {
    line(ctx,HexLoc.ofLoc(loc))
  }
  private def line(ctx : CanvasRenderingContext2D, hexLoc : HexLoc) : Unit = {
    val pixel = PixelLoc.ofHexLoc(hexLoc,gridSize)
    ctx.lineTo(Math.floor(pixel.x), Math.floor(pixel.y));
  }
  private def text(ctx : CanvasRenderingContext2D, text : String, pixel : PixelLoc, color : String) : Unit = {
    ctx.globalAlpha = 1.0
    ctx.fillStyle = color
    ctx.textAlign = "center"
    ctx.fillText(text, Math.floor(pixel.x), Math.floor(pixel.y))
  }

  private def hexCorner(hexLoc : HexLoc, scale : Double, corner : Int) : HexLoc = {
    hexLoc + HexVec.corners(corner) * scale
  }

  private def drawHex(ctx : CanvasRenderingContext2D, loc : Loc, color : String, scale : Double) : Unit = {
    drawHex(ctx,HexLoc.ofLoc(loc), color, scale)
  }
  private def drawHex(ctx : CanvasRenderingContext2D, hexLoc : HexLoc, color : String, scale : Double) : Unit = {
    ctx.globalAlpha = 0.2
    ctx.fillStyle = color
    ctx.beginPath()
    move(ctx, hexLoc + HexVec.corners(0) * scale)
    line(ctx, hexLoc + HexVec.corners(1) * scale)
    line(ctx, hexLoc + HexVec.corners(2) * scale)
    line(ctx, hexLoc + HexVec.corners(3) * scale)
    line(ctx, hexLoc + HexVec.corners(4) * scale)
    line(ctx, hexLoc + HexVec.corners(5) * scale)
    line(ctx, hexLoc + HexVec.corners(0) * scale)
    ctx.fill();
    ctx.closePath();
  }

  private def drawTile(ctx : CanvasRenderingContext2D, loc : Loc, tile: Tile) : Unit = {
    drawTile(ctx,HexLoc.ofLoc(loc),tile)
  }
  private def drawTile(ctx : CanvasRenderingContext2D, hexLoc : HexLoc, tile: Tile) : Unit = {
    tile.terrain match {
      case Wall => drawHex(ctx, hexLoc, "white", tileScale)
      case Ground => drawHex(ctx, hexLoc, "green", tileScale)
      case Water => drawHex(ctx, hexLoc, "blue", tileScale)
      case ManaSpire => drawHex(ctx, hexLoc, "orange", tileScale)
      case Spawner(S0, _) =>
        drawHex(ctx, hexLoc, "gray", tileScale)
        drawHex(ctx, hexLoc, "red", tileScale*0.7)
      case Spawner(S1, _) =>
        drawHex(ctx, hexLoc, "gray", tileScale)
        drawHex(ctx, hexLoc, "blue", tileScale*0.7)
    }
    text(ctx, Loc(hexLoc.x.round.toInt, hexLoc.y.round.toInt).toString, PixelLoc.ofHexLoc(hexLoc, gridSize)+PixelVec(0, -gridSize/2.0), "black")
  }

  private def drawPiece(ctx : CanvasRenderingContext2D, loc : Loc, scale : Double, piece: Piece, isSelected: Boolean) : Unit = {
    drawPiece(ctx,HexLoc.ofLoc(loc),scale,piece,isSelected)
  }
  private def drawPiece(ctx : CanvasRenderingContext2D, hexLoc : HexLoc, scale : Double, piece: Piece, isSelected: Boolean) : Unit = {
    def pieceColor(p : Piece, isSelected: Boolean) : String = {
      (isSelected, p.side) match {
        case (true, S0) => "aqua"
        case (false, S0) => "blue"
        case (true, S1) => "lightcoral"
        case (false, S1) => "red"
      }
    }
    drawHex(ctx, hexLoc, pieceColor(piece,isSelected), scale)
    text(ctx, piece.id.toString, PixelLoc.ofHexLoc(hexLoc,gridSize), "black")
  }


  def drawEverything(
    canvas: Canvas,
    ctx: CanvasRenderingContext2D,
    board : BoardState,
    game: Game,
    translateOrigin: PixelVec,
    hoverLoc: Option[Loc],
    selected: Option[PieceSpec],
    path : List[Loc]
  ) : Unit = {
    ctx.setTransform(1,0,0,1,0,0)
    ctx.clearRect(0.0, 0.0, canvas.width.toDouble, canvas.height.toDouble)
    ctx.translate(translateOrigin.dx,translateOrigin.dy)

    hoverLoc.foreach { hoverLoc => println(hoverLoc) }
    // Techs / Reinforcements
    for((pieceStats, i) <- Units.techs.view.zipWithIndex) {
      val s0 = board.reinforcements(S0).filter(x => x == pieceStats).length
      val s1 = board.reinforcements(S1).filter(x => x == pieceStats).length
      val loc = Loc((i/2)+2, if(i%2==0) -3 else -2)
      val color =
        (game.tech(S0)(i), game.tech(S1)(i)) match {
          case (T0, T0) => "#000000"
          case (T0, T1) => "#770000"
          case (T0, T2) => "#ff0000"
          case (T1, T0) => "#000077"
          case (T1, T1) => "#770077"
          case (T1, T2) => "#ff0077"
          case (T2, T0) => "#0000ff"
          case (T2, T1) => "#7700ff"
          case (T2, T2) => "#ff00ff"
        }
      drawHex(ctx, loc, color, tileScale)
      text(ctx, i.toString, PixelLoc.ofHexLoc(HexLoc.ofLoc(loc), gridSize), "black")
      text(ctx, s0.toString, PixelLoc.ofHexLoc(HexLoc.ofLoc(loc) + HexVec.corners(3) * pieceScale, gridSize), "blue")
      text(ctx, game.tech(S0)(i).toString, PixelLoc.ofHexLoc(HexLoc.ofLoc(loc) + HexVec.corners(4) * techScale, gridSize), "blue")
      text(ctx, s1.toString, PixelLoc.ofHexLoc(HexLoc.ofLoc(loc) + HexVec.corners(1) * pieceScale, gridSize), "red")
      text(ctx, game.tech(S1)(i).toString, PixelLoc.ofHexLoc(HexLoc.ofLoc(loc) + HexVec.corners(0) * techScale, gridSize), "red")
    }

    //Mouse hover
    hoverLoc.foreach { hoverLoc => drawHex(ctx, hoverLoc, "purple", tileScale) }

    //Piece selected by mouse click
    val selectedPiece = selected.flatMap { selected => board.findPiece(selected) }
    selectedPiece.foreach { piece => drawHex(ctx, piece.loc, "red", tileScale) }

    def isPieceSelected(piece: Piece) : Boolean = {
      selectedPiece match {
        case None => false
        case Some(p) => piece.id == p.id
      }
    }

    board.tiles.iteri {case (loc, tile) =>
      drawTile(ctx,loc,tile)
    }
    board.pieces.iteri {case (loc, pieces) =>
      pieces match {
        case Nil => ()
        case p :: Nil =>
          drawPiece(ctx, loc, pieceScale, p, isPieceSelected(p))
        case p1 :: p2 :: Nil  =>
          val c1 = HexLoc.ofLoc(loc) + HexVec.corners(5) * smallPieceOffset
          val c2 = HexLoc.ofLoc(loc) + HexVec.corners(2) * smallPieceOffset
          drawPiece(ctx, c1, smallPieceScale, p1, isPieceSelected(p1))
          drawPiece(ctx, c2, smallPieceScale, p2, isPieceSelected(p2))
        case p1 :: p2 :: p3 :: Nil  => ()
          val c1 = HexLoc.ofLoc(loc) + HexVec.corners(5) * smallPieceOffset
          val c2 = HexLoc.ofLoc(loc) + HexVec.corners(3) * smallPieceOffset
          val c3 = HexLoc.ofLoc(loc) + HexVec.corners(1) * smallPieceOffset
          drawPiece(ctx, c1, smallPieceScale, p1, isPieceSelected(p1))
          drawPiece(ctx, c2, smallPieceScale, p2, isPieceSelected(p2))
          drawPiece(ctx, c3, smallPieceScale, p3, isPieceSelected(p3))
        case _ => ()
      }
    }
    if(path.length > 0) {
      ctx.globalAlpha = 1.0
      ctx.fillStyle = "black"
      ctx.setLineDash(scala.scalajs.js.Array(5.0, 10.0))
      ctx.beginPath()
      selectedPiece match {
        case None => sys.error("")
        case Some(piece) =>
          move(ctx, piece.loc)
      }
      for(i <- 1 to path.length-1) {
        line(ctx, path(i))
      }
      ctx.stroke()
      ctx.closePath()
    }
    selectedPiece match {
      case None => ()
      case Some(piece) =>
        for((loc, (_d, can_land)) <- board.legalMoves(piece, piece.loc)) {
          if(can_land) {
            drawHex(ctx, loc, "yellow", tileScale)
          }
        }
    }
  }
}

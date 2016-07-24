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

  import scala.language.implicitConversions
  private implicit def hexLocOfLoc(loc: Loc): HexLoc = {
    HexLoc.ofLoc(loc)
  }

  private def move(ctx : CanvasRenderingContext2D, hexLoc : HexLoc) : Unit = {
    val pixel = PixelLoc.ofHexLoc(hexLoc,gridSize)
    ctx.moveTo(Math.floor(pixel.x), Math.floor(pixel.y));
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

  private def drawHex(ctx : CanvasRenderingContext2D, hexLoc : HexLoc, color : String, scale : Double, doStroke: Boolean, doFill:Boolean, alpha: Double, lineWidth: Double) : Unit = {
    ctx.globalAlpha = alpha
    ctx.lineWidth = lineWidth
    ctx.strokeStyle = color
    ctx.fillStyle = color
    ctx.beginPath()
    move(ctx, hexLoc + HexVec.corners(0) * scale)
    line(ctx, hexLoc + HexVec.corners(1) * scale)
    line(ctx, hexLoc + HexVec.corners(2) * scale)
    line(ctx, hexLoc + HexVec.corners(3) * scale)
    line(ctx, hexLoc + HexVec.corners(4) * scale)
    line(ctx, hexLoc + HexVec.corners(5) * scale)
    line(ctx, hexLoc + HexVec.corners(0) * scale)
    if(doStroke) ctx.stroke()
    if(doFill)  ctx.fill()
    ctx.closePath()
  }
  private def strokeHex(ctx : CanvasRenderingContext2D, hexLoc : HexLoc, color : String, scale : Double, alpha: Double = 1.0, lineWidth: Double = 1.0) : Unit = {
    drawHex(ctx,hexLoc,color,scale,true,false,alpha,lineWidth);
  }
  private def fillHex(ctx : CanvasRenderingContext2D, hexLoc : HexLoc, color : String, scale : Double, alpha: Double = 0.2) : Unit = {
    drawHex(ctx,hexLoc,color,scale,false,true,alpha,1.0);
  }

  private def drawTile(ctx : CanvasRenderingContext2D, hexLoc : HexLoc, tile: Tile) : Unit = {
    tile.terrain match {
      case Wall => fillHex(ctx, hexLoc, "white", tileScale)
      case Ground => fillHex(ctx, hexLoc, "green", tileScale)
      case Water => fillHex(ctx, hexLoc, "blue", tileScale)
      case ManaSpire => fillHex(ctx, hexLoc, "orange", tileScale)
      case Spawner(S0, _) =>
        fillHex(ctx, hexLoc, "gray", tileScale)
        fillHex(ctx, hexLoc, "red", tileScale*0.7)
      case Spawner(S1, _) =>
        fillHex(ctx, hexLoc, "gray", tileScale)
        fillHex(ctx, hexLoc, "blue", tileScale*0.7)
    }
    text(ctx, Loc(hexLoc.x.round.toInt, hexLoc.y.round.toInt).toString, PixelLoc.ofHexLoc(hexLoc, gridSize)+PixelVec(0, -gridSize/2.0), "black")
  }

  private def drawPiece(ctx : CanvasRenderingContext2D, hexLoc : HexLoc, scale : Double, piece: Piece) : Unit = {
    val pieceColor =
      piece.side match {
        case S0 => "blue"
        case S1 => "red"
      }
    fillHex(ctx, hexLoc, pieceColor, scale)
    text(ctx, piece.id.toString, PixelLoc.ofHexLoc(hexLoc,gridSize)+PixelVec(0, gridSize/3.0), "black")
    text(ctx, piece.curStats.displayName, PixelLoc.ofHexLoc(hexLoc,gridSize), "black")
  }

  private def locAndScaleOfPiece(board: BoardState, piece: Piece) : (HexLoc,Double) = {
    val loc = piece.loc
    board.pieces(loc) match {
      case Nil => assertUnreachable()
      case _ :: Nil => (loc,pieceScale)
      case p1 :: _ :: Nil  =>
        if(piece.id == p1.id) (HexLoc.ofLoc(loc) + HexVec.corners(5) * smallPieceOffset, smallPieceScale)
        else                 (HexLoc.ofLoc(loc) + HexVec.corners(2) * smallPieceOffset, smallPieceScale)
      case p1 :: p2 :: _ :: Nil =>
        if(piece.id == p1.id)      (HexLoc.ofLoc(loc) + HexVec.corners(5) * smallPieceOffset, smallPieceScale)
        else if(piece.id == p2.id) (HexLoc.ofLoc(loc) + HexVec.corners(3) * smallPieceOffset, smallPieceScale)
        else                      (HexLoc.ofLoc(loc) + HexVec.corners(1) * smallPieceOffset, smallPieceScale)
      case _ => assertUnreachable()
    }
  }

  def drawEverything(
    canvas: Canvas,
    ctx: CanvasRenderingContext2D,
    board : BoardState,
    game: Game,
    translateOrigin: PixelVec,
    hoverLoc: Option[Loc],
    hoverSpec: Option[PieceSpec],
    selected: Option[PieceSpec],
    path : List[Loc]
  ) : Unit = {
    ctx.setTransform(1,0,0,1,0,0)
    ctx.clearRect(0.0, 0.0, canvas.width.toDouble, canvas.height.toDouble)
    ctx.translate(translateOrigin.dx,translateOrigin.dy)

    hoverLoc.foreach { hoverLoc => println(hoverLoc) }

    //Techs / Reinforcements
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
      fillHex(ctx, loc, color, tileScale)
      text(ctx, i.toString, PixelLoc.ofHexLoc(HexLoc.ofLoc(loc), gridSize), "black")
      text(ctx, s0.toString, PixelLoc.ofHexLoc(HexLoc.ofLoc(loc) + HexVec.corners(3) * pieceScale, gridSize), "blue")
      text(ctx, game.tech(S0)(i).toString, PixelLoc.ofHexLoc(HexLoc.ofLoc(loc) + HexVec.corners(4) * techScale, gridSize), "blue")
      text(ctx, s1.toString, PixelLoc.ofHexLoc(HexLoc.ofLoc(loc) + HexVec.corners(1) * pieceScale, gridSize), "red")
      text(ctx, game.tech(S1)(i).toString, PixelLoc.ofHexLoc(HexLoc.ofLoc(loc) + HexVec.corners(0) * techScale, gridSize), "red")
    }

    //Terrain
    board.tiles.foreachi {case (loc, tile) =>
      drawTile(ctx,loc,tile)
    }

    //Pieces
    board.pieces.foreach { pieces =>
      pieces.foreach { piece =>
        val (loc,scale) = locAndScaleOfPiece(board,piece)
        drawPiece(ctx, loc, scale, piece)
      }
    }

    val hoverPiece = hoverSpec.flatMap { spec => board.findPiece(spec) }
    val selectedPiece = selected.flatMap { spec => board.findPiece(spec) }


    //Highlighted movement paths
    if(path.length > 0) {
      ctx.globalAlpha = 1.0
      ctx.fillStyle = "black"
      ctx.setLineDash(scala.scalajs.js.Array(5.0, 10.0))
      ctx.beginPath()
      selectedPiece match {
        case None => sys.error("")
        case Some(piece) =>
          val (loc,_) = locAndScaleOfPiece(board,piece)
          move(ctx, loc)
      }
      for(i <- 1 to path.length-1) {
        line(ctx, path(i))
      }
      ctx.stroke()
      ctx.closePath()
      ctx.setLineDash(scala.scalajs.js.Array())
    }

    //Movement range
    selectedPiece match {
      case None => ()
      case Some(piece) =>
        for((loc, (_d, can_land)) <- board.legalMoves(piece, piece.loc)) {
          if(can_land) {
            fillHex(ctx, loc, "yellow", tileScale, 0.1)
          }
        }
    }

    //Mouse hover for hex
    hoverLoc.foreach { hoverLoc =>
      if(board.inBounds(hoverLoc) && board.tiles(hoverLoc).terrain != Wall)
        strokeHex(ctx, hoverLoc, "black", tileScale, alpha=0.3)
    }

    //Mouse hover for piece
    hoverPiece.foreach { piece =>
      val (loc,scale) = locAndScaleOfPiece(board,piece)
      strokeHex(ctx, loc, "black", scale)
    }

    //Piece selected by mouse click
    selectedPiece.foreach { piece =>
      val (loc,scale) = locAndScaleOfPiece(board,piece)
      strokeHex(ctx, loc, "purple", scale, lineWidth=2.5)
    }

  }
}

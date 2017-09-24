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
  val reinforcementScale = 23.0 / gridSize
  val techScale = 20.0 / gridSize
  val smallPieceScale = 14.0 / gridSize
  val smallPieceOffset = 15.0 / gridSize

  def drawEverything(
    canvas: Canvas,
    ctx: CanvasRenderingContext2D,
    board : BoardState,
    translateOrigin: PixelVec,
    mouseState: MouseState,
    flipDisplay: Boolean
  ) : Unit = {

    import scala.language.implicitConversions
    implicit def hexLocOfLoc(loc: Loc): HexLoc = {
      HexLoc.ofLoc(loc,flipDisplay,board)
    }

    def move(ctx : CanvasRenderingContext2D, hexLoc : HexLoc) : Unit = {
      val pixel = PixelLoc.ofHexLoc(hexLoc,gridSize)
      ctx.moveTo(Math.floor(pixel.x), Math.floor(pixel.y));
    }
    def line(ctx : CanvasRenderingContext2D, hexLoc : HexLoc) : Unit = {
      val pixel = PixelLoc.ofHexLoc(hexLoc,gridSize)
      ctx.lineTo(Math.floor(pixel.x), Math.floor(pixel.y));
    }
    def text(ctx : CanvasRenderingContext2D, text : String, pixel : PixelLoc, color : String) : Unit = {
      ctx.globalAlpha = 1.0
      ctx.fillStyle = color
      ctx.textAlign = "center"
      ctx.fillText(text, Math.floor(pixel.x), Math.floor(pixel.y))
    }

    def hexCorner(hexLoc : HexLoc, scale : Double, corner : Int) : HexLoc = {
      hexLoc + HexVec.corners(corner) * scale
    }

    def drawHex(ctx : CanvasRenderingContext2D, hexLoc : HexLoc, color : String, scale : Double, doStroke: Boolean, doFill:Boolean, alpha: Double, lineWidth: Double) : Unit = {
      ctx.globalAlpha = alpha
      ctx.lineWidth = lineWidth
      ctx.strokeStyle = color
      ctx.fillStyle = color
      ctx.beginPath()
      move(ctx, hexCorner(hexLoc,scale,0))
      line(ctx, hexCorner(hexLoc,scale,1))
      line(ctx, hexCorner(hexLoc,scale,2))
      line(ctx, hexCorner(hexLoc,scale,3))
      line(ctx, hexCorner(hexLoc,scale,4))
      line(ctx, hexCorner(hexLoc,scale,5))
      line(ctx, hexCorner(hexLoc,scale,0))
      if(doStroke) ctx.stroke()
      if(doFill)  ctx.fill()
      ctx.closePath()
    }
    def strokeHex(ctx : CanvasRenderingContext2D, hexLoc : HexLoc, color : String, scale : Double, alpha: Double = 1.0, lineWidth: Double = 1.0) : Unit = {
      drawHex(ctx,hexLoc,color,scale,true,false,alpha,lineWidth);
    }
    def fillHex(ctx : CanvasRenderingContext2D, hexLoc : HexLoc, color : String, scale : Double, alpha: Double = 0.2) : Unit = {
      drawHex(ctx,hexLoc,color,scale,false,true,alpha,1.0);
    }

    def drawTile(ctx : CanvasRenderingContext2D, hexLoc : HexLoc, tile: Tile) : Unit = {
      tile.terrain match {
        case Wall => fillHex(ctx, hexLoc, "white", tileScale)
        case Ground => fillHex(ctx, hexLoc, "green", tileScale)
        case Water => fillHex(ctx, hexLoc, "blue", tileScale)
        case Graveyard => fillHex(ctx, hexLoc, "orange", tileScale)
        case Spawner(S0, _) =>
          fillHex(ctx, hexLoc, "gray", tileScale)
          fillHex(ctx, hexLoc, "red", tileScale*0.7)
        case Spawner(S1, _) =>
          fillHex(ctx, hexLoc, "gray", tileScale)
          fillHex(ctx, hexLoc, "blue", tileScale*0.7)
      }
      val (loc,_) = hexLoc.round(flipDisplay,board)
      text(ctx, loc.toString, PixelLoc.ofHexLoc(hexLoc, gridSize)+PixelVec(0, -gridSize/2.0), "black")
    }

    def drawPiece(ctx : CanvasRenderingContext2D, hexLoc : HexLoc, scale : Double, pieceName: PieceName, side: Side) : Unit = {
      val pieceColor =
        side match {
          case S0 => "blue"
          case S1 => "red"
        }
      val _ = pieceName
      fillHex(ctx, hexLoc, pieceColor, scale)
    }

    def locAndScaleOfPiece(board: BoardState, piece: Piece) : (HexLoc,Double) = {
      val loc = piece.loc
      board.pieces(loc) match {
        case Nil => assertUnreachable()
        case _ :: Nil => (loc,pieceScale)
        case p1 :: _ :: Nil  =>
          val hexLoc = hexLocOfLoc(loc)
          if(piece.id == p1.id) (hexCorner(hexLoc,smallPieceOffset,5), smallPieceScale)
          else                 (hexCorner(hexLoc,smallPieceOffset,2), smallPieceScale)
        case p1 :: p2 :: _ :: Nil =>
          val hexLoc = hexLocOfLoc(loc)
          if(piece.id == p1.id)      (hexCorner(hexLoc,smallPieceOffset,5), smallPieceScale)
          else if(piece.id == p2.id) (hexCorner(hexLoc,smallPieceOffset,3), smallPieceScale)
          else                       (hexCorner(hexLoc,smallPieceOffset,1), smallPieceScale)
        case _ => assertUnreachable()
      }
    }

    def locsOfReinforcement(loc: Loc, count: Int): Array[HexLoc] = {
      val hexLoc = hexLocOfLoc(loc)
      val stackSpacingHeight = 0.10 / Math.sqrt(count.toDouble)
      val offsetVec = HexVec(0.5,-1.0)
      val result = (0 until count).map { i =>
        hexLoc + offsetVec * (i * stackSpacingHeight - 0.05)
      }.toArray
      result
    }

    ctx.setTransform(1,0,0,1,0,0)
    ctx.clearRect(0.0, 0.0, canvas.width.toDouble, canvas.height.toDouble)
    ctx.translate(translateOrigin.dx,translateOrigin.dy)

    //Reinforcements
    Side.foreach { side =>
      val locsAndContents = Reinforcements.getLocsAndContents(side,flipDisplay,board)
      locsAndContents.foreach { case (loc,pieceName,count) =>
        val locs = locsOfReinforcement(loc,count)
        locs.foreach { hexLoc =>
          drawPiece(ctx, hexLoc, reinforcementScale, pieceName, side)
        }
      }
    }

    // //Techs / Reinforcements
    // for((pieceStats, i) <- Units.techs.view.zipWithIndex) {
    //   val s0 = board.reinforcements(S0).filter(x => x == pieceStats).length
    //   val s1 = board.reinforcements(S1).filter(x => x == pieceStats).length
    //   val loc = Loc((i/2)+2, if(i%2==0) -3 else -2)
    //   val color =
    //     (game.tech(S0)(i), game.tech(S1)(i)) match {
    //       case (T0, T0) => "#000000"
    //       case (T0, T1) => "#770000"
    //       case (T0, T2) => "#ff0000"
    //       case (T1, T0) => "#000077"
    //       case (T1, T1) => "#770077"
    //       case (T1, T2) => "#ff0077"
    //       case (T2, T0) => "#0000ff"
    //       case (T2, T1) => "#7700ff"
    //       case (T2, T2) => "#ff00ff"
    //     }
    //   fillHex(ctx, loc, color, tileScale)
    //   val hexLoc = HexLoc.ofLoc(loc)
    //   text(ctx, i.toString, PixelLoc.ofHexLoc(hexLoc, gridSize), "black")
    //   text(ctx, s0.toString, PixelLoc.ofHexLoc(hexCorner(hexLoc,pieceScale,3), gridSize), "blue")
    //   text(ctx, game.tech(S0)(i).toString, PixelLoc.ofHexLoc(hexCorner(hexLoc,techScale,4), gridSize), "blue")
    //   text(ctx, s1.toString, PixelLoc.ofHexLoc(hexCorner(hexLoc,pieceScale,1), gridSize), "red")
    //   text(ctx, game.tech(S1)(i).toString, PixelLoc.ofHexLoc(hexCorner(hexLoc,techScale,0), gridSize), "red")
    // }

    //Terrain
    board.tiles.foreachi {case (loc, tile) =>
      drawTile(ctx,loc,tile)
    }

    //Pieces
    board.pieces.foreach { pieces =>
      pieces.foreach { piece =>
        val (loc,scale) = locAndScaleOfPiece(board,piece)
        drawPiece(ctx, loc, scale, piece.baseStats.name, piece.side)
      }
    }

    val selectedPiece = mouseState.selected.flatMap { target => target.findPiece(board) }

    //Highlighted movement paths
    if(mouseState.path.length > 0) {
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
      for(i <- 1 to mouseState.path.length-1) {
        line(ctx, mouseState.path(i))
      }
      ctx.stroke()
      ctx.closePath()
      ctx.setLineDash(scala.scalajs.js.Array())
    }

    //Movement range
    selectedPiece match {
      case None => ()
      case Some(piece) =>
        val moves = board.legalMoves(piece)
        for((loc,_) <- moves) {
          fillHex(ctx, loc, "yellow", tileScale, 0.1)
        }
    }

    //Mouse hover
    mouseState.hovered.foreach { mouseTarget =>
      //Highlight hex tile on mouse hover
      mouseState.hoverLoc.foreach { hoverLoc =>
        strokeHex(ctx, hoverLoc, "black", tileScale, alpha=0.3)
      }

      //Highlight mouse's target on mouse hover
      mouseTarget match {
        case MouseTile(_) => ()
        case MouseReinforcement(pieceName,side) =>
          Reinforcements.getSelectedLocAndCount(side, flipDisplay, board, pieceName) match {
            case None => ()
            case Some((loc,count)) =>
              val locs = locsOfReinforcement(loc,count)
              strokeHex(ctx,locs.last, "black", reinforcementScale)
          }
        case MousePiece(spec) =>
          board.findPiece(spec) match {
            case None => ()
            case Some(piece) =>
              val (loc,scale) = locAndScaleOfPiece(board,piece)
              strokeHex(ctx, loc, "black", scale)
          }
      }
    }

    //Piece selected by mouse click
    mouseState.selected.foreach { mouseTarget =>
      mouseTarget match {
        case MouseTile(_) => ()
        case MouseReinforcement(pieceName,side) =>
          Reinforcements.getSelectedLocAndCount(side, flipDisplay, board, pieceName) match {
            case None => ()
            case Some((loc,count)) =>
              val locs = locsOfReinforcement(loc,count)
              strokeHex(ctx,locs.last, "black", reinforcementScale)
          }
        case MousePiece(spec) =>
          board.findPiece(spec) match {
            case None => ()
            case Some(piece) =>
              val (loc,scale) = locAndScaleOfPiece(board,piece)
              strokeHex(ctx, loc, "black", scale)
          }
      }
    }
  }

}

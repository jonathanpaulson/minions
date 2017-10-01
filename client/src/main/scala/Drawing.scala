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
  val techScale = 23.0 / gridSize
  val smallPieceScale = 14.0 / gridSize
  val smallPieceOffset = 15.0 / gridSize

  def drawEverything(
    canvas: Canvas,
    ctx: CanvasRenderingContext2D,
    game: Game,
    boards: Array[BoardState],
    boardIdx: Int,
    mouseState: MouseState,
    flipDisplay: Boolean
  ) : Unit = {

    val board = boards(boardIdx)

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
    def text(
      ctx : CanvasRenderingContext2D,
      text : String,
      pixel : PixelLoc,
      color : String,
      textAlign : String = "center"
    ) : Unit
    = {
      ctx.globalAlpha = 1.0
      ctx.fillStyle = color
      ctx.textAlign = textAlign
      ctx.fillText(text, Math.floor(pixel.x), Math.floor(pixel.y))
    }

    def hexCorner(hexLoc : HexLoc, scale : Double, corner : Int) : HexLoc = {
      hexLoc + HexVec.corners(corner) * scale
    }

    def drawHex(ctx : CanvasRenderingContext2D, hexLoc : HexLoc, color : String, scale : Double, doStroke: Boolean, doFill:Boolean, alpha: Double, lineWidth: Double) : Unit = {
      val oldAlpha = ctx.globalAlpha
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
      ctx.globalAlpha = oldAlpha
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
        case Ground | StartHex(_) => fillHex(ctx, hexLoc, "green", tileScale)
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

    def drawPiece(ctx : CanvasRenderingContext2D, hexLoc : HexLoc, scale : Double,
      pieceName: PieceName, side: Side, id: Option[Int], count:Option[Int] ) : Unit = {
      val pieceColor =
        side match {
          case S0 => "blue"
          case S1 => "red"
        }
      fillHex(ctx, hexLoc, pieceColor, scale)
      id.foreach { id =>
        text(ctx, id.toString, PixelLoc.ofHexLoc(hexLoc,gridSize)+PixelVec(0, gridSize/3.0), "black")
      }
      val stats = Units.pieceMap(pieceName)
      val displayName = count match {
        case None => stats.displayName
        case Some(count) => stats.displayName + " x " + count
      }
      text(ctx, displayName, PixelLoc.ofHexLoc(hexLoc,gridSize), "black")
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

    // def locsOfReinforcement(loc: Loc, count: Int): Array[HexLoc] = {
    //   val hexLoc = hexLocOfLoc(loc)
    //   val stackSpacingHeight = 0.10 / Math.sqrt(count.toDouble)
    //   val offsetVec = HexVec(0.5,-1.0)
    //   val result = (0 until count).map { i =>
    //     hexLoc + offsetVec * (i * stackSpacingHeight - 0.05)
    //   }.toArray
    //   result
    // }

    ctx.setTransform(1,0,0,1,0,0)
    ctx.clearRect(0.0, 0.0, canvas.width.toDouble, canvas.height.toDouble)

    //Background fill based on whose turn it is
    val backgroundColor = board.side match {
      case S0 => "#f3f3ff"
      case S1 => "#fff3f3"
    }
    ctx.fillStyle = backgroundColor
    ctx.globalAlpha = 1.0
    ctx.fillRect(0.0, 0.0, canvas.width.toDouble, canvas.height.toDouble)

    ctx.translate(UI.translateOrigin.dx,UI.translateOrigin.dy)

    //Game info text
    Side.foreach { side =>
      val infoLoc = UI.Info.getLoc(side,flipDisplay,board)
      val pixelLoc = PixelLoc.ofHexLoc(hexLocOfLoc(infoLoc), gridSize)
      val color = side match {
        case S0 => "#000099"
        case S1 => "#770000"
      }
      val mana = game.mana(side) + boards.foldLeft(0) { case (sum,board) =>
        sum + board.manaThisRound(side)
      }

      def textAtLoc(s: String, dpx: Double, dpy: Double) =
        text(ctx, s, pixelLoc+PixelVec(dpx,dpy), color, textAlign="left")

      if(side == board.side)
        textAtLoc(side.toColorName + " Team's Turn!", 0.0, -9.0)
      textAtLoc(side.toColorName + " Team Mana: " + mana, 0.0, 3.0)
    }

    //End turn hex
    if(game.isBoardDone(boardIdx)) {
      fillHex(ctx, UI.EndTurn.loc, "#ff99ff", tileScale, alpha=1.0)
      strokeHex(ctx, UI.EndTurn.loc, "#ff00ff", tileScale, lineWidth=2.0)
    }
    else {
      fillHex(ctx, UI.EndTurn.loc, "gray", tileScale)
    }
    text(ctx, "End Turn", PixelLoc.ofHexLoc(hexLocOfLoc(UI.EndTurn.loc), gridSize), "black")

    //Reinforcements
    Side.foreach { side =>
      val locsAndContents = UI.Reinforcements.getLocsAndContents(side,flipDisplay,board)
      locsAndContents.foreach { case (loc,pieceName,count) =>
        drawPiece(ctx, hexLocOfLoc(loc), reinforcementScale, pieceName, side, None, Some(count))
      }
    }

    //Techs
    val techLocs = UI.Tech.getLocs(game)
    for(i <- 0 until game.techLine.length) {
      val techState = game.techLine(i)
      val loc = techLocs(i)
      val fillColor =
        (techState.level(S0), techState.level(S1)) match {
          case (TechLocked, TechLocked) => "#888888"
          case (TechLocked, (TechUnlocked | TechAcquired)) => "#ffbbbb"
          case ((TechUnlocked | TechAcquired), TechLocked) => "#bbbbff"
          case ((TechUnlocked | TechAcquired), (TechUnlocked | TechAcquired)) => "#ff99ff"
        }
      val strokeColor =
        (techState.level(S0), techState.level(S1)) match {
          case ((TechLocked | TechUnlocked), (TechLocked | TechUnlocked)) => None
          case ((TechLocked | TechUnlocked), TechAcquired) => Some("#ff3333")
          case (TechAcquired, (TechLocked | TechUnlocked)) => Some("#33333ff")
          case (TechAcquired, TechAcquired) => Some("#ff00ff")
        }

      fillHex(ctx, loc, fillColor, tileScale, alpha=1.0)
      strokeColor.foreach { color =>
        strokeHex(ctx, loc, color, tileScale, lineWidth=2.0)
      }

      val hexLoc = hexLocOfLoc(loc)
      text(ctx, techState.displayName, PixelLoc.ofHexLoc(hexLoc, gridSize), "black")
      text(ctx, techState.level(S0).toUnicodeSymbol, PixelLoc.ofHexLoc(hexLoc + HexVec.corners(4) * techScale, gridSize), "blue")
      text(ctx, techState.level(S1).toUnicodeSymbol, PixelLoc.ofHexLoc(hexLoc + HexVec.corners(0) * techScale, gridSize), "red")
    }

    //Terrain
    board.tiles.foreachi {case (loc, tile) =>
      drawTile(ctx,loc,tile)
    }

    //Pieces
    board.pieces.foreach { pieces =>
      pieces.foreach { piece =>
        val (loc,scale) = locAndScaleOfPiece(board,piece)
        drawPiece(ctx, loc, scale, piece.baseStats.name, piece.side, Some(piece.id), None)
      }
    }

    val dragPiece = mouseState.dragTarget.findPiece(board)

    //Draw line for movement paths
    mouseState.mode match {
      case (_: SelectTargetMouseMode) =>
        //TODO highlight legal targets
      case (mode: NormalMouseMode) =>
        val path = mode.path
        if(path.length > 0) {
          ctx.globalAlpha = 1.0
          ctx.fillStyle = "black"
          ctx.setLineDash(scala.scalajs.js.Array(5.0, 10.0))
          ctx.beginPath()
          dragPiece match {
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
    }

    //Highlight movement range
    dragPiece match {
      case None => ()
      case Some(piece) =>
        val moves = board.legalMoves(piece)
        for((loc,_) <- moves) {
          fillHex(ctx, loc, "yellow", tileScale, alpha=0.1)
        }
    }

    //Highlight hex tile on mouse hover
    mouseState.hoverLoc.foreach { hoverLoc =>
      strokeHex(ctx, hoverLoc, "black", tileScale, alpha=0.3)
    }

    //Highlight mouse's target on mouse hover
    mouseState.hovered match {
      case MouseNone => ()
      case MouseTile(_) => ()
      case MouseEndTurn =>
        val loc = UI.EndTurn.loc
        strokeHex(ctx,loc, "black", tileScale)
      case MouseTech(techIdx) =>
        val loc = UI.Tech.getLoc(techIdx)
        strokeHex(ctx,loc, "black", tileScale)
      case MouseReinforcement(pieceName,side) =>
        UI.Reinforcements.getSelectedLocAndCount(side, flipDisplay, board, pieceName) match {
          case None => ()
          case Some((loc,_)) =>
            strokeHex(ctx,hexLocOfLoc(loc), "black", reinforcementScale)
        }
      case MousePiece(spec) =>
        board.findPiece(spec) match {
          case None => ()
          case Some(piece) =>
            val (loc,scale) = locAndScaleOfPiece(board,piece)
            strokeHex(ctx, loc, "black", scale)
        }
    }

    //Piece selected by mouse click
    mouseState.dragTarget match {
      case MouseNone => ()
      case MouseTile(_) => ()
      case MouseEndTurn =>
        val loc = UI.EndTurn.loc
        fillHex(ctx, loc, "yellow", tileScale, alpha=0.1)
        strokeHex(ctx,loc, "black", tileScale)
      case MouseTech(techIdx) =>
        val loc = UI.Tech.getLoc(techIdx)
        fillHex(ctx, loc, "yellow", tileScale, alpha=0.1)
        strokeHex(ctx,loc, "black", tileScale)
      case MouseReinforcement(pieceName,side) =>
        UI.Reinforcements.getSelectedLocAndCount(side, flipDisplay, board, pieceName) match {
          case None => ()
          case Some((loc,_)) =>
            fillHex(ctx, hexLocOfLoc(loc), "yellow", reinforcementScale, alpha=0.1)
            strokeHex(ctx, hexLocOfLoc(loc), "black", reinforcementScale)
        }
      case MousePiece(spec) =>
        board.findPiece(spec) match {
          case None => ()
          case Some(piece) =>
            val (loc,scale) = locAndScaleOfPiece(board,piece)
            //Only stroke and no highlight since we're already getting a highlight from drawing paths.
            strokeHex(ctx, loc, "black", scale)
        }
    }
  }
}

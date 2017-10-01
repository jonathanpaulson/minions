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
  val techScale = 23.0 / gridSize
  val smallPieceScale = 14.0 / gridSize
  val smallPieceOffset = 15.0 / gridSize

  def drawEverything(
    canvas: Canvas,
    ctx: CanvasRenderingContext2D,
    game: Game,
    boards: Array[Board],
    boardIdx: Int,
    mouseState: MouseState,
    flipDisplay: Boolean,
    ctrlPressed: Boolean,
    showCoords: Boolean
  ) : Unit = {

    val board = boards(boardIdx).curState

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
        case Graveyard =>
          fillHex(ctx, hexLoc, "#aa8899", tileScale)
          strokeHex(ctx, hexLoc, "#aa8899", tileScale, alpha=0.4)
        case Spawner(S0, _) =>
          fillHex(ctx, hexLoc, "gray", tileScale)
          fillHex(ctx, hexLoc, "red", tileScale*0.7)
        case Spawner(S1, _) =>
          fillHex(ctx, hexLoc, "gray", tileScale)
          fillHex(ctx, hexLoc, "blue", tileScale*0.7)
      }
      if(showCoords) {
        val (loc,_) = hexLoc.round(flipDisplay,board)
        text(ctx, loc.toString, PixelLoc.ofHexLoc(hexLoc, gridSize)+PixelVec(0, -gridSize/2.0), "black")
      }
    }

    def displayNameOfPieceName(pieceName: PieceName): String = {
      Units.pieceMap(pieceName).displayName
    }

    def drawPiece(ctx : CanvasRenderingContext2D, hexLoc : HexLoc, scale : Double, side: Side, label: String) : Unit = {
      val pieceColor =
        side match {
          case S0 => "blue"
          case S1 => "red"
        }
      fillHex(ctx, hexLoc, pieceColor, scale)
      text(ctx, label, PixelLoc.ofHexLoc(hexLoc,gridSize), "black")
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
        sum + board.curState.manaThisRound(side)
      }

      def textAtLoc(s: String, dpx: Double, dpy: Double) =
        text(ctx, s, pixelLoc+PixelVec(dpx,dpy), color, textAlign="left")

      if(game.winner.nonEmpty) {
        if(game.winner == Some(side))
          textAtLoc(side.toColorName + " Team wins the game!", 0.0, -9.0)
      }
      else {
        if(side == board.side)
          textAtLoc(side.toColorName + " Team's Turn!", 0.0, -9.0)
      }

      textAtLoc(side.toColorName + " Team Mana: " + mana, 0.0, 3.0)

      textAtLoc(side.toColorName + " Team Wins: " + game.wins(side) + "/" + game.targetNumWins, 120.0, 3.0)

      if(side == board.side)
        textAtLoc("SorceryPower: " + board.sorceryPower, 240.0, 3.0)
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

    //Resign board hex
    fillHex(ctx, UI.ResignBoard.loc, "gray", tileScale)
    text(ctx, "Resign Board", PixelLoc.ofHexLoc(hexLocOfLoc(UI.ResignBoard.loc), gridSize), "black")

    //Reinforcements
    Side.foreach { side =>
      val locsAndContents = UI.Reinforcements.getLocsAndContents(side,flipDisplay,board)
      locsAndContents.foreach { case (loc,pieceName,count) =>
        val label = displayNameOfPieceName(pieceName) + " x " + count
        drawPiece(ctx, hexLocOfLoc(loc), pieceScale, side, label)
      }
    }

    //Dead pieces
    {
      val locsAndContents = UI.DeadPieces.getLocsAndContents(board)
      if(locsAndContents.length > 0) {
        text(ctx, "Dead pieces", PixelLoc.ofHexLoc(hexLocOfLoc(UI.DeadPieces.getDescLoc(board)), gridSize), "black")
      }
      locsAndContents.foreach { case (loc,_,pieceName,side) =>
        val label = displayNameOfPieceName(pieceName)
        drawPiece(ctx, hexLocOfLoc(loc), pieceScale, side, label)
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
          case (TechAcquired, (TechLocked | TechUnlocked)) => Some("#3333ff")
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

    def pieceCanStillDoThings(piece: Piece): Boolean = {
      piece.actState match {
        case DoneActing => false
        case Attacking(numAttacks) => numAttacks < piece.curStats(board).numAttacks
        case Moving(stepsUsed) => !(piece.curStats(board).isLumbering && stepsUsed > 0)
      }
    }
    def pieceHasNotDoneThings(piece: Piece): Boolean = {
      piece.actState match {
        case DoneActing | Attacking(_) => false
        case Moving(stepsUsed) => stepsUsed == 0
      }
    }

    //Pieces
    board.pieces.foreach { pieces =>
      pieces.foreach { piece =>
        val (loc,scale) = locAndScaleOfPiece(board,piece)
        val label = piece.baseStats.displayName
        drawPiece(ctx, loc, scale, piece.side, label)
        if(piece.side != game.curSide) {
          if(piece.damage > 0)
            strokeHex(ctx, loc, "magenta", scale)
        }
        else {
          if(pieceHasNotDoneThings(piece))
            strokeHex(ctx, loc, "green", scale, lineWidth=1.5)
          else if(pieceCanStillDoThings(piece))
            strokeHex(ctx, loc, "orange", scale, lineWidth=1.5)
        }
      }
    }

    def drawPath(path: Vector[Loc], overrideStart: Option[HexLoc] = None): Unit = {
      if(path.length > 0) {
        ctx.globalAlpha = 1.0
        ctx.fillStyle = "black"
        ctx.setLineDash(scala.scalajs.js.Array(5.0, 10.0))
        ctx.beginPath()
        move(ctx, overrideStart.getOrElse(path(0)))
        for(i <- 1 to path.length-1) {
          line(ctx, path(i))
        }
        ctx.stroke()
        ctx.closePath()
        ctx.setLineDash(scala.scalajs.js.Array())
      }
    }

    def findLocOfPiece(pieceSpec: PieceSpec): Option[Loc] = {
      //First try the initial board
      board.findPiece(pieceSpec) match {
        case Some(piece) => Some(piece.loc)
        case None =>
          //Then try the start of turn board
          boards(boardIdx).initialStateThisTurn.findPiece(pieceSpec).map { piece => piece.loc }
      }
    }

    def highlightUndoneAction(action: PlayerAction): Unit = {
      action match {
        case Movements(movements) =>
          movements.foreach { case Movement(spec,path) =>
            drawPath(path)
            findLocOfPiece(spec).foreach { loc =>
              strokeHex(ctx,hexLocOfLoc(loc), "black", tileScale)
            }
          }
        case Attack(aSpec,tSpec) =>
          findLocOfPiece(aSpec).foreach { loc =>
            strokeHex(ctx,hexLocOfLoc(loc), "black", tileScale)
          }
          findLocOfPiece(tSpec).foreach { loc =>
            strokeHex(ctx,hexLocOfLoc(loc), "magenta", tileScale)
          }
        case Spawn(_,_) =>
          UI.Reinforcements.getLocs(board.side, flipDisplay, board).foreach { loc =>
            strokeHex(ctx, hexLocOfLoc(loc), "black", tileScale)
          }
        case ActivateAbility(spec,_,targets) =>
          List(spec,targets.target0,targets.target1).foreach { spec =>
            findLocOfPiece(spec).foreach { loc =>
              strokeHex(ctx, hexLocOfLoc(loc), "black", tileScale)
            }
          }
        case PlaySpell(_,targets) =>
          List(targets.target0,targets.target1).foreach { spec =>
            findLocOfPiece(spec).foreach { loc =>
              strokeHex(ctx, hexLocOfLoc(loc), "black", tileScale)
            }
          }
        case DiscardSpell(_) =>
          ()
      }
    }

    def highlightUndoneActionsForPieceSpec(pieceSpec: PieceSpec): Unit = {
      boards(boardIdx).findLocalPieceUndoActions(pieceSpec) match {
        case None => ()
        case Some(actions) => actions.foreach(highlightUndoneAction)
      }
    }

    def canClickOnTech(techIdx: Int): Boolean = {
      game.tryIsLegal(PerformTech(game.curSide,techIdx)).isSuccess ||
      game.techLine(techIdx).startingLevelThisTurn(game.curSide) == TechAcquired
    }

    mouseState.mode match {
      //TODO highlight legal targets
      case (_: SelectTargetMouseMode) => ()

      case (mode: NormalMouseMode) =>
        //Highlight mouse's target on mouse hover
        mouseState.hovered match {
          case MouseNone => ()
          case MouseTile(_) => ()
          case MouseEndTurn =>
            val loc = UI.EndTurn.loc
            strokeHex(ctx,loc, "black", tileScale, alpha=0.5)
          case MouseResignBoard =>
            val loc = UI.ResignBoard.loc
            strokeHex(ctx,loc, "black", tileScale, alpha=0.5)
          case MouseTech(techIdx) =>
            if(canClickOnTech(techIdx)) {
              val loc = UI.Tech.getLoc(techIdx)
              strokeHex(ctx,loc, "black", tileScale, alpha=0.5)
            }
          case MouseReinforcement(pieceName,side) =>
            UI.Reinforcements.getSelectedLocAndCount(side, flipDisplay, board, pieceName) match {
              case None => ()
              case Some((loc,_)) =>
                strokeHex(ctx,hexLocOfLoc(loc), "black", pieceScale, alpha=0.5)
            }
            if(ctrlPressed) {
              val pieceSpec = board.unsummonedThisTurn.reverse.findMap { case (pieceSpec,name,_) =>
                if(pieceName == name) Some(pieceSpec) else None
              }
              pieceSpec.foreach { pieceSpec => highlightUndoneActionsForPieceSpec(pieceSpec) }
            }

          case MouseDeadPiece(pieceSpec) =>
            UI.DeadPieces.getSelectedLoc(board, pieceSpec) match {
              case None => ()
              case Some(loc) =>
                strokeHex(ctx, hexLocOfLoc(loc), "black", pieceScale, alpha=0.5)
            }
            if(ctrlPressed)
              highlightUndoneActionsForPieceSpec(pieceSpec)
          case MousePiece(spec) =>
            board.findPiece(spec) match {
              case None => ()
              case Some(piece) =>
                val (loc,scale) = locAndScaleOfPiece(board,piece)
                strokeHex(ctx, loc, "black", scale, alpha=0.5)
            }

            if(ctrlPressed)
              highlightUndoneActionsForPieceSpec(spec)
        }

        //Draw highlights based on piece selected by mouse click
        mouseState.dragTarget match {
          case MouseNone => ()
          case MouseTile(_) => ()
          case MouseEndTurn =>
            val loc = UI.EndTurn.loc
            fillHex(ctx, loc, "yellow", tileScale, alpha=0.1)
            strokeHex(ctx,loc, "black", tileScale, alpha=0.5)
          case MouseResignBoard =>
            val loc = UI.ResignBoard.loc
            fillHex(ctx, loc, "yellow", tileScale, alpha=0.1)
            strokeHex(ctx,loc, "black", tileScale, alpha=0.5)
          case MouseTech(techIdx) =>
            if(canClickOnTech(techIdx)) {
              val loc = UI.Tech.getLoc(techIdx)
              fillHex(ctx, loc, "yellow", tileScale, alpha=0.1)
              strokeHex(ctx,loc, "black", tileScale, alpha=0.5)
            }
          case MouseReinforcement(pieceName,side) =>
            UI.Reinforcements.getSelectedLocAndCount(side, flipDisplay, board, pieceName) match {
              case None => ()
              case Some((loc,_)) =>
                fillHex(ctx, hexLocOfLoc(loc), "yellow", pieceScale, alpha=0.1)
                strokeHex(ctx, hexLocOfLoc(loc), "black", pieceScale, alpha=0.5)
            }
            val locs = board.legalSpawnLocs(pieceName)
            for(loc <- locs) {
              fillHex(ctx, loc, "yellow", tileScale, alpha=0.1)
            }

          case MouseDeadPiece(pieceSpec) =>
            if(ctrlPressed) {
              UI.DeadPieces.getSelectedLoc(board, pieceSpec) match {
                case None => ()
                case Some(loc) =>
                  fillHex(ctx, hexLocOfLoc(loc), "yellow", pieceScale, alpha=0.1)
                  strokeHex(ctx, hexLocOfLoc(loc), "black", pieceScale, alpha=0.5)
              }
            }
          case MousePiece(spec) =>
            board.findPiece(spec) match {
              case None => ()
              case Some(piece) =>
                val (loc,scale) = locAndScaleOfPiece(board,piece)

                if(ctrlPressed) {
                  fillHex(ctx, loc, "yellow", scale, alpha=0.1)
                  strokeHex(ctx, loc, "black", scale, alpha=0.5)
                }
                else {
                  if(piece.side == game.curSide) {
                    //Only stroke and no highlight since we're already getting a highlight from drawing paths.
                    strokeHex(ctx, loc, "black", scale, alpha=0.5)

                    //Draw the movement path
                    val path = mode.path
                    drawPath(path.toVector,overrideStart = Some(loc))

                    //Highlight the movement range
                    val moves = board.legalMoves(piece)
                    for((loc,_) <- moves) {
                      fillHex(ctx, loc, "yellow", tileScale, alpha=0.1)
                    }

                    //Indicate attacking a piece if legal
                    mouseState.hovered.findPiece(board) match {
                      case None => ()
                      case Some(targetPiece) =>
                        val attackerStats = piece.curStats(board)
                        val targetStats = targetPiece.curStats(board)
                        val attackerHasMoved = piece.hasMoved || path.length > 1
                        if(targetPiece.side != piece.side &&
                          board.canAttack(attackerStats,attackerHasMoved,piece.actState,targetStats)) {
                          val (targetLoc,targetScale) = locAndScaleOfPiece(board,targetPiece)
                          fillHex(ctx, targetLoc, "magenta", targetScale, alpha=0.1)
                          strokeHex(ctx, targetLoc, "magenta", targetScale, alpha=0.5)
                        }
                    }
                  }
                }
            }
        }
    }

    //Highlight hex tile on mouse hover
    mouseState.hoverLoc.foreach { hoverLoc =>
      strokeHex(ctx, hoverLoc, "black", tileScale, alpha=0.3)
    }

  }
}

package minionsgame.jsclient

import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html.Canvas
import org.scalajs.dom.raw.HTMLImageElement

import minionsgame.core._
import RichImplicits._

object Drawing {
  //The euclidean distance from the center of a hexagon to the corner in pixels.
  val gridSize = 31.0
  val tileScale = 30.5 / gridSize
  val pieceScale = 25.0 / gridSize
  val techScale = 30.0 / gridSize
  val techInteriorScale = 19.0 / gridSize
  val smallPieceScale = 14.0 / gridSize
  val smallPieceOffset = 15.0 / gridSize

  def loadImage(src: String): HTMLImageElement = {
    val image = org.scalajs.dom.document.createElement("img").asInstanceOf[HTMLImageElement]
    image.src = src
    image
  }
  val textures: Map[String,HTMLImageElement] = Map(
    "img_terrain_grass0" -> loadImage("img/terrain/grass0.png"),
    "img_terrain_grass1" -> loadImage("img/terrain/grass1.png"),
    "img_terrain_grass2" -> loadImage("img/terrain/grass2.png"),
    "img_terrain_grass3" -> loadImage("img/terrain/grass3.png"),
    "img_terrain_sand0" -> loadImage("img/terrain/sand0.png"),
    "img_terrain_dirt0" -> loadImage("img/terrain/dirt0.png"),
    "img_terrain_dirt1" -> loadImage("img/terrain/dirt1.png"),
    "img_terrain_water0" -> loadImage("img/terrain/water0.png"),
    "img_terrain_water1" -> loadImage("img/terrain/water1.png"),
    "img_terrain_graveyard0" -> loadImage("img/terrain/graveyard0.png"),
    "img_terrain_graveyard1" -> loadImage("img/terrain/graveyard1.png"),
    "img_terrain_graveyard2" -> loadImage("img/terrain/graveyard2.png"),
    "img_terrain_graveyard3" -> loadImage("img/terrain/graveyard3.png"),
    "img_terrain_teleporter" -> loadImage("img/terrain/teleporter.png"),
    "img_terrain_spawner" -> loadImage("img/terrain/spawner.png"),
    "img_terrain_leyline" -> loadImage("img/terrain/leyline.png"),
  )

  def drawEverything(
    canvas: Canvas,
    ctx: CanvasRenderingContext2D,
    game: Game,
    boards: Array[Board],
    boardNames: Array[String],
    boardIdx: Int,
    mouseState: MouseState,
    flipDisplay: Boolean,
    undoing: Boolean,
    showCoords: Boolean,
    timeLeft: Option[Double]
  ) : Unit = {

    val board = boards(boardIdx).curState

    import scala.language.implicitConversions
    implicit def hexLocOfLoc(loc: Loc): HexLoc = {
      HexLoc.ofLoc(loc,flipDisplay,board)
    }

    def move(hexLoc : HexLoc) : Unit = {
      val pixel = PixelLoc.ofHexLoc(hexLoc,gridSize)
      ctx.moveTo(pixel.x, pixel.y);
    }
    def line(hexLoc : HexLoc) : Unit = {
      val pixel = PixelLoc.ofHexLoc(hexLoc,gridSize)
      ctx.lineTo(pixel.x, pixel.y);
    }
    def text(
      text : String,
      pixel : PixelLoc,
      color : String,
      textAlign : String = "center",
      textBaseline : String = "alphabetic",
      style : String = "normal",
      fontSize : Int = 10,
    ) : Unit
    = {
      ctx.globalAlpha = 1.0
      ctx.fillStyle = color
      ctx.textAlign = textAlign
      ctx.textBaseline = textBaseline
      ctx.font = style + " " + fontSize + "px sans-serif"
      ctx.fillText(text, Math.floor(pixel.x), Math.floor(pixel.y))
    }

    def hexCorner(hexLoc : HexLoc, scale : Double, corner : Int) : HexLoc = {
      hexLoc + HexVec.corners(corner) * scale
    }

    def drawHex(hexLoc : HexLoc, color : Option[String], texture: Option[(String,String)], scale : Double, doStroke: Boolean, doFill:Boolean, alpha: Double, lineWidth: Double) : Unit = {
      val oldAlpha = ctx.globalAlpha
      ctx.globalAlpha = alpha
      ctx.lineWidth = lineWidth
      color.foreach { color =>
        ctx.strokeStyle = color
        ctx.fillStyle = color
      }
      texture.foreach { case (texture,mode) =>
        val img = textures(texture)
        val pat = ctx.createPattern(img,mode)
        ctx.fillStyle = pat
      }
      ctx.beginPath()
      move(hexCorner(hexLoc,scale,0))
      line(hexCorner(hexLoc,scale,1))
      line(hexCorner(hexLoc,scale,2))
      line(hexCorner(hexLoc,scale,3))
      line(hexCorner(hexLoc,scale,4))
      line(hexCorner(hexLoc,scale,5))
      line(hexCorner(hexLoc,scale,0))
      if(doStroke) ctx.stroke()
      if(doFill)  ctx.fill()
      ctx.closePath()
      ctx.globalAlpha = oldAlpha
    }
    def strokeHex(hexLoc : HexLoc, color : String, scale : Double, alpha: Double = 1.0, lineWidth: Double = 1.0) : Unit = {
      drawHex(hexLoc,Some(color),None,scale,true,false,alpha,lineWidth);
    }
    //TODO stop drawing pieces with so much alpha. They change color too much on different terrain
    //This requires recalibrating all the colors
    def fillHex(hexLoc : HexLoc, color : String, scale : Double, alpha: Double = 1.0) : Unit = {
      drawHex(hexLoc,Some(color),None,scale,false,true,alpha,1.0);
    }
    def fillHexWithTexture(hexLoc : HexLoc, texture: String, scale : Double, alpha: Double = 1.0) : Unit = {
      drawHex(hexLoc,None,Some((texture,"repeat")),scale,false,true,alpha,1.0);
    }
    def fillHexWithImage(hexLoc : HexLoc, texture: String, scale : Double, alpha: Double = 1.0) : Unit = {
      //Adjust so that the image is centered
      val img = textures(texture)
      ctx.save()
      val pixel = PixelLoc.ofHexLoc(hexLoc,gridSize)
      ctx.translate(pixel.x,pixel.y)
      ctx.translate(-img.width*0.5,-img.height*0.5)
      drawHex(HexLoc.ofPixel(PixelLoc(img.width*0.5,img.height*0.5),gridSize),None,Some((texture,"no-repeat")),scale,false,true,alpha,1.0);
      ctx.restore()
    }

    //Based on murmurhash's avalanche mixer
    def deterministicRandom(x: Int, y: Int, n: Int) = {
      var h: Long = x.toLong + 37 * y.toLong
      h ^= h >> 33;
      h *= 0xff51afd7ed558ccdL;
      h ^= h >> 33;
      h *= 0xc4ceb9fe1a85ec53L;
      h ^= h >> 33;
      ((h % n) + n) % n
    }

    def drawTile(loc : Loc, tile: Tile) : Unit = {
      val hexLoc = hexLocOfLoc(loc)
      tile.terrain match {
        case Wall => fillHex(hexLoc, "white", tileScale)
        case Ground | StartHex(_) =>
          val texture = BoardMaps.groundImage(boardNames(boardIdx))
          fillHexWithTexture(hexLoc, texture, tileScale)
          //fillHex(hexLoc, "green", tileScale)
        case Water =>
          val texture = BoardMaps.waterImage(boardNames(boardIdx))
          fillHexWithTexture(hexLoc, texture, tileScale)
        case Graveyard =>
          val img = "img_terrain_graveyard" + deterministicRandom(loc.x,loc.y,4)
          fillHexWithImage(hexLoc, img, tileScale)
          //fillHex(hexLoc, "#aa8899", tileScale)
          strokeHex(hexLoc, "#776677", tileScale, alpha=0.4, lineWidth=1.5)
        case SorceryNode =>
          val texture = BoardMaps.groundImage(boardNames(boardIdx))
          fillHexWithTexture(hexLoc, texture, tileScale)
          fillHex(hexLoc, "#ffcc88", tileScale, alpha=0.15)
          strokeHex(hexLoc, "#ffcc88", tileScale, alpha=0.5, lineWidth=2.0)
          val img = "img_terrain_leyline"
          fillHexWithImage(hexLoc, img, tileScale)
        case Teleporter =>
          val texture = BoardMaps.groundImage(boardNames(boardIdx))
          fillHexWithTexture(hexLoc, texture, tileScale)
          fillHex(hexLoc, "#ccff88", tileScale, alpha=0.15)
          strokeHex(hexLoc, "#ccff88", tileScale, alpha=0.5, lineWidth=2.0)
          val img = "img_terrain_teleporter"
          fillHexWithImage(hexLoc, img, tileScale)
        case Spawner(_) =>
          val texture = BoardMaps.groundImage(boardNames(boardIdx))
          fillHexWithTexture(hexLoc, texture, tileScale)
          fillHex(hexLoc, "#ff55ff", tileScale, alpha=0.15)
          strokeHex(hexLoc, "#ff55ff", tileScale, alpha=0.5, lineWidth=2.0)
          val img = "img_terrain_spawner"
          fillHexWithImage(hexLoc, img, tileScale)
      }
      if(showCoords) {
        val (loc,_) = hexLoc.round(flipDisplay,board)
        text(loc.toString, PixelLoc.ofHexLoc(hexLoc, gridSize)+PixelVec(0, -gridSize/2.0), "black")
      }
    }

    def displayNameOfPieceName(pieceName: PieceName): String = {
      Units.pieceMap(pieceName).shortDisplayName
    }

    def drawPiece(hexLoc : HexLoc, scale : Double, side: Option[Side], label: String) : Unit = {
      val pieceColor =
        side match {
          case None => "#cccccc"
          case Some(S0) => "#ccccff"
          case Some(S1) => "#ffbbbb"
        }
      fillHex(hexLoc, pieceColor, scale)
      strokeHex(hexLoc, "black", scale, alpha=0.2)
      if(label != "")
        text(label, PixelLoc.ofHexLoc(hexLoc,gridSize), "black")
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

    def drawSidebar(stats: Option[PieceStats], side: Option[Side], tile : Option[Tile]) = {
      val loc = UI.Sidebar.loc
      drawPiece(loc, 5.0, side, "")
      var row_idx = 0
      def row(n:Int) : PixelLoc = {
        return PixelLoc.ofHexLoc(loc, gridSize) + PixelVec(0, 15.0*(n-6))
      }
      def show(s:String) : Unit = {
        text(s, row(row_idx), "black")
        row_idx = row_idx + 1
      }
      stats match {
        case None => ()
        case Some(stats) =>
          show(stats.displayName)
          val aStr = stats.attackEffect match {
            case None => "Can't attack"
            case Some(Damage(n)) => "Attack: " + n + " damage"
            case Some(Unsummon) => "Attack: Unsummon"
            case Some(Kill) => "Attack: Instant kill"
            case Some(Enchant(_)) => ""
            case Some(TransformInto(_)) => ""
          }
          if(stats.isNecromancer && stats.swarmMax <= 1) {
            show("If your necromancer dies, you lose the board!")
          } else if(stats.isNecromancer) {
            show("If your necromancers die, you lose the board!")
          } else {
            val costStr = "Cost: " + stats.cost + " souls"
            stats.deathSpawn match {
              case None =>
                if(stats.rebate > 0) {
                  show(costStr + " (death: +" + stats.rebate + " souls)")
                } else {
                  show(costStr)
                }
              case Some(pieceName) =>
                show(costStr + " (death: becomes " + Units.pieceMap(pieceName).displayName + ")")
            }
          }

          if(stats.numAttacks <= 1) {
            show(aStr)
          } else {
            show(aStr + " (" + stats.numAttacks + "x/turn)")
          }

          if(stats.defense >= 100000) {
            show("Cannot be killed")
          } else {
            show("Defense: " + stats.defense)
          }

          if(stats.moveRange == 0) {
            show("Cannot move")
          } else if(stats.moveRange == 1) {
            show("Speed: 1 hex/turn")
          } else {
            show("Speed: " + stats.moveRange + " hexes/turn")
          }
          if(stats.attackEffect.isDefined) {
            val vsFlyingStr = {
              if(stats.attackRangeVsFlying != stats.attackRange) " (" + stats.attackRangeVsFlying + " vs flying)"
              else ""
            }
            if(stats.attackRange == 1) {
              show("Attack range: 1 hex" + vsFlyingStr)
            } else {
              show("Attack range: " + stats.attackRange + " hexes" + vsFlyingStr)
            }
          }
          if(stats.isFlying) {
            show("Flying (move over water or enemies)")
          }
          if(stats.isLumbering) {
            show("Lumbering (cannot attack after moving)")
          }
          if(stats.swarmMax > 1) {
            show("Swarm (up to " + stats.swarmMax + "/hex)")
          }
          if(stats.spawnRange > 0) {
            assert(stats.spawnRange <= 2)
            if(stats.spawnRange == 1)
              show("Summoner (friendly units can spawn adjacent)")
            else
              show("Greater Summoner (spawn units at range 2)")
          }
          if(stats.isPersistent) {
            show("Persistent (cannot be unsummoned)")
          }
          if(stats.isEldritch) {
            show("Eldritch (can spawn next to any friendly unit)")
          }
          if(stats.isWailing) {
            show("Dies after the turn it attacks.")
          }
          if(!stats.canHurtNecromancer) {
            show("Cannot attack necromancers.")
          }
          if(stats.extraMana > 0) {
            show("Produces " + stats.extraMana + " souls/turn.")
          }
          if(stats.extraSorceryPower > 0) {
            show("Produces " + stats.extraSorceryPower + " sorcery power/turn.")
          }
          stats.abilities.foreach { case (_,ability) =>
            show("")
            show("Ability: " + ability.displayName)
            show(ability.desc)
          }
      }
      tile match {
        case None => ()
        case Some(tile) =>
          if(stats.nonEmpty)
            show("------------------------------")

          tile.terrain match {
            case Wall =>
              show("Terrain: Wall")
              show("Impassable")
            case Ground | StartHex(_) =>
              show("Terrain: Ground")
            case Water =>
              show("Terrain: Water")
              show("Only passable by flying units.")
            case Graveyard =>
              show("Terrain: Graveyard")
              show("Gain 1 soul at end of turn if occupied.")
              show("At the start of turn, win the board if you")
              show("occupy at least 8 graveyards.")
            case SorceryNode =>
              show("Terrain: Ley Line")
              show("Gain 1 sorcery power at start of turn if occupied.")
            case Teleporter =>
              show("Terrain: Teleporter")
              show("A piece that begins the turn here may spend its")
              show("entire turn to move to any hex on the board.")
            case Spawner(spawnName) =>
              val name = Units.pieceMap(spawnName).displayName
              show("Terrain: " + name + " Spawner")
              show("You may spawn a free " + name + " here.")
              show("Only one spawner can be used per turn.")
          }
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

    //Board title
    text(
      boardNames(boardIdx) + " (board " + (boardIdx+1) + ")",
      PixelLoc.ofLoc(UI.Info.getBoardTitleLoc(board), gridSize),
      "black", textAlign="left", textBaseline="top",fontSize=14
    )

    //Forward and backward buttons
    if(boardIdx > 0)
      text("<- Prev Board", PixelLoc.ofLoc(UI.PrevBoard.locs(0), gridSize), "black", textAlign="left", textBaseline="top", fontSize=12)
    if(boardIdx < boardNames.length-1)
      text("Next Board ->", PixelLoc.ofLoc(UI.NextBoard.locs(board)(0), gridSize), "black", textAlign="left", textBaseline="top", fontSize=12)

    //Game info text
    Side.foreach { side =>
      val infoLoc = UI.Info.getLoc(side,flipDisplay,board)
      val pixelLoc = PixelLoc.ofHexLoc(hexLocOfLoc(infoLoc), gridSize)
      val color = side match {
        case S0 => "#000099"
        case S1 => "#770000"
      }
      val mana = boards.foldLeft(game.mana(side)) { case (sum,board) =>
        sum + board.curState.manaThisRound(side)
      }
      val newMana = boards.foldLeft(game.extraManaPerTurn) { case (sum, board) =>
        sum + board.curState.endOfTurnMana(side)
      }

      def textAtLoc(s: String, dpx: Double, dpy: Double) =
        text(s, pixelLoc+PixelVec(dpx,dpy), color, textAlign="left")

      if(game.winner.nonEmpty) {
        if(game.winner == Some(side))
          textAtLoc(side.toColorName + " Team wins the game!", 0.0, -9.0)
      }
      else {
        if(side == board.side) {
          textAtLoc(side.toColorName + " Team's Turn!", 0.0, -9.0)
          timeLeft.foreach { timeLeft =>
            val seconds: Int = Math.floor(timeLeft).toInt
            val timeStr = {
              if(seconds < 0) "-" + ((-seconds) / 60).toString + ":" + "%02d".format((-seconds) % 60)
              else (seconds / 60).toString + ":" + "%02d".format(seconds % 60)
            }
            textAtLoc("Time left: " + timeStr, 120.0, -9.0)
          }
        }
      }

      textAtLoc("Net +souls this board: " + (board.totalMana(side) - board.totalCosts(side)), 250.0, -9.0)

      textAtLoc(side.toColorName + " Team Souls: " + mana + " (+" + newMana + "/turn)", 0.0, 3.0)

      textAtLoc(side.toColorName + " Team Wins: " + game.wins(side) + "/" + game.targetNumWins, 150.0, 3.0)

      if(side == board.side)
        textAtLoc("Sorcery Power: " + board.sorceryPower, 250.0, 3.0)
    }

    //End turn hex
    if(game.isBoardDone(boardIdx)) {
      fillHex(UI.EndTurn.loc, "#ff99ff", tileScale, alpha=1.0)
      strokeHex(UI.EndTurn.loc, "#ff00ff", tileScale, lineWidth=2.0)
    }
    else {
      fillHex(UI.EndTurn.loc, "#dddddd", tileScale)
      strokeHex(UI.EndTurn.loc, "#666666", tileScale, lineWidth=1.0)
    }
    text("End Turn", PixelLoc.ofHexLoc(hexLocOfLoc(UI.EndTurn.loc), gridSize), "black")

    //Resign board hex
    fillHex(UI.ResignBoard.loc, "#dddddd", tileScale)
    strokeHex(UI.ResignBoard.loc, "#666666", tileScale, lineWidth=1.0)
    text("Resign", PixelLoc.ofHexLoc(hexLocOfLoc(UI.ResignBoard.loc), gridSize) + PixelVec(0,-4.0), "black")
    text("Board", PixelLoc.ofHexLoc(hexLocOfLoc(UI.ResignBoard.loc), gridSize) + PixelVec(0,7.0), "black")

    //Reinforcements
    Side.foreach { side =>
      val locsAndContents = UI.Reinforcements.getLocsAndContents(side,flipDisplay,board)
      locsAndContents.foreach { case (loc,pieceName,count) =>
        val hexLoc = hexLocOfLoc(loc)
        drawPiece(hexLoc, pieceScale, Some(side), "")
        val label = displayNameOfPieceName(pieceName)
        text(label, PixelLoc.ofHexLoc(hexLoc,gridSize) + PixelVec(0,-4.0), "black")
        text("x " + count, PixelLoc.ofHexLoc(hexLoc,gridSize) + PixelVec(0,8.0), "black")
      }
    }

    //Dead pieces
    {
      val locsAndContents = UI.DeadPieces.getLocsAndContents(board)
      if(locsAndContents.length > 0) {
        text("Dead pieces", PixelLoc.ofHexLoc(hexLocOfLoc(UI.DeadPieces.getDescLoc(board)), gridSize), "black")
      }
      locsAndContents.foreach { case (loc,_,pieceName,side) =>
        val label = displayNameOfPieceName(pieceName)
        drawPiece(hexLocOfLoc(loc), pieceScale, Some(side), label)
      }
    }

    //Techs
    val techLocs = UI.Tech.getLocs(game)
    for(i <- 0 until game.techLine.length) {
      val techState = game.techLine(i)
      val loc = techLocs(i)
      val fillColor =
        (techState.level(S0), techState.level(S1)) match {
          case (TechLocked, TechLocked) => "#aaaaaa"
          case (TechLocked, (TechUnlocked | TechAcquired)) => "#ffbbbb"
          case ((TechUnlocked | TechAcquired), TechLocked) => "#bbbbff"
          case ((TechUnlocked | TechAcquired), (TechUnlocked | TechAcquired)) => "#ff99ff"
        }
      val strokeColor =
        (techState.level(S0), techState.level(S1)) match {
          case ((TechLocked | TechUnlocked), (TechLocked | TechUnlocked)) => Some("#888888")
          case ((TechLocked | TechUnlocked), TechAcquired) => Some("#ff3333")
          case (TechAcquired, (TechLocked | TechUnlocked)) => Some("#3333ff")
          case (TechAcquired, TechAcquired) => Some("#ff00ff")
        }

      fillHex(loc, fillColor, techScale, alpha=1.0)
      strokeColor.foreach { color =>
        strokeHex(loc, color, techScale, lineWidth=2.0)
      }

      val hexLoc = hexLocOfLoc(loc)
      text(techState.shortDisplayName, PixelLoc.ofHexLoc(hexLoc, gridSize), "black")
      techState.techNumber.foreach { techNumber =>
        text("#" + techNumber, PixelLoc.ofHexLoc(hexLoc, gridSize) + PixelVec(0,10), "black")
      }
      text(techState.level(S0).toUnicodeSymbol, PixelLoc.ofHexLoc(hexLoc + HexVec.corners(4) * techInteriorScale, gridSize), "blue")
      text(techState.level(S1).toUnicodeSymbol, PixelLoc.ofHexLoc(hexLoc + HexVec.corners(0) * techInteriorScale, gridSize), "red")
    }

    //Terrain
    board.tiles.foreachi {case (loc, tile) =>
      drawTile(loc,tile)
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

    def getAttackStringAndColor(baseStats: PieceStats, curStats: PieceStats, actState: ActState): (String,String) = {
      val effectStr = curStats.attackEffect match {
        case None => ""
        case Some(Damage(n)) => "A" + n
        case Some(Unsummon) => "A*"
        case Some(Kill) => "AK"
        case Some(_) => "A!"
      }
      val displayedAttacks = actState match {
        case DoneActing => curStats.numAttacks
        case Moving(_) => curStats.numAttacks
        case Attacking(attacksUsed) =>
          if(attacksUsed >= curStats.numAttacks) curStats.numAttacks
          else curStats.numAttacks - attacksUsed
      }
      val countStr = {
        if(displayedAttacks > 1) displayedAttacks.toString
        else ""
      }
      val colorFromEnchantments: Option[String] = {
        if(curStats.numAttacks > baseStats.numAttacks) Some("green")
        else {
          (curStats.attackEffect, baseStats.attackEffect) match {
            case (Some(Damage(c)), Some(Damage(b))) => if(c < b) Some("magenta") else if(c == b) None else Some("green")
            case (None, Some(Damage(_))) => Some("magenta")
            case (Some(Unsummon), Some(Damage(_))) => Some("green")
            case _ => None
          }
        }
      }

      val color = {
        actState match {
          case DoneActing => "#777777"
          case Moving(n) =>
            if(n > 0 && curStats.isLumbering) "#777777"
            else colorFromEnchantments.getOrElse("black")
          case Attacking(attacksUsed) =>
            if(attacksUsed >= curStats.numAttacks) "#777777"
            else colorFromEnchantments.getOrElse {
              if(attacksUsed > 0) "brown"
              else "black"
            }
        }
      }
      (countStr+effectStr,color)
    }

    def getDefenseStringAndColor(baseStats: PieceStats, curStats: PieceStats, damage: Int): (String,String) = {
      if(curStats.defense > 100000) ("","black")
      else {
        val str = (if(curStats.isPersistent) "P" else "D") + (curStats.defense - damage)
        val color = {
          if(damage > 0 || curStats.defense < baseStats.defense) "magenta"
          else if(curStats.isPersistent && !baseStats.isPersistent) "green"
          else if(curStats.defense > baseStats.defense) "green"
          else "black"
        }
        (str,color)
      }
    }

    def getRangeStringAndColor(baseStats: PieceStats, curStats: PieceStats): (String,String) = {
      val baseStr = {
        if(!curStats.isLumbering && curStats.attackRange <= 1 && curStats.attackRangeVsFlying <= 1) ""
        else if(curStats.isLumbering && curStats.attackRange <= 1) "L"
        else if(curStats.isLumbering) "L" + curStats.attackRange
        else "R" + curStats.attackRange
      }
      val str = {
        if(curStats.attackRangeVsFlying != curStats.attackRange) baseStr + "F" + curStats.attackRangeVsFlying
        else baseStr
      }
      val color = {
        if(curStats.isLumbering && !baseStats.isLumbering) "magenta"
        else if(curStats.attackRange < baseStats.attackRange) "magenta"
        else if(curStats.attackRangeVsFlying > baseStats.attackRangeVsFlying) "green"
        else "black"
      }
      (str,color)
    }

    def getMoveStringAndColor(baseStats: PieceStats, curStats: PieceStats, actState: ActState): (String,String) = {
      val displayedMoveRange = {
        actState match {
          case DoneActing | Attacking(_) => curStats.moveRange
          case Moving(stepsUsed) =>
            if(stepsUsed >= curStats.moveRange) curStats.moveRange
            else curStats.moveRange - stepsUsed
        }
      }

      val str = {
        if(curStats.abilities.contains(BlinkAbility.name)) "M*"
        else if(!curStats.isFlying && curStats.moveRange == 1) ""
        else if(curStats.isFlying && curStats.moveRange == 1) "F"
        else if(curStats.isFlying) "F" + displayedMoveRange
        else "M" + displayedMoveRange
      }
      val color = {
        actState match {
          case DoneActing => "#777777"
          case Attacking(_) =>
            if(curStats.abilities.contains(BlinkAbility.name)) "black"
            else "#777777"
          case Moving(stepsUsed) =>
            if(curStats.abilities.contains(BlinkAbility.name)) "black"
            else if(stepsUsed >= curStats.moveRange) "#777777"
            else if(curStats.moveRange < baseStats.moveRange) "magenta"
            else if(curStats.moveRange > baseStats.moveRange) "green"
            else if(displayedMoveRange < curStats.moveRange) "brown"
            else "black"
        }
      }
      (str,color)
    }

    //Pieces
    board.pieces.foreach { pieces =>
      pieces.foreach { piece =>
        val (loc,scale) = locAndScaleOfPiece(board,piece)
        val baseStats = piece.baseStats
        val curStats = piece.curStats(board)
        val label = baseStats.shortDisplayName

        drawPiece(loc, scale, Some(piece.side), "")

        val (aStr,aColor) = getAttackStringAndColor(baseStats,curStats,piece.actState)
        val (dStr,dColor) = getDefenseStringAndColor(baseStats,curStats,piece.damage)
        val (rStr,rColor) = getRangeStringAndColor(baseStats,curStats)
        val (mStr,mColor) = getMoveStringAndColor(baseStats,curStats,piece.actState)

        //Multiple pieces in same hex
        if(board.pieces(piece.loc).length > 1) {
          text(label, PixelLoc.ofHexLoc(loc,gridSize) + PixelVec(0,-3.0), "black", fontSize=8)
          text(aStr, PixelLoc.ofHexLoc(loc,gridSize) + PixelVec(-5.0,5.0), aColor, fontSize=8)
          text(dStr, PixelLoc.ofHexLoc(loc,gridSize) + PixelVec(5.0,5.0), dColor, fontSize=8)
        }
        //One piece in hex
        else {
          if(baseStats.name == Units.zombie.name && curStats.isBaseStats) {
            text(label, PixelLoc.ofHexLoc(loc,gridSize) + PixelVec(0,-4.0), "black")
          }
          else {
            text(label, PixelLoc.ofHexLoc(loc,gridSize) + PixelVec(0,-8.0), "black")
            text(aStr, PixelLoc.ofHexLoc(loc,gridSize) + PixelVec(-10.0,2.0), aColor)
            text(dStr, PixelLoc.ofHexLoc(loc,gridSize) + PixelVec(10.0,2.0), dColor)
            text(rStr, PixelLoc.ofHexLoc(loc,gridSize) + PixelVec(-10.0,12.0), rColor)
            text(mStr, PixelLoc.ofHexLoc(loc,gridSize) + PixelVec(10.0,12.0), mColor)
          }
        }

        if(piece.side != game.curSide) {
          if(piece.damage > 0)
            strokeHex(loc, "magenta", scale)
        }
        else {
          if(pieceHasNotDoneThings(piece))
            strokeHex(loc, "green", scale, lineWidth=2.0)
          else if(pieceCanStillDoThings(piece))
            strokeHex(loc, "orange", scale, lineWidth=1.5)
        }
      }
    }

    def drawPath(path: Vector[Loc], overrideStart: Option[HexLoc] = None): Unit = {
      if(path.length > 0) {
        ctx.globalAlpha = 1.0
        ctx.fillStyle = "black"
        ctx.setLineDash(scala.scalajs.js.Array(5.0, 10.0))
        ctx.beginPath()
        move(overrideStart.getOrElse(path(0)))
        for(i <- 1 to path.length-1) {
          line(path(i))
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
              strokeHex(hexLocOfLoc(loc), "black", tileScale)
            }
          }
        case Attack(aSpec,tSpec) =>
          findLocOfPiece(aSpec).foreach { loc =>
            strokeHex(hexLocOfLoc(loc), "black", tileScale)
          }
          findLocOfPiece(tSpec).foreach { loc =>
            fillHex(hexLocOfLoc(loc), "magenta", tileScale, alpha=0.1)
            strokeHex(hexLocOfLoc(loc), "magenta", tileScale)
          }
        case Spawn(_,_) =>
          UI.Reinforcements.getLocs(board.side, flipDisplay, board).foreach { loc =>
            strokeHex(hexLocOfLoc(loc), "black", tileScale, alpha=0.5)
          }
        case ActivateTile(loc) =>
          strokeHex(hexLocOfLoc(loc), "black", tileScale)
        case ActivateAbility(spec,_,targets) =>
          List(spec,targets.target0,targets.target1).foreach { spec =>
            findLocOfPiece(spec).foreach { loc =>
              strokeHex(hexLocOfLoc(loc), "black", tileScale)
            }
          }
        case Teleport(_,src,dest) =>
          strokeHex(hexLocOfLoc(src), "black", tileScale)
          strokeHex(hexLocOfLoc(dest), "black", tileScale)
        case PlaySpell(_,targets) =>
          List(targets.target0,targets.target1).foreach { spec =>
            findLocOfPiece(spec).foreach { loc =>
              strokeHex(hexLocOfLoc(loc), "black", tileScale)
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

    def highlightHex(hexLoc: HexLoc, scale: Double = tileScale) = {
      fillHex(hexLoc, "yellow", scale, alpha=0.15)
      strokeHex(hexLoc, "black", scale, alpha=0.5, lineWidth=1.5)
    }

    mouseState.mode match {
      //TODO highlight legal targets
      case (_: SelectTargetMouseMode) => ()

      case (mode: NormalMouseMode) =>
        //Highlight mouse's target on mouse hover
        mouseState.hovered match {
          case MouseNone => ()
          case MouseTile(loc) =>
            drawSidebar(None, None, Some(board.tiles(loc)))
          case MouseEndTurn =>
            val loc = UI.EndTurn.loc
            strokeHex(loc, "black", tileScale, alpha=0.5)
          case MouseResignBoard =>
            val loc = UI.ResignBoard.loc
            strokeHex(loc, "black", tileScale, alpha=0.5)
          case MousePrevBoard =>
            if(boardIdx > 0)
              text("<- Prev Board", PixelLoc.ofLoc(UI.PrevBoard.locs(0), gridSize), "darkgreen", textAlign="left", textBaseline="top", fontSize=12)
          case MouseNextBoard =>
            if(boardIdx < boardNames.length-1)
              text("Next Board ->", PixelLoc.ofLoc(UI.NextBoard.locs(board)(0), gridSize), "darkgreen", textAlign="left", textBaseline="top", fontSize=12)

          case MouseTech(techIdx) =>
            if(canClickOnTech(techIdx)) {
              val loc = UI.Tech.getLoc(techIdx)
              strokeHex(loc, "black", tileScale, alpha=0.5)
            }
            drawSidebar(Some(game.techLine(techIdx).tech.pieceStats), None, None)
          case MouseReinforcement(pieceName,side) =>
            UI.Reinforcements.getSelectedLocAndCount(side, flipDisplay, board, pieceName) match {
              case None => ()
              case Some((loc,_)) =>
                strokeHex(hexLocOfLoc(loc), "black", pieceScale, alpha=0.5)
            }
            if(undoing) {
              val pieceSpec = board.unsummonedThisTurn.reverse.findMap { case (pieceSpec,name,_) =>
                if(pieceName == name) Some(pieceSpec) else None
              }
              pieceSpec.foreach { pieceSpec => highlightUndoneActionsForPieceSpec(pieceSpec) }
            }
            drawSidebar(Some(Units.pieceMap(pieceName)), Some(side), None)
          case MouseDeadPiece(pieceSpec) =>
            UI.DeadPieces.getSelectedLoc(board, pieceSpec) match {
              case None => ()
              case Some(loc) =>
                strokeHex(hexLocOfLoc(loc), "black", pieceScale, alpha=0.5)
            }
            if(undoing)
              highlightUndoneActionsForPieceSpec(pieceSpec)
            UI.DeadPieces.getSelectedPiece(board, pieceSpec) match {
              case None => ()
              case Some((stats, side)) =>
                drawSidebar(Some(stats), Some(side), None)
            }
          case MousePiece(spec) =>
            board.findPiece(spec) match {
              case None => ()
              case Some(piece) =>
                val (loc,scale) = locAndScaleOfPiece(board,piece)
                strokeHex(loc, "black", scale, alpha=0.5)
                drawSidebar(Some(piece.curStats(board)), Some(piece.side), Some(board.tiles(piece.loc)))
            }

            if(undoing)
              highlightUndoneActionsForPieceSpec(spec)
        }

        //Draw highlights based on piece selected by mouse click
        mouseState.dragTarget match {
          case MouseNone => ()
          case MouseTile(_) => ()
          case MouseEndTurn =>
            val loc = UI.EndTurn.loc
            highlightHex(loc)
          case MouseResignBoard =>
            val loc = UI.ResignBoard.loc
            highlightHex(loc)
          case MousePrevBoard =>
            if(boardIdx > 0)
              text("<- Prev Board", PixelLoc.ofLoc(UI.PrevBoard.locs(0), gridSize), "cyan", textAlign="left", textBaseline="top", fontSize=12)
          case MouseNextBoard =>
            if(boardIdx < boardNames.length-1)
              text("Next Board ->", PixelLoc.ofLoc(UI.NextBoard.locs(board)(0), gridSize), "cyan", textAlign="left", textBaseline="top", fontSize=12)
          case MouseTech(techIdx) =>
            if(canClickOnTech(techIdx)) {
              val loc = UI.Tech.getLoc(techIdx)
              highlightHex(loc)
            }
          case MouseReinforcement(pieceName,side) =>
            UI.Reinforcements.getSelectedLocAndCount(side, flipDisplay, board, pieceName) match {
              case None => ()
              case Some((loc,_)) =>
                highlightHex(loc,scale=pieceScale)
            }
            if(!undoing) {
              val locs = board.legalSpawnLocs(pieceName)
              for(loc <- locs) {
                highlightHex(loc)
              }
            }

          case MouseDeadPiece(pieceSpec) =>
            if(undoing) {
              UI.DeadPieces.getSelectedLoc(board, pieceSpec) match {
                case None => ()
                case Some(loc) =>
                  highlightHex(loc)
              }
            }
          case MousePiece(spec) =>
            board.findPiece(spec) match {
              case None => ()
              case Some(piece) =>
                val (loc,scale) = locAndScaleOfPiece(board,piece)

                if(undoing) {
                  highlightHex(loc)
                }
                else {
                  if(piece.side == game.curSide) {
                    //Only stroke and no highlight since we're already getting a highlight from drawing paths.
                    strokeHex(loc, "black", scale, alpha=0.5)

                    //Draw based on what would happen if we released the mouse
                    mouseState.hoverLoc.foreach { hoverLoc =>
                      mode.dragPieceMouseUpActions(mouseState.hovered, hoverLoc, piece, board).foreach {
                        case (_ : Movements) =>
                          //If moving, draw the movement path
                          val path = mode.path
                          drawPath(path.toVector,overrideStart = Some(loc))
                        case (_ : Teleport) =>
                          //If teleporting, highlight the teleport location
                          strokeHex(hoverLoc, "cyan", scale, alpha=0.3, lineWidth=2)
                          fillHex(hoverLoc, "cyan", scale, alpha=0.05)
                        case (_ : Attack) | (_ : Spawn) | (_ : ActivateTile) | (_ : ActivateAbility) | (_ : PlaySpell) | (_ : DiscardSpell) =>
                          ()
                      }
                    }

                    //Highlight the movement range
                    val moveLocsAndSteps = board.legalMoves(piece)
                    moveLocsAndSteps.foreach { case (loc,_) =>
                      highlightHex(loc)
                    }

                    val attackerStats = piece.curStats(board)
                    val attackerState = piece.actState
                    def canAttackIfInRange(targetPiece: Piece, attackerHasMoved: Boolean): Boolean = {
                      val targetStats = targetPiece.curStats(board)
                      board.canAttack(attackerStats,attackerHasMoved,attackerState,targetStats)
                    }
                    def canBeInRange(targetPiece: Piece): Boolean = {
                      val targetStats = targetPiece.curStats(board)
                      val attackRange = { if(targetStats.isFlying) attackerStats.attackRangeVsFlying else attackerStats.attackRange }
                      moveLocsAndSteps.exists { case (loc,_) =>
                        board.topology.distance(loc,targetPiece.loc) <= attackRange
                      }
                    }
                    def inRangeNow(targetPiece: Piece): Boolean = {
                      val targetStats = targetPiece.curStats(board)
                      val attackRange = { if(targetStats.isFlying) attackerStats.attackRangeVsFlying else attackerStats.attackRange }
                      board.topology.distance(piece.loc,targetPiece.loc) <= attackRange
                    }
                    def canAttack(targetPiece: Piece): Boolean = {
                      if(targetPiece.side == piece.side)
                        false
                      else if(inRangeNow(targetPiece) && !piece.hasMoved)
                        canAttackIfInRange(targetPiece, attackerHasMoved = false)
                      else if(canBeInRange(targetPiece))
                        canAttackIfInRange(targetPiece, attackerHasMoved = true)
                      else false
                    }

                    //Highlight all legal pieces to attack
                    board.pieces.foreach { pieces =>
                      pieces.foreach { targetPiece =>
                        if(canAttack(targetPiece)) {
                          val (targetLoc,targetScale) = locAndScaleOfPiece(board,targetPiece)
                          fillHex(targetLoc, "magenta", targetScale, alpha=0.15)
                          strokeHex(targetLoc, "magenta", targetScale, alpha=0.5, lineWidth=1.5)
                        }
                      }
                    }

                    //Highlight the hovered piece if attacking it is legal
                    mouseState.hovered.findPiece(board) match {
                      case None => ()
                      case Some(targetPiece) =>
                        if(canAttack(targetPiece)) {
                          val (targetLoc,targetScale) = locAndScaleOfPiece(board,targetPiece)
                          fillHex(targetLoc, "magenta", targetScale, alpha=0.1)
                          strokeHex(targetLoc, "magenta", targetScale, alpha=0.5)
                        }
                    }
                  }
                }
            }
        }
    }

    //Highlight hex tile on mouse hover
    mouseState.hovered match {
      case MouseNone => ()
      case MousePrevBoard => ()
      case MouseNextBoard => ()
      case _ =>
        mouseState.hoverLoc.foreach { hoverLoc =>
          strokeHex(hoverLoc, "black", tileScale, alpha=0.3)
        }
    }
  }
}

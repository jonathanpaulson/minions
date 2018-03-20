package minionsgame.jsclient

import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html.Canvas
import org.scalajs.dom.raw.HTMLImageElement
import scala.util.{Try,Success,Failure}

import minionsgame.core._
import RichImplicits._

object Drawing {
  //The euclidean distance from the center of a hexagon to the corner in pixels.
  val gridSize = 32.0
  val tileScale = 31.5 / gridSize
  val pieceScale = 26.5 / gridSize
  val techScale = 31.0 / gridSize
  val spellScale = 29.0 / gridSize
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
    externalInfo: ExternalInfo,
    boards: Array[Board],
    boardNames: Array[String],
    boardIdx: Int,
    ui: UI,
    mouseState: MouseState,
    undoing: Boolean,
    timeLeft: Option[Double],
    client: Client
  ) : Unit = {

    val board = boards(boardIdx).curState

    import scala.language.implicitConversions
    implicit def pixelLocOfHexLoc(hexLoc: HexLoc): PixelLoc = {
      PixelLoc.ofHexLoc(hexLoc,gridSize)
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
      alpha: Double = 1.0,
    ) : Unit
    = {
      val oldAlpha = ctx.globalAlpha
      ctx.globalAlpha = alpha
      ctx.fillStyle = color
      ctx.textAlign = textAlign
      ctx.textBaseline = textBaseline
      ctx.font = style + " " + fontSize + "px sans-serif"
      ctx.fillText(text, Math.floor(pixel.x), Math.floor(pixel.y))
      ctx.globalAlpha = oldAlpha
    }

    def hexCorner(hexLoc : HexLoc, scale : Double, corner : Int) : HexLoc = {
      hexLoc + HexVec.corners(corner) * scale
    }

    def drawHex(hexLoc : HexLoc, color : Option[String], texture: Option[(String,String)], scale : Double, doStroke: Boolean, doFill:Boolean, alpha: Double, lineWidth: Double, rectangle: Boolean) : Unit = {
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
      if(!rectangle) {
        line(hexCorner(hexLoc,scale,2))
      }
      line(hexCorner(hexLoc,scale,3))
      line(hexCorner(hexLoc,scale,4))
      if(!rectangle) {
        line(hexCorner(hexLoc,scale,5))
      }
      line(hexCorner(hexLoc,scale,0))
      ctx.closePath()
      if(doStroke) ctx.stroke()
      if(doFill)  ctx.fill()
      ctx.globalAlpha = oldAlpha
    }
    def strokeHex(hexLoc : HexLoc, color : String, scale : Double, alpha: Double = 1.0, lineWidth: Double = 1.0, rectangle: Boolean = false) : Unit = {
      drawHex(hexLoc,Some(color),None,scale,true,false,alpha,lineWidth,rectangle);
    }
    //TODO stop drawing pieces with so much alpha. They change color too much on different terrain
    //This requires recalibrating all the colors
    def fillHex(hexLoc : HexLoc, color : String, scale : Double, alpha: Double = 1.0, rectangle: Boolean = false) : Unit = {
      drawHex(hexLoc,Some(color),None,scale,false,true,alpha,1.0, rectangle);
    }
    def fillHexWithTexture(hexLoc : HexLoc, texture: String, scale : Double, alpha: Double, rectangle:Boolean = false) : Unit = {
      drawHex(hexLoc,None,Some((texture,"repeat")),scale,false,true,alpha,1.0,rectangle);
    }
    def fillHexWithImage(hexLoc : HexLoc, texture: String, scale: Double, alpha:Double) : Unit = {
      //Adjust so that the image is centered
      val img = textures(texture)
      val pixel = PixelLoc.ofHexLoc(hexLoc,gridSize)
      val oldAlpha = ctx.globalAlpha
      ctx.globalAlpha = alpha
      ctx.drawImage(img, pixel.x - img.width*scale*0.5, pixel.y - img.height*scale*0.5, img.width*scale, img.height*scale)
      ctx.globalAlpha = oldAlpha
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

    def drawTile(hexLoc: HexLoc, loc: Loc, tile: Tile, scaleBy: Double, alpha: Double = 1.0, showLoc: Boolean = false) : Unit = {
      val scale = scaleBy*tileScale
      tile.terrain match {
        case Wall => fillHex(hexLoc, "white", scale, alpha=alpha)
        case Ground | StartHex(_) =>
          val texture = BoardMaps.groundImage(boardNames(boardIdx))
          fillHexWithTexture(hexLoc, texture, scale, alpha=alpha)
          //fillHex(hexLoc, "green", scale)
        case Water =>
          val texture = BoardMaps.waterImage(boardNames(boardIdx))
          fillHexWithTexture(hexLoc, texture, scale, alpha=alpha)
        case Flood =>
          val texture = BoardMaps.waterImage(boardNames(boardIdx))
          fillHexWithTexture(hexLoc, texture, scale, alpha=0.8*alpha)
          strokeHex(hexLoc, "#bbeeff", scale, alpha=alpha, lineWidth=2.0*scaleBy)
        case Graveyard =>
          val texture = BoardMaps.groundImage(boardNames(boardIdx))
          fillHexWithTexture(hexLoc, texture, scale, alpha=alpha)
          fillHex(hexLoc, "#dddddd", scale, alpha=alpha)
          strokeHex(hexLoc, "#666666", scale, alpha=0.5*alpha, lineWidth=2.0*scaleBy)
          val img = "img_terrain_graveyard" + deterministicRandom(loc.x,loc.y,4)
          fillHexWithImage(hexLoc, img, scale, alpha=alpha)
        case SorceryNode =>
          val texture = BoardMaps.groundImage(boardNames(boardIdx))
          fillHexWithTexture(hexLoc, texture, scale, alpha=alpha)
          fillHex(hexLoc, "#ffcc88", scale, alpha=0.15*alpha)
          strokeHex(hexLoc, "#ffcc88", scale, alpha=0.5*alpha, lineWidth=2.0*scaleBy)
          val img = "img_terrain_leyline"
          fillHexWithImage(hexLoc, img, scale, alpha=alpha)
        case Teleporter =>
          val texture = BoardMaps.groundImage(boardNames(boardIdx))
          fillHexWithTexture(hexLoc, texture, scale, alpha=alpha)
          fillHex(hexLoc, "#ccff88", scale, alpha=0.15*alpha)
          strokeHex(hexLoc, "#ccff88", scale, alpha=0.5*alpha, lineWidth=2.0*scaleBy)
          val img = "img_terrain_teleporter"
          fillHexWithImage(hexLoc, img, scale, alpha=alpha)
        case Spawner(_) =>
          val texture = BoardMaps.groundImage(boardNames(boardIdx))
          fillHexWithTexture(hexLoc, texture, scale, alpha=alpha)
          fillHex(hexLoc, "#ff55ff", scale, alpha=0.15*alpha)
          strokeHex(hexLoc, "#ff55ff", scale, alpha=0.5*alpha, lineWidth=2.0*scaleBy)
          val img = "img_terrain_spawner"
          fillHexWithImage(hexLoc, img, scale, alpha=alpha)
        case Mist =>
          fillHex(hexLoc, "#6E754C", scale, alpha=alpha)
          strokeHex(hexLoc, "#6E754C", scale, alpha=0.8*alpha, lineWidth=2.0*scaleBy)
        case Earthquake =>
          fillHex(hexLoc, "#846935", scale, alpha=alpha)
          strokeHex(hexLoc, "#846935", scale, alpha=0.8*alpha, lineWidth=2.0*scaleBy)
        case Firestorm =>
          fillHex(hexLoc, "#e25822", scale, alpha=alpha)
          strokeHex(hexLoc, "#e25822", scale, alpha=0.8*alpha, lineWidth=2.0*scaleBy)
        case Whirlwind =>
          fillHex(hexLoc, "#8ECCCC", scale, alpha=alpha)
          strokeHex(hexLoc, "#8ECCCC", scale, alpha=0.8*alpha, lineWidth=2.0*scaleBy)
      }
      if(showLoc) {
        text(loc.toString, PixelLoc.ofHexLoc(hexLoc, gridSize)+PixelVec(0, -gridSize/2.0), "black")
      }
    }

    def displayNameOfPieceName(pieceName: PieceName): String = {
      Units.pieceMap(pieceName).shortDisplayName
    }

    def drawPiece(hexLoc : HexLoc, scale : Double, side: Option[Side], pieceName:PieceName, alpha: Double = 1.0) : Unit = {
      val pieceColor =
        side match {
          case None => "#cccccc"
          case Some(S0) =>
            if(Units.pieceMap(pieceName).isNecromancer) "#bbddff"
            else "#ccccff"
          case Some(S1) =>
            if(Units.pieceMap(pieceName).isNecromancer) "#ffccaa"
            else "#ffbbbb"
        }
      fillHex(hexLoc, pieceColor, scale, alpha = alpha)
      strokeHex(hexLoc, "black", scale, alpha = 0.2 * alpha)
    }

    def drawSpell(hexLoc: HexLoc, scale : Double, side: Option[Side], spellId: Option[SpellId], subLabel: Option[String] = None, alpha: Double = 1.0) : Unit = {
      val color =
        side match {
          case None => "#aaaaaa"
          case Some(S0) => "#aaccff"
          case Some(S1) => "#ffccaa"
        }
      val strokeColor =
        side match {
          case None => "#aaaaaa"
          case Some(S0) => "#0000bb"
          case Some(S1) => "#bb0000"
        }
      fillHex(hexLoc, color, scale, rectangle=true, alpha=alpha)
      strokeHex(hexLoc, strokeColor, scale, alpha=0.4*alpha, rectangle=true)
      spellId.foreach { spellId =>
        externalInfo.spellsRevealed.get(spellId) match {
          case None =>
            text("Unknown", PixelLoc.ofHexLoc(hexLoc,gridSize) + PixelVec(0,-4.0), "black", alpha=alpha)
            text("Spell", PixelLoc.ofHexLoc(hexLoc,gridSize) + PixelVec(0,7.0), "black", alpha=alpha)
          case Some(spellName) =>
            val spell = Spells.spellMap(spellName)
            subLabel match {
              case None => text(spell.shortDisplayName, hexLoc, "black", alpha=alpha)
              case Some(subLabel) =>
                text(spell.shortDisplayName, PixelLoc.ofHexLoc(hexLoc,gridSize) + PixelVec(0,-4.0), "black", alpha=alpha)
                text(subLabel, PixelLoc.ofHexLoc(hexLoc,gridSize) + PixelVec(0,7.0), "black", alpha=alpha)
            }
        }
      }
    }


    def locAndScaleOfPiece(board: BoardState, piece: Piece) : (HexLoc,Double) = {
      val loc = piece.loc
      board.pieces(loc) match {
        case Nil => assertUnreachable()
        case _ :: Nil =>
          val hexLoc = ui.MainBoard.hexLoc(loc)
          (hexLoc,pieceScale)
        case p1 :: _ :: Nil  =>
          val hexLoc = ui.MainBoard.hexLoc(loc)
          if(piece.id == p1.id) (hexCorner(hexLoc,smallPieceOffset,5), smallPieceScale)
          else                 (hexCorner(hexLoc,smallPieceOffset,2), smallPieceScale)
        case p1 :: p2 :: _ :: Nil =>
          val hexLoc = ui.MainBoard.hexLoc(loc)
          if(piece.id == p1.id)      (hexCorner(hexLoc,smallPieceOffset,5), smallPieceScale)
          else if(piece.id == p2.id) (hexCorner(hexLoc,smallPieceOffset,3), smallPieceScale)
          else                       (hexCorner(hexLoc,smallPieceOffset,1), smallPieceScale)
        case _ => assertUnreachable()
      }
    }

    def drawSidebar(
      piece: Option[Piece] = None,
      stats: Option[PieceStats] = None,
      side: Option[Side] = None,
      tile: Option[Tile] = None,
      spell: Option[Int] = None,
      spellTargets: Option[Option[SpellOrAbilityTargets]] = None,
    ) = {
      val hexLoc = ui.Sidebar.origin
      tile match {
        case None => ()
        case Some(tile) => drawTile(hexLoc, Loc.zero, tile, 6.0)
      }
      stats match {
        case None => ()
        case Some(stats) => drawPiece(hexLoc, pieceScale*6.0, side, stats.name)
      }
      spell match {
        case None => ()
        case Some(_) => drawSpell(hexLoc, spellScale*6.0, side, spellId=None)
      }
      var row_idx = 0
      def row(n:Int) : PixelLoc = {
        PixelLoc.ofHexLoc(hexLoc, gridSize) + PixelVec(0, 16.0*(n-6))
      }
      def show(s:String, color:String = "black") : Unit = {
        text(s, row(row_idx), color, fontSize = 11)
        row_idx = row_idx + 1
      }
      stats match {
        case None => ()
        case Some(stats) =>
          show(stats.displayName)
          if(stats.isNecromancer && stats.defense.isEmpty) {
            // Immortal necromancer cannot be killed
          } else if(stats.isNecromancer && stats.swarmMax > 1) {
            if(side.map(_.opp) == client.ourSide)
              show("If ANY of them die, you win the board!")
            else
              show("If ANY of them die, you lose the board!")
          } else if(stats.isNecromancer) {
            if(side.map(_.opp) == client.ourSide)
              show("If it dies, you win the board!")
            else
              show("If it dies, you lose the board!")
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

          val aStr = stats.attackEffect match {
            case None => "Can't attack"
            case Some(Damage(n)) => "Attack: " + n + " damage"
            case Some(Unsummon) => "Attack: Unsummon"
            case Some(Kill) => "Attack: Instant kill"
            case Some(Enchant(_)) => ""
            case Some(TransformInto(_)) => ""
          }
          if(stats.numAttacks <= 1) {
            val cantAttack =
              piece match {
                case None => false
                case Some(p) =>
                  if(stats.isLumbering && p.hasMoved) {
                    true
                  } else {
                    p.actState match {
                      case Moving(_) => false
                      case Attacking(attacks) =>
                        assert(attacks <= 1)
                        attacks == 1
                      case Spawning | DoneActing => true
                    }
                  }
              }
            show(aStr, if(cantAttack) "grey" else "black")
          } else {
            val (alreadyAttackedStr, attackColor) =
              piece match {
                case None => ("", "black")
                case Some(p) =>
                  if(stats.isLumbering && p.hasMoved) {
                    ("", "grey")
                  } else {
                    p.actState match {
                      case Moving(_) => ("", "black")
                      case Attacking(attacks) =>
                        if(attacks == stats.numAttacks) {
                          ("", "grey")
                        } else if(attacks > 0) {
                          (" (" + (stats.numAttacks - attacks) + " left)", "red")
                        } else {
                          ("", "black")
                        }
                      case Spawning | DoneActing => ("", "grey")
                    }
                  }
              }
            show(aStr + " (" + stats.numAttacks + "x/turn)" + alreadyAttackedStr, attackColor)
          }

          stats.defense match {
            case None => show("Cannot be killed")
            case Some(d) =>
              val (dmgStr, healthColor) =
                piece match {
                  case None => ("", "black")
                  case Some(p) =>
                    if(p.damage > 0) {
                      ((d-p.damage) + " / ", "red")
                    } else {
                      ("", "black")
                  }
                }
              show("Health: " + dmgStr + d, healthColor)
          }

          if(stats.moveRange == 0) {
            show("Cannot move")
          } else {
            val hexStr = if(stats.moveRange == 1) "hex" else "hexes"
            val (usedStr, speedColor) =
              piece match {
                case None => ("", "black")
                case Some(p) =>
                    p.actState match {
                      case Moving(stepsUsed) =>
                        if(stepsUsed == stats.moveRange) {
                          ("", "grey")
                        } else if(stepsUsed > 0) {
                          (" (" + (stats.moveRange - stepsUsed) + " left)", "red")
                        } else {
                          ("", "black")
                        }
                      case Attacking(_) => ("", "grey")
                      case Spawning | DoneActing => ("", "grey")
                    }
              }
            show("Speed: " + stats.moveRange + " " + hexStr + "/turn" + usedStr, speedColor)
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
          stats.spawnRange match {
            case None => ()
            case Some(spawnRange) =>
              assert(spawnRange >= 1 && spawnRange <= 2)
              if(spawnRange == 1)
                show("Spawner (friendly units can spawn adjacent)")
              else
                show("Greater Spawner (spawn units at range 2)")
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
          if(stats.canBlink) {
            show("Can move to reinforcements.")
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
            ability.desc.foreach { line =>
              show(line)
            }
          }
          piece.foreach { piece =>
            piece.modsWithDuration.foreach { mod =>
              if(mod.mod.isGood) {
                show("")
                show(mod.mod.displayName + ":",color="green")
                show(mod.mod.desc,color="green")
              }
              else {
                show("")
                show(mod.mod.displayName + ":",color="red")
                show(mod.mod.desc,color="red")
              }
            }
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
            case Mist =>
              show("Terrain: Mist")
              show("Non-persistent units are unsummoned at the end of the turn.")
            case Earthquake =>
              show("Terrain: Earthquake")
              show("Only passable by unit types with at least two speed")
            case Firestorm =>
              show("Terrain: Firestorm")
              show("Only passable by unit types with at least four health")
            case Flood =>
              show("Terrain: Flood")
              show("Only passable by flying unit types.")
            case Whirlwind =>
              show("Terrain: Whirlwind")
              show("Only passable by persistent unit types.")
          }
      }
      spell match {
        case None => ()
        case Some(spellId) =>
          show("")
          show("")
          externalInfo.spellsRevealed.get(spellId) match {
            case None =>
              show("Unknown Spell")
            case Some(spellName) =>
              val spell = Spells.spellMap(spellName)
              val typStr =
                spell.spellType match {
                  case NormalSpell => ""
                  case Sorcery => " (Sorcery)"
                  case Cantrip => " (Cantrip)"
                  case DoubleCantrip => "" //double cantrip spell description covers this
                }
              show(spell.displayName + typStr)
              spellTargets.foreach { targets => targets match {
                case None => show("(discarded)")
                case Some(_) => show("(played)")
              }}
              spell.desc.foreach { line =>
                show(line)
              }
              spell.spellType match {
                case NormalSpell => ()
                case Sorcery =>
                  show("");
                  show("Sorcery (costs 1 sorcery power to play)")
                  show("If sorcery power is negative, will auto-discard")
                  show("spells at end of turn.")
                case Cantrip =>
                  show("");
                  show("Cantrip (gain 1 sorcery power when played)")
                case DoubleCantrip => () //double cantrip spell description covers this
              }
              if(spell.spawnPhaseOnly) {
                show("")
                show("This spell's effect occurs only AFTER all normal")
                show("movement and attacks. (For convenience, the UI")
                show("will still allow moves and attacks that would be")
                show("legal without this spell's effect)")
              }
          }
      }
    }

    ctx.setTransform(1,0,0,1,0,0)
    ctx.clearRect(0.0, 0.0, canvas.width.toDouble, canvas.height.toDouble)

    //Background fill based on whose turn it is
    val backgroundColor = board.side match {
      case S0 => "#eeeeff"
      case S1 => "#ffeeee"
    }
    ctx.fillStyle = backgroundColor
    ctx.globalAlpha = 1.0
    ctx.fillRect(0.0, 0.0, canvas.width.toDouble, canvas.height.toDouble)

    ctx.translate(UI.translateOrigin.dx, UI.translateOrigin.dy)

    //Board title
    text(
      boardNames(boardIdx) + " (board " + (boardIdx+1) + ")",
      ui.TitleInfo.origin,
      "black", textAlign="center", textBaseline="top", fontSize=16, style="bold"
    )

    //Forward and backward buttons
    if(boardIdx > 0)
      text("<- Prev Board", ui.PrevBoard.hexLoc(ui.PrevBoard.locs(0)), "black", textAlign="center", textBaseline="top", fontSize=12)
    if(boardIdx < boardNames.length-1)
      text("Next Board ->", ui.NextBoard.hexLoc(ui.NextBoard.locs(0)), "black", textAlign="center", textBaseline="top", fontSize=12)

    def textColorOfSide(side: Side): String = {
      side match {
        case S0 => "#000099"
        case S1 => "#770000"
      }
    }
    def rectColorOfSide(side: Side): String = {
      side match {
        case S0 => "#e0e0ff"
        case S1 => "#ffe0e0"
      }
    }

    //Game info text
    Side.foreach { side =>
      val infoLoc = ui.TopInfo.getHexLoc(side)
      val pixelLoc = PixelLoc.ofHexLoc(infoLoc, gridSize)
      val color = textColorOfSide(side)
      val mana = boards.foldLeft(game.mana(side) + game.manaThisRound(side)) { case (sum,board) =>
        sum + board.curState.manaThisRound(side)
      }
      val newMana = boards.foldLeft(game.extraManaPerTurn(side)) { case (sum, board) =>
        sum + board.curState.endOfTurnMana(side)
      }

      def textAtLoc(s: String, dpx: Double, dpy: Double, style:String = "normal") =
        text(s, pixelLoc+PixelVec(dpx,dpy), color, textAlign="left", fontSize = 14, style = style)

      if(side == board.side) {
        ctx.fillStyle = rectColorOfSide(side)
        ctx.fillRect(pixelLoc.x - 10.0, pixelLoc.y - 26.0, 470.0, 24.0)
      }

      textAtLoc(side.toColorName + " Team:", 0.0, -10.0, style = "bold")
      textAtLoc("Wins: " + game.wins(side) + "/" + game.targetNumWins, 110.0, -10.0)
      textAtLoc("Souls: " + mana + " (+" + newMana + "/turn)", 200.0, -10.0)
      if(side == game.curSide) {
        textAtLoc("Techs Used: " + game.numTechsThisTurn + "/" + (game.extraTechsAndSpellsThisTurn + 1), 345.0, -10.0)
      }
    }

    // Terrain
    for((terrain, i) <- ui.Terrain.terrains.zipWithIndex) {
      val loc = Loc(i,0)
      val alpha = if(board.tiles.find({ tile => tile.terrain == terrain}).isEmpty) 1.0 else 0.2
      drawTile(ui.Terrain.hexLoc(loc), loc, Tile(terrain), ui.Terrain.gridSizeScale*0.8, alpha=alpha)
    }

    //Board-specific info text
    Side.foreach { side =>
      val infoLoc = ui.BoardInfo.getHexLoc(side)
      val pixelLoc = PixelLoc.ofHexLoc(infoLoc, gridSize)
      val color = textColorOfSide(side)

      if(side == board.side) {
        ctx.fillStyle = rectColorOfSide(side)
        ctx.fillRect(pixelLoc.x - 5.0, pixelLoc.y - 16.0, 345.0, 24.0)
      }

      def textAtLoc(s: String, dpx: Double, dpy: Double, style:String = "normal", size:Int = 14) =
        text(s, pixelLoc+PixelVec(dpx,dpy), color, textAlign="left", fontSize = size, style = style)

      textAtLoc("Souls: +" + board.endOfTurnMana(side) + "/turn", 0, 0)
      textAtLoc("Souls in play: " + board.manaOnBoard(side), 100, -7, size=11)
      textAtLoc("Souls spent: " + board.totalCosts(side), 100, 3, size=11)

      if(side == board.side) {
        if(board.sorceryPower < 0)
          textAtLoc("Sorcery Power: " + board.sorceryPower, 220, 0, style = "bold")
        else
          textAtLoc("Sorcery Power: " + board.sorceryPower, 220, 0)
      }
    }

    //Clock
    if(client.gotFatalError) {
      text("Connection error", ui.Clock.origin, "red", textAlign="left", style = "bold", fontSize=24)
    }
    else {
      timeLeft match {
        case None => ()
        case Some(timeLeft) =>
          fillHex(ui.Clock.origin, "#dddddd", tileScale)
          strokeHex(ui.Clock.origin, "#666666", tileScale, lineWidth=1.0)
          val pauseText = if(client.isPaused) { "Unpause" } else { "Pause" }
          text(pauseText, ui.Clock.origin, "black")

          val seconds: Int = Math.floor(timeLeft).toInt
          val timeStr = {
            if(seconds < 0) "-" + ((-seconds) / 60).toString + ":" + "%02d".format((-seconds) % 60)
            else (seconds / 60).toString + ":" + "%02d".format(seconds % 60)
          }
          val clockStr = board.side.toColorName + " Team Time left: " + timeStr
          text(clockStr, ui.Clock.origin + HexVec(1,0), textColorOfSide(board.side), textAlign="left", style = "bold", fontSize=16)
      }
    }

    // Options
    fillHex(ui.Options.origin, "#dddddd", tileScale)
    strokeHex(ui.Options.origin, "#666666", tileScale, lineWidth=1.0)
    val coord_ploc = PixelLoc.ofHexLoc(ui.Options.origin, gridSize)
    text(if(client.showCoords) "Hide" else "Show", coord_ploc + PixelVec(0, -4.0), "black")
    text("Coords", coord_ploc + PixelVec(0, 7.0), "black")

    //End turn hex
    if(game.isBoardDone(boardIdx)) {
      fillHex(ui.EndTurn.origin, "#ff99ff", tileScale, alpha=1.0)
      strokeHex(ui.EndTurn.origin, "#ff00ff", tileScale, lineWidth=2.0)
    }
    else {
      fillHex(ui.EndTurn.origin, "#dddddd", tileScale)
      strokeHex(ui.EndTurn.origin, "#666666", tileScale, lineWidth=1.0)
    }
    text("End Turn", ui.EndTurn.origin, "black")

    //Resign board hex
    fillHex(ui.ResignBoard.origin, "#dddddd", tileScale)
    strokeHex(ui.ResignBoard.origin, "#666666", tileScale, lineWidth=1.0)
    val resign_ploc = PixelLoc.ofHexLoc(ui.ResignBoard.origin, gridSize)
    text("Resign", resign_ploc + PixelVec(0,-4.0), "black")
    text("Board", resign_ploc + PixelVec(0,7.0), "black")

    //Extra tech and spells hex
    fillHex(ui.ExtraTechAndSpell.origin, "#dddddd", tileScale)
    strokeHex(ui.ExtraTechAndSpell.origin, "#666666", tileScale, lineWidth=1.0)
    val extra_ploc = PixelLoc.ofHexLoc(ui.ExtraTechAndSpell.origin, gridSize)
    text("Buy Extra", extra_ploc + PixelVec(0,-6.0), "black")
    text("Tech+Spell", extra_ploc + PixelVec(0,5.0), "black")
    text("(" + game.extraTechCost + " souls)", extra_ploc + PixelVec(0, 16.0), "black")

    //Reinforcements
    Side.foreach { side =>
      val locsAndContents = ui.Reinforcements.getHexLocsAndContents(side,board)
      locsAndContents.foreach { case (hexLoc,pieceName,count) =>
        drawPiece(hexLoc, pieceScale, Some(side), pieceName)
        val label = displayNameOfPieceName(pieceName)
        text(label, PixelLoc.ofHexLoc(hexLoc,gridSize) + PixelVec(0,-4.0), "black")
        text("x " + count, PixelLoc.ofHexLoc(hexLoc,gridSize) + PixelVec(0,8.0), "black")
      }
    }

    //SpellHand
    Side.foreach { side =>
      val locs = ui.SpellHand.getLocs(side)
      board.spellsInHand(side).zipWithIndex.foreach { case (spellId, idx) =>
        //If the user has so many spells that it overflows, then we just won't draw them all...
        if(idx < locs.length) {
          val hexLoc = ui.SpellHand.hexLoc(locs(idx))
          drawSpell(hexLoc, spellScale, Some(side), Some(spellId))
        }
      }
    }

    //Dead pieces
    {
      val locsAndContents = ui.DeadPieces.getHexLocsAndContents(board)
      if(locsAndContents.length > 0) {
        text("Dead pieces", PixelLoc.ofHexLoc(ui.DeadPieces.descLoc, gridSize), "black")
      }
      locsAndContents.foreach { case (hexLoc,_,pieceName,side) =>
        val label = displayNameOfPieceName(pieceName)
        drawPiece(hexLoc, pieceScale, Some(side), pieceName)
        text(label, PixelLoc.ofHexLoc(hexLoc,gridSize), "black")
      }
    }

    //Used spells
    {
      val locsAndContents = ui.SpellPlayed.getHexLocsAndContents(board)
      if(locsAndContents.length > 0) {
        text("Used spells", PixelLoc.ofHexLoc(ui.SpellPlayed.descLoc, gridSize), "black")
      }
      locsAndContents.foreach { case (hexLoc, spellId, side, targets) =>
        val subLabel = targets match {
          case None => "(discard)"
          case Some(_) => ""
        }
        drawSpell(hexLoc, spellScale, Some(side), Some(spellId), subLabel=Some(subLabel))
      }
    }


    //Techs
    val techLocs = ui.Tech.getHexLocs(game)
    for(i <- 0 until game.techLine.length) {
      val techState = game.techLine(i)
      val hexLoc = techLocs(i)
      val (fillColor, strokeColor) =
        (techState.level(S0), techState.level(S1)) match {
          case ((TechLocked | TechUnlocked), (TechLocked | TechUnlocked)) => ("#aaaaaa", "#888888")
          case (TechAcquired, TechAcquired) => ("#A3C2A3", "#9ED49E")
          case ((TechLocked | TechUnlocked), TechAcquired) => ("#ffbbbb", "#ff3333")
          case (TechAcquired, (TechLocked | TechUnlocked)) => ("#bbbbff", "#3333ff")
        }

      fillHex(hexLoc, fillColor, techScale, alpha=1.0)
      strokeHex(hexLoc, strokeColor, techScale, lineWidth=2.0)

      text(techState.shortDisplayName, PixelLoc.ofHexLoc(hexLoc, gridSize), "black")
      techState.techNumber.foreach { techNumber =>
        text("#" + techNumber, PixelLoc.ofHexLoc(hexLoc, gridSize) + PixelVec(0,10), "black")
      }
      text(techState.level(S0).toUnicodeSymbol, PixelLoc.ofHexLoc(hexLoc + HexVec.corners(4) * techInteriorScale, gridSize), "blue")
      text(techState.level(S1).toUnicodeSymbol, PixelLoc.ofHexLoc(hexLoc + HexVec.corners(0) * techInteriorScale, gridSize), "red")
    }

    //Spells
    client.ourSide.foreach { ourSide =>
      var availableLabeled = false
      var upcomingLabeled = false
      for(i <- (-1) until ui.SpellChoice.size) {
        val (spellId, spellSide) = game.resolveSpellChoice(i, client.ourSide)
        spellId match {
          case None => ()
          case Some(spellId) =>
            val labelLoc = PixelLoc.ofHexLoc(ui.SpellChoice.hexLoc(ui.SpellChoice.getLoc(i-1)), gridSize)
            if(game.spellsToChoose.contains(spellId)) {
              if(!availableLabeled) {
                text("Available", labelLoc + PixelVec(0,-4.0), "black")
                text("Spells", labelLoc + PixelVec(0,7.0), "black")
                availableLabeled = true
              }
            } else {
              if(!upcomingLabeled) {
                text("Upcoming", labelLoc + PixelVec(0,-4.0), "black")
                text("Spells", labelLoc + PixelVec(0,7.0), "black")
                upcomingLabeled = true
              }
            }
            val hexLoc = ui.SpellChoice.hexLoc(ui.SpellChoice.getLoc(i))

            if(board.hasGainedSpell && game.spellsToChoose.contains(spellId))
              drawSpell(hexLoc, spellScale, spellSide, Some(spellId), alpha=0.4)
            else
              drawSpell(hexLoc, spellScale, spellSide, Some(spellId), alpha=1.0)
        }
      }
    }


    //Terrain
    board.tiles.foreachi {case (loc, tile) =>
      val hexLoc = ui.MainBoard.hexLoc(loc)
      drawTile(hexLoc,loc,tile, 1.0,showLoc=client.showCoords)
    }

    val preSpawnBoard = boards(boardIdx).preSpawnState()
    preSpawnBoard.tiles.foreachi { case (loc, tile) =>
      if(tile.terrain != board.tiles(loc).terrain && tile.terrain != Ground) {
        val hexLoc = ui.MainBoard.hexLoc(loc)
        drawTile(hexLoc,loc,tile, 1.0, alpha=0.4,showLoc=client.showCoords)
      }
    }

    def pieceCanStillDoThings(piece: Piece): Boolean = {
      piece.actState match {
        case Spawning | DoneActing => false
        case Attacking(numAttacks) => numAttacks < piece.curStats(board).numAttacks
        case Moving(stepsUsed) => !(piece.curStats(board).isLumbering && stepsUsed > 0)
      }
    }
    def pieceHasNotDoneThings(piece: Piece): Boolean = {
      piece.actState match {
        case Spawning | DoneActing | Attacking(_) => false
        case Moving(stepsUsed) => stepsUsed == 0
      }
    }

    def getAttackStringAndColor(baseStats: PieceStats, curStats: PieceStats, actState: ActState): (String,String) = {
      val effectStr = (curStats.attackEffect, baseStats.attackEffect) match {
        case (None,None) => ""
        case (None,Some(_)) => "A-"
        case (Some(Damage(n)),_) => "A" + n
        case (Some(Unsummon),_) => "A*"
        case (Some(Kill),_) => "AK"
        case (Some(_),_) => "A!"
      }
      val displayedAttacks = actState match {
        case Spawning | DoneActing => curStats.numAttacks
        case Moving(_) => curStats.numAttacks
        case Attacking(attacksUsed) =>
          if(attacksUsed >= curStats.numAttacks) curStats.numAttacks
          else curStats.numAttacks - attacksUsed
      }
      val countStr = {
        if(displayedAttacks > 1 && curStats.attackEffect!=None) displayedAttacks.toString
        else ""
      }
      val colorFromEnchantments: Option[String] = {
        if(curStats.numAttacks > baseStats.numAttacks) Some("green")
        else {
          (curStats.attackEffect, baseStats.attackEffect) match {
            case (Some(Damage(c)), Some(Damage(b))) => if(c < b) Some("magenta") else if(c == b) None else Some("green")
            case (None, Some(_)) => Some("magenta")
            case (Some(Unsummon), Some(Damage(_))) => Some("green")
            case _ => None
          }
        }
      }

      val color = {
        actState match {
          case Spawning | DoneActing => "#777777"
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
      (curStats.defense, baseStats.defense) match {
        case (None, None) => ("", "black")
        case (Some(_), None) | (None, Some(_)) => assertUnreachable()
        case (Some(dcur), Some(dbase)) =>
          val str = (if(curStats.isPersistent) "P" else "H") + (dcur - damage)
          val color = {
            if(damage > 0 || dcur < dbase) "magenta"
            else if(curStats.isPersistent && !baseStats.isPersistent) "green"
            else if(dcur > dbase) "green"
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
          case Spawning | DoneActing | Attacking(_) => curStats.moveRange
          case Moving(stepsUsed) =>
            if(stepsUsed >= curStats.moveRange) curStats.moveRange
            else curStats.moveRange - stepsUsed
        }
      }

      val str = {
        if(curStats.canBlink) "M*"
        else if(!curStats.isFlying && curStats.moveRange == 1) ""
        else if(curStats.isFlying && curStats.moveRange == 1) "F"
        else if(curStats.isFlying) "F" + displayedMoveRange
        else "M" + displayedMoveRange
      }
      val color = {
        actState match {
          case Spawning | DoneActing => "#777777"
          case Attacking(_) =>
            if(curStats.canBlink) "black"
            else "#777777"
          case Moving(stepsUsed) =>
            if(curStats.canBlink) "black"
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
    def drawBoardPiece(piece: Piece, board: BoardState, alpha: Double = 1.0): Unit = {
      val (loc,scale) = locAndScaleOfPiece(board,piece)
      val baseStats = piece.baseStats
      val curStats = piece.curStats(board)
      val label = baseStats.shortDisplayName

      drawPiece(loc, scale, Some(piece.side), curStats.name, alpha = alpha)

      if(piece.modsWithDuration.exists { mod => mod.mod.isGood }) {
        piece.side match {
          case S0 =>
            fillHex(loc, "#ccffff", scale, alpha=0.50 * alpha)
            strokeHex(loc, "#22eeff", scale, lineWidth=1.0, alpha = alpha)
          case S1 =>
            fillHex(loc, "#ffeecc", scale, alpha=0.50 * alpha)
            strokeHex(loc, "#ffaa44", scale, lineWidth=1.0, alpha = alpha)
        }
      }
      else if(piece.modsWithDuration.exists { mod => !mod.mod.isGood }) {
        fillHex(loc, "#bb00bb", scale, alpha=0.15 * alpha)
        strokeHex(loc, "magenta", scale, lineWidth=0.4, alpha = alpha)
      }

      val (aStr,aColor) = getAttackStringAndColor(baseStats,curStats,piece.actState)
      val (dStr,dColor) = getDefenseStringAndColor(baseStats,curStats,piece.damage)
      val (rStr,rColor) = getRangeStringAndColor(baseStats,curStats)
      val (mStr,mColor) = getMoveStringAndColor(baseStats,curStats,piece.actState)

      //Multiple pieces in same hex
      if(board.pieces(piece.loc).length > 1) {
        text(label, PixelLoc.ofHexLoc(loc,gridSize) + PixelVec(0,-3.0), "black", fontSize=8, alpha = alpha)
        text(aStr, PixelLoc.ofHexLoc(loc,gridSize) + PixelVec(-5.0,5.0), aColor, fontSize=8, alpha = alpha)
        text(dStr, PixelLoc.ofHexLoc(loc,gridSize) + PixelVec(5.0,5.0), dColor, fontSize=8, alpha = alpha)
      }
      //One piece in hex
      else {
        if(baseStats.name == Units.zombie.name && curStats.isBaseStats) {
          text(label, PixelLoc.ofHexLoc(loc,gridSize) + PixelVec(0,-4.0), "black", alpha = alpha)
        }
        else {
          text(label, PixelLoc.ofHexLoc(loc,gridSize) + PixelVec(0,-8.0), "black", alpha = alpha)
          text(aStr, PixelLoc.ofHexLoc(loc,gridSize) + PixelVec(-10.0,2.0), aColor, alpha = alpha)
          text(dStr, PixelLoc.ofHexLoc(loc,gridSize) + PixelVec(10.0,2.0), dColor, alpha = alpha)
          text(rStr, PixelLoc.ofHexLoc(loc,gridSize) + PixelVec(-10.0,12.0), rColor, alpha = alpha)
          text(mStr, PixelLoc.ofHexLoc(loc,gridSize) + PixelVec(10.0,12.0), mColor, alpha = alpha)
        }
      }

      if(piece.side != game.curSide) {
        if(piece.damage > 0)
          strokeHex(loc, "magenta", scale, alpha = alpha)
      }
      else {
        if(pieceHasNotDoneThings(piece))
          strokeHex(loc, "green", scale, lineWidth=2.5, alpha = alpha)
        else if(pieceCanStillDoThings(piece))
          strokeHex(loc, "orange", scale, lineWidth=2.5, alpha = alpha)
      }
    }

    board.pieces.foreach { pieces =>
      pieces.foreach { piece =>
        drawBoardPiece(piece,board)
      }
    }
    preSpawnBoard.pieces.foreachi { case (loc,pieces) =>
      if(board.pieces(loc).isEmpty) {
        pieces.foreach { piece =>
          drawBoardPiece(piece,preSpawnBoard,alpha=0.3)
        }
      }
    }

    // Draw translucent special tiles
    board.tiles.foreachi { case (loc, tile) =>
      val hexLoc = ui.MainBoard.hexLoc(loc)
      tile.terrain match {
        case Graveyard =>
          val img = "img_terrain_graveyard" + deterministicRandom(loc.x,loc.y,4)
          fillHexWithImage(hexLoc, img, scale=1.0, alpha=0.4)
        case SorceryNode =>
          val img = "img_terrain_leyline"
          fillHexWithImage(hexLoc, img, scale=1.0, alpha=0.4)
        case Teleporter =>
          val img = "img_terrain_teleporter"
          fillHexWithImage(hexLoc, img, scale=1.0, alpha=0.4)
        case Spawner(_) =>
          val img = "img_terrain_spawner"
          fillHexWithImage(hexLoc, img, scale=1.0, alpha=0.4)
      }
    }

    def drawPath(path: Vector[Loc], overrideStart: Option[HexLoc] = None): Unit = {
      if(path.length > 0) {
        ctx.globalAlpha = 1.0
        ctx.fillStyle = "black"
        ctx.setLineDash(scala.scalajs.js.Array(5.0, 10.0))
        ctx.beginPath()
        move(overrideStart.getOrElse(ui.MainBoard.hexLoc(path(0))))
        for(i <- 1 to path.length-1) {
          line(ui.MainBoard.hexLoc(path(i)))
        }
        ctx.stroke()
        ctx.closePath()
        ctx.setLineDash(scala.scalajs.js.Array())
      }
    }

    def highlightHex(hexLoc: HexLoc, scale: Double = tileScale, fillColor: String = "yellow", strokeColor: String = "black", alpha: Double = 1.0, rectangle: Boolean = false) = {
      fillHex(hexLoc, fillColor, scale, alpha=0.15 * alpha, rectangle)
      strokeHex(hexLoc, strokeColor, scale, alpha=0.5 * alpha, lineWidth=1.5, rectangle)
    }
    def highlightPiece(piece: Piece, fillColor: String = "yellow", strokeColor: String = "black", alpha: Double = 1.0) = {
      val (targetLoc,targetScale) = locAndScaleOfPiece(board,piece)
      fillHex(targetLoc, fillColor, targetScale, alpha=0.15 * alpha)
      strokeHex(targetLoc, strokeColor, targetScale, alpha=0.5 * alpha, lineWidth=1.5)
    }

    def findLocOfPiece(pieceSpec: PieceSpec): Option[Loc] = {
      //First try the current board
      board.findPiece(pieceSpec) match {
        case Some(piece) => Some(piece.loc)
        case None =>
          //Then try to see if it was killed or unsummoned
          board.killedThisTurn.findMap { case (spec,_,_,loc) => if(spec == pieceSpec) Some(loc) else None } match {
            case Some(loc) => Some(loc)
            case None =>
              board.unsummonedThisTurn.findMap { case (spec,_,_,loc) => if(spec == pieceSpec) Some(loc) else None } match {
                case Some(loc) => Some(loc)
                case None =>
                  //Then try the start of turn board
                  boards(boardIdx).initialStateThisTurn.findPiece(pieceSpec).map { piece => piece.loc }
              }
          }
      }
    }

    def highlightUndoneAction(action: PlayerAction): Unit = {
      action match {
        case Movements(movements) =>
          movements.foreach { case Movement(spec,path) =>
            drawPath(path)
            findLocOfPiece(spec).foreach { loc =>
              strokeHex(ui.MainBoard.hexLoc(loc), "black", tileScale)
            }
          }
        case Attack(aSpec,tSpec) =>
          findLocOfPiece(aSpec).foreach { loc =>
            strokeHex(ui.MainBoard.hexLoc(loc), "black", tileScale)
          }
          findLocOfPiece(tSpec).foreach { loc =>
            fillHex(ui.MainBoard.hexLoc(loc), "magenta", tileScale, alpha=0.1)
            strokeHex(ui.MainBoard.hexLoc(loc), "magenta", tileScale)
          }
        case Spawn(_,_) =>
          ui.Reinforcements.getLocs(board.side).foreach { loc =>
            strokeHex(ui.Reinforcements.hexLoc(loc), "black", tileScale, alpha=0.5)
          }
        case ActivateTile(loc) =>
          strokeHex(ui.MainBoard.hexLoc(loc), "black", tileScale)
        case ActivateAbility(spec,_,targets) =>
          List(spec,targets.target0,targets.target1).foreach { spec =>
            findLocOfPiece(spec).foreach { loc =>
              strokeHex(ui.MainBoard.hexLoc(loc), "black", tileScale)
            }
          }
        case Blink(_, src) =>
          strokeHex(ui.MainBoard.hexLoc(src), "black", tileScale)
        case Teleport(_,src,dest) =>
          strokeHex(ui.MainBoard.hexLoc(src), "black", tileScale)
          strokeHex(ui.MainBoard.hexLoc(dest), "black", tileScale)
        case PlaySpell(_,targets) =>
          List(targets.target0,targets.target1).foreach { spec =>
            findLocOfPiece(spec).foreach { loc =>
              strokeHex(ui.MainBoard.hexLoc(loc), "black", tileScale)
            }
          }
        case DiscardSpell(_) =>
          ()
      }
    }

    def highlightUndoneGeneralAction(action: GeneralBoardAction): Unit = {
      action match {
        case BuyReinforcement(pieceName) =>
          ui.Reinforcements.getHexLocsAndContents(board.side,board).foreach { case (hexLoc,name,_) =>
            if(name == pieceName)
              strokeHex(hexLoc, "black", tileScale, alpha=0.5)
          }
          ui.Tech.getHexLocsAndContents(game).foreach { case (hexLoc,techState) =>
            techState.tech match {
              case PieceTech(name) =>
                if(name == pieceName)
                  highlightHex(hexLoc,scale=techScale)
            }
          }
        case GainSpell(spellId) =>
          val idx = game.spellsToChoose.indexOf(spellId)
          if(idx >= 0) {
            val hexLoc = ui.SpellChoice.hexLoc(ui.SpellChoice.getLoc(idx))
            strokeHex(hexLoc, "black", tileScale, alpha=0.5, rectangle=true)
            highlightHex(hexLoc,scale=spellScale, rectangle=true)
          }
      }
    }

    def highlightUndoneActionsForPieceSpec(pieceSpec: PieceSpec): Unit = {
      boards(boardIdx).findLocalPieceUndoActions(pieceSpec) match {
        case None => ()
        case Some(actions) => actions.foreach(highlightUndoneAction)
      }
    }
    def highlightUndoneActionsForSpell(spellId: SpellId): Unit = {
      boards(boardIdx).findSpellUndoActions(spellId) match {
        case Some(actions) => actions.foreach(highlightUndoneAction)
        case None =>
          boards(boardIdx).findGainSpellUndoAction(spellId) match {
            case Some(action) => highlightUndoneGeneralAction(action)
            case None => ()
          }
      }
    }

    def canClickOnTech(techIdx: Int): Boolean = {
      game.tryIsLegal(PerformTech(game.curSide,techIdx)).isSuccess ||
      game.techLine(techIdx).startingLevelThisTurn(game.curSide) == TechAcquired
    }

    mouseState.mode match {
      case (mode: SelectTargetMouseMode) =>
        mode.pieceTargets.foreach { pieceSpec =>
          board.findPiece(pieceSpec).foreach { piece =>
            highlightPiece(piece,alpha=0.7)
          }
        }
        mode.locTargets.foreach { loc =>
          highlightHex(ui.MainBoard.hexLoc(loc),alpha=0.7)
        }
      case (_: SelectTerrainMouseMode) =>
        ui.Terrain.terrains.zipWithIndex.foreach { case(_,i) =>
          highlightHex(ui.Terrain.hexLoc(Loc(i,0)), alpha=0.7, scale=ui.Terrain.gridSizeScale)
        }

      case (mode: DragPieceToLocMouseMode) =>
        mode.pieceTargets.foreach { pieceSpec =>
          board.findPiece(pieceSpec).foreach { piece =>
            highlightPiece(piece,alpha=0.7)
          }
        }
        mode.locTargets.foreach { loc =>
          highlightHex(ui.MainBoard.hexLoc(loc),alpha=0.7)
        }
        mouseState.dragTarget match {
          case MousePiece(spec,_) =>
            board.findPiece(spec) match {
              case None => ()
              case Some(piece) =>
                if(mode.pieceTargets.contains(piece.spec)) {
                  highlightPiece(piece,alpha=0.7)
                  mouseState.hovered.getLoc().foreach { hoverLoc =>
                    if(mode.locTargets.contains(hoverLoc)) {
                      highlightHex(ui.MainBoard.hexLoc(hoverLoc),alpha=0.7)
                    }
                  }
                }
            }
          case MouseTile(loc) =>
            if(mode.locTargets.contains(loc)) {
              highlightHex(ui.MainBoard.hexLoc(loc),alpha=0.7)
            }
          case _ => ()
        }

      case (mode: NormalMouseMode) =>
        //Highlight mouse's target on mouse hover
        mouseState.hovered match {
          case MouseNone => ()
          case MouseSpellHand(spellId,side,_) =>
            drawSidebar(side=Some(side), spell=Some(spellId))
          case MouseSpellChoice(spellId,side,_) =>
            drawSidebar(side=side, spell=Some(spellId))
          case MouseSpellPlayed(infoOpt, _) =>
            infoOpt match {
              case None => ()
              case Some(info) =>
                drawSidebar(side=Some(info.side), spell=Some(info.spellId), spellTargets=Some(info.targets))
                info.targets match {
                  case None => highlightUndoneAction(DiscardSpell(info.spellId))
                  case Some(targets) => highlightUndoneAction(PlaySpell(info.spellId, targets))
                }
            }
          case MouseTile(loc) =>
            drawSidebar(tile=Some(board.tiles(loc)))
          case MouseExtraTechAndSpell(_) =>
            strokeHex(ui.ExtraTechAndSpell.origin, "black", tileScale, alpha=0.5)
          case MouseEndTurn(_) =>
            strokeHex(ui.EndTurn.origin, "black", tileScale, alpha=0.5)
          case MouseResignBoard(_) =>
            strokeHex(ui.ResignBoard.origin, "black", tileScale, alpha=0.5)
          case MousePause(_) =>
            strokeHex(ui.Clock.origin, "black", tileScale, alpha=0.5)
          case MouseCoords(_) =>
            strokeHex(ui.Options.origin, "black", tileScale, alpha=0.5)
          case MousePrevBoard =>
            if(boardIdx > 0)
              text("<- Prev Board", ui.PrevBoard.hexLocs(0), "darkgreen", textAlign="center", textBaseline="top", fontSize=12)
          case MouseNextBoard =>
            if(boardIdx < boardNames.length-1)
              text("Next Board ->", ui.NextBoard.hexLocs(0), "darkgreen", textAlign="center", textBaseline="top", fontSize=12)
          case MouseTerrain(terrain,loc) =>
            strokeHex(ui.Terrain.hexLoc(loc), "black", ui.Terrain.gridSizeScale, alpha=0.5)
            drawSidebar(tile=Some(Tile(terrain)))
          case MouseTech(techIdx,loc) =>
            if(canClickOnTech(techIdx)) {
              strokeHex(ui.Tech.hexLoc(loc), "black", tileScale, alpha=0.5)
            }
            drawSidebar(stats=Some(game.techLine(techIdx).tech.pieceStats))
          case MouseReinforcement(pieceNameOpt,side,loc) =>
            pieceNameOpt match {
              case None => ()
              case Some(pieceName) =>
                strokeHex(ui.Reinforcements.hexLoc(loc), "black", pieceScale, alpha=0.5)
                if(undoing) {
                  val pieceSpec = board.unsummonedThisTurn.reverse.findMap { case (pieceSpec,name,_,_) =>
                    if(pieceName == name) Some(pieceSpec) else None
                  }
                  pieceSpec match {
                    case Some(pieceSpec) => highlightUndoneActionsForPieceSpec(pieceSpec)
                    case None =>
                      boards(boardIdx).findBuyReinforcementUndoAction(pieceName) match {
                        case Some(action) => highlightUndoneGeneralAction(action)
                        case None => ()
                      }
                  }
                }
                drawSidebar(stats=Some(Units.pieceMap(pieceName)), side=Some(side))
            }
          case MouseDeadPiece(pieceSpec,loc) =>
            strokeHex(ui.DeadPieces.hexLoc(loc), "black", pieceScale, alpha=0.5)
            if(undoing)
              highlightUndoneActionsForPieceSpec(pieceSpec)
            ui.DeadPieces.getSelectedPiece(board, pieceSpec) match {
              case None => ()
              case Some((stats, side)) =>
                drawSidebar(stats=Some(stats), side=Some(side))
            }
          case MousePiece(spec,_) =>
            board.findPiece(spec) match {
              case None => ()
              case Some(piece) =>
                val (loc,scale) = locAndScaleOfPiece(board,piece)
                strokeHex(loc, "black", scale, alpha=0.5)
                drawSidebar(piece=Some(piece), stats=Some(piece.curStats(board)), side=Some(piece.side), tile=Some(board.tiles(piece.loc)))
            }

            if(undoing)
              highlightUndoneActionsForPieceSpec(spec)
        }

        //Draw highlights based on piece selected by mouse click
        mouseState.dragTarget match {
          case MouseNone => ()
          case MouseSpellHand(spellId,_,loc) =>
            if(client.ourSide == Some(game.curSide)) {
              highlightHex(ui.SpellHand.hexLoc(loc), rectangle=true)
              ui.SpellPlayed.getLocs().foreach { loc =>
                highlightHex(ui.SpellPlayed.hexLoc(loc), rectangle=true)
              }
              if(undoing)
                highlightUndoneActionsForSpell(spellId)
              else {
                externalInfo.spellsRevealed.get(spellId).foreach { spellName =>
                  val spell = Spells.spellMap(spellName)
                  spell match {
                    case (spell: TargetedSpell) =>
                      board.pieceById.values.foreach { piece =>
                        spell.tryCanTarget(board.side,piece,board) match {
                          case Failure(_) => ()
                          case Success(()) => highlightPiece(piece)
                        }
                      }
                    case (spell: TerrainAndTileSpell) =>
                      val arbitraryTerrain = Whirlwind
                      board.tiles.foreachLoc { loc =>
                        spell.tryCanTarget(board.side,arbitraryTerrain,loc,board) match {
                          case Failure(_) => ()
                          case Success(()) => highlightHex(ui.MainBoard.hexLoc(loc))
                        }
                      }
                    case (spell: TileSpell) =>
                      board.tiles.foreachLoc { loc =>
                        spell.tryCanTarget(board.side,loc,board) match {
                          case Failure(_) => ()
                          case Success(()) => highlightHex(ui.MainBoard.hexLoc(loc))
                        }
                      }
                    case (spell: PieceAndLocSpell) =>
                      board.pieceById.values.foreach { piece =>
                        spell.tryCanTargetPiece(board.side,piece) match {
                          case Failure(_) => ()
                          case Success(()) => highlightPiece(piece)
                        }
                      }
                    case (_: NoTargetSpell) =>
                      ()
                    case (_: NoEffectSpell) =>
                      ()
                  }
                }
              }
            }

          case MouseSpellChoice(_,_,loc) =>
            if(client.ourSide == Some(game.curSide)) {
              highlightHex(ui.SpellChoice.hexLoc(loc), rectangle=true)
            }
          case MouseSpellPlayed(_,loc) =>
            if(client.ourSide == Some(game.curSide)) {
              if(undoing) {
                highlightHex(ui.SpellPlayed.hexLoc(loc), rectangle=true)
              }
            }
          case MouseTile(_) => ()
          case MouseExtraTechAndSpell(_) =>
            if(client.ourSide == Some(game.curSide)) {
              highlightHex(ui.ExtraTechAndSpell.origin)
            }
          case MouseEndTurn(_) =>
            if(client.ourSide == Some(game.curSide)) {
              highlightHex(ui.EndTurn.origin)
            }
          case MousePause(_) =>
            highlightHex(ui.Clock.origin)
          case MouseCoords(_) =>
            highlightHex(ui.Options.origin)
          case MouseResignBoard(_) =>
            if(client.ourSide == Some(game.curSide)) {
              highlightHex(ui.ResignBoard.origin)
            }
          case MousePrevBoard =>
            if(boardIdx > 0)
              text("<- Prev Board", ui.PrevBoard.hexLocs(0), "cyan", textAlign="center", textBaseline="top", fontSize=12)
          case MouseNextBoard =>
            if(boardIdx < boardNames.length-1)
              text("Next Board ->", ui.NextBoard.hexLocs(0), "cyan", textAlign="center", textBaseline="top", fontSize=12)
          case MouseTerrain(_,loc) =>
            highlightHex(ui.Terrain.hexLoc(loc), scale=ui.Terrain.gridSizeScale)
          case MouseTech(techIdx,loc) =>
            if(client.ourSide == Some(game.curSide)) {
              if(undoing) {
                val techState = game.techLine(techIdx)
                techState.tech match {
                  case PieceTech(pieceName) =>
                    boards(boardIdx).findBuyReinforcementUndoAction(pieceName) match {
                      case Some(action) => highlightUndoneGeneralAction(action)
                      case None => ()
                    }
                }
              }
              else {
                if(canClickOnTech(techIdx)) {
                  highlightHex(ui.Tech.hexLoc(loc))
                }
              }
            }
          case MouseReinforcement(pieceNameOpt,side,loc) =>
            pieceNameOpt match {
              case None => ()
              case Some(pieceName) =>
                if(side==game.curSide && client.ourSide == Some(side)) {
                  highlightHex(ui.Reinforcements.hexLoc(loc),scale=pieceScale)
                  if(!undoing) {
                    val locs = board.legalSpawnLocs(pieceName)
                    for(loc <- locs) {
                      highlightHex(ui.MainBoard.hexLoc(loc))
                    }
                  }
                }
            }
          case MouseDeadPiece(_,loc) =>
            if(client.ourSide == Some(game.curSide)) {
              if(undoing) {
                highlightHex(ui.DeadPieces.hexLoc(loc))
              }
            }
          case MousePiece(spec,_) =>
            board.findPiece(spec) match {
              case None => ()
              case Some(piece) =>
                val (loc,scale) = locAndScaleOfPiece(board,piece)

                if(undoing) {
                  highlightHex(loc, scale)
                }
                else {
                  if(Some(piece.side) == client.ourSide) {
                    //Only stroke and no highlight since we're already getting a highlight from drawing paths.
                    strokeHex(loc, "black", scale, alpha=0.5)

                    //Draw based on what would happen if we released the mouse
                    mouseState.hovered.getLoc().foreach { hoverLoc =>
                      val actions = mode.dragPieceMouseUpActions(mouseState.hovered, hoverLoc, piece, board)
                      actions.foreach {
                        case (_ : Movements) =>
                          //If moving, draw the movement path
                          val path = mode.path
                          drawPath(path.toVector,overrideStart = Some(loc))
                        case (_ : Teleport) =>
                          //If teleporting, highlight the teleport location
                          strokeHex(ui.MainBoard.hexLoc(hoverLoc), "cyan", scale, alpha=0.3, lineWidth=2)
                          fillHex(ui.MainBoard.hexLoc(hoverLoc), "cyan", scale, alpha=0.05)
                        case (_ : Blink) | (_ : Attack) | (_ : Spawn) | (_ : ActivateTile) | (_ : ActivateAbility) | (_ : PlaySpell) | (_ : DiscardSpell) =>
                          ()
                      }
                    }

                    val attackerStats = piece.curStats(board)

                    if(attackerStats.canBlink) {
                      ui.Reinforcements.getLocs(piece.side).foreach { loc =>
                        highlightHex(ui.Reinforcements.hexLoc(loc))
                      }
                    }

                    //Highlight the movement range
                    val moveLocsAndSteps = board.legalMoves(piece)
                    moveLocsAndSteps.foreach { case (loc,_) =>
                      highlightHex(ui.MainBoard.hexLoc(loc))
                    }

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
                        if(canAttack(targetPiece))
                          highlightPiece(targetPiece, fillColor="magenta", strokeColor="magenta")
                      }
                    }

                    //Highlight the hovered piece if attacking it is legal
                    mouseState.hovered.findPiece(board) match {
                      case None => ()
                      case Some(targetPiece) =>
                        if(canAttack(targetPiece))
                          highlightPiece(targetPiece, fillColor="magenta", strokeColor="magenta")
                    }
                  } else {
                    val stats = piece.curStats(board)
                    val moveLocs = board.withinTerrainRange(piece, stats.moveRange)
                    val moveLocsPieceCouldAttackFrom = { if(stats.isLumbering) Set(piece.loc) else moveLocs }
                    var attackLocs = Set[Loc]()
                    moveLocsPieceCouldAttackFrom.foreach { fromLoc =>
                      board.tiles.topology.forEachReachable(fromLoc) { (loc,dist) =>
                        if(dist<=stats.attackRange && board.inBounds(loc)) {
                          attackLocs += loc
                          dist < stats.attackRange
                        } else {
                          false
                        }
                      }
                    }
                    (attackLocs ++ moveLocs).foreach { loc =>
                      val hexLoc = ui.MainBoard.hexLoc(loc)
                      if(moveLocs.contains(loc))
                        highlightHex(hexLoc)
                       else
                        highlightHex(hexLoc,tileScale,fillColor="magenta", strokeColor="magenta")
                    }
                  }
                }
            }
        }
    }

    //Highlight hex tile on mouse hover
    mouseState.hovered.getLoc().foreach { hoverLoc =>
      val (component, rectangle) = mouseState.hovered match {
        case MouseNone => (ui.MainBoard, false)
        case MouseSpellChoice(_,_,_) => (ui.SpellChoice, true)
        case MouseSpellHand(_,_,_) => (ui.SpellHand, true)
        case MouseSpellPlayed(_,_) => (ui.SpellPlayed, true)
        case MousePiece(_,_) => (ui.MainBoard, false)
        case MouseTile(_) => (ui.MainBoard, false)
        case MouseTech(_,_) => (ui.Tech, false)
        case MouseReinforcement(_,_,_) => (ui.Reinforcements, false)
        case MouseDeadPiece(_,_) => (ui.DeadPieces, false)
        case MouseExtraTechAndSpell(_) => (ui.ExtraTechAndSpell, false)
        case MouseEndTurn(_) => (ui.EndTurn, false)
        case MouseNextBoard => (ui.NextBoard, false)
        case MousePrevBoard => (ui.PrevBoard, false)
        case MouseTerrain(_,_) => (ui.Terrain, false)
        case MouseResignBoard(_) => (ui.ResignBoard, false)
        case MousePause(_) => (ui.Clock, false)
        case MouseCoords(_) => (ui.Options, false)
      }
      strokeHex(component.hexLoc(hoverLoc), "black", tileScale*component.gridSizeScale, alpha=0.3, rectangle=rectangle)
    }
  }
}

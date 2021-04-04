package minionsgame.server

import scala.util.{Try,Success,Failure}
import scala.concurrent.Future
import java.util.concurrent.atomic.AtomicInteger
import com.typesafe.config.ConfigFactory
import java.util.Calendar
import java.text.SimpleDateFormat
import java.security.SecureRandom

import akka.actor.{ActorSystem, Actor, ActorRef, Cancellable, Terminated, Props, Status}
import akka.stream.{ActorMaterializer,OverflowStrategy}
import akka.stream.scaladsl.{Flow,Sink,Source}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{ContentTypes,HttpEntity,StatusCodes}
import akka.http.scaladsl.model.ws.{Message,TextMessage}
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.Directives._
import akka.event.Logging

import minionsgame.core._
import RichImplicits._

import akka.http.scaladsl.Http
import play.api.libs.json._
import akka.stream.scaladsl.Keep

private class AIActor(out: ActorRef, game: GameState, doTutorial: Boolean) extends Actor {
  var name = "Igor"
  var side = Some(S1)
  var tutorialStep = 0
  val aiRand = Rand(RandUtils.sha256Long(game.randSeed + "#ai"))
  var nextActionIdSuffix: Int = 0

  def makeActionId(): String = {
    nextActionIdSuffix = nextActionIdSuffix + 1
    name + nextActionIdSuffix.toString
  }
  def chat(message: String) = {
    out ! Protocol.Chat(name, side, true, message)
  }
  override def receive: Receive = {
    case Protocol.ReportTimeLeft(_) => ()
    case Protocol.ReportNewTurn(S1) =>
      if(game.game.winner.isEmpty) {
        if(game.game.souls(S1) > 10) {
          out ! Protocol.DoGameAction(BuyExtraTechAndSpell(S1))
        }
        val techs = game.game.techLine
        val unlockedTechs = techs.indices.filter { i =>
          techs(i).level(S1) == TechAcquired
        }

        val unlockedUnits = unlockedTechs.flatMap { i =>
          techs(i).tech match {
            case PieceTech(pieceName) =>
              val stats = game.externalInfo.pieceMap(pieceName)
              if(stats.name!="zombie" && stats.moveRange > 0 && stats.cost <= game.game.souls(S1) && !stats.attackEffect.isEmpty) {
                Some(stats)
              } else {
                None
              }
            case Copycat => None
            case TechSeller => None
            case Metamagic => None
          }
        }

        var keepBuying = true
        while(keepBuying && !unlockedUnits.isEmpty) {
          val chosenUnit = unlockedUnits(aiRand.nextInt(unlockedUnits.length))
          if(game.boards(0).curState.canFreeBuyPiece(S1,chosenUnit.name)) {
            out ! Protocol.DoBoardAction(0, DoGeneralBoardAction(BuyReinforcement(chosenUnit.name,free=true), makeActionId()))
            Thread.sleep(100)
            keepBuying = true
          }
          else if(game.game.souls(S1) >= chosenUnit.cost) {
            out ! Protocol.DoBoardAction(0, DoGeneralBoardAction(BuyReinforcement(chosenUnit.name,free=false), makeActionId()))
            Thread.sleep(100)
            keepBuying = true
          } else {
            keepBuying = false
          }
        }

        def distance(baseStats: PieceStats, curStats: PieceStats, l1: Loc, l2: Loc): Int = {
          var ans = 1000
          val board = game.boards(0).curState
          board.tiles.topology.forEachReachable(l1) { case (loc, dist) =>
            if(board.inBounds(loc) && board.canWalkOnTile(baseStats, curStats, board.tiles(loc))) {
              if(loc == l2) {
                ans = dist
              }
              true //Can continue moving from this location
            } else {
              false //Can't keep moving from this location
            }
          }
          ans
        }

        def targetLocs(attackRange: Int): List[Loc] = {
          val board = game.boards(0).curState
          val targetLocs = board.pieces.filterLocs { loc =>
            val withinRangeOfEnemy =
              board.pieceById.values.exists { enemyPiece =>
                enemyPiece.side == S0 && board.topology.distance(enemyPiece.loc, loc) <= attackRange
              }
            val alreadyOccupiedByUs = board.pieceById.values.exists { piece => piece.side == S1 && piece.loc == loc }
            val graveyard = board.tiles(loc).terrain == Graveyard
            withinRangeOfEnemy || (graveyard && !alreadyOccupiedByUs)
          }
          targetLocs
        }

        def bestMove(baseStats: PieceStats, curStats: PieceStats, options: Map[Loc,Int]): Option[Loc] = {
          val targets = targetLocs(curStats.attackRange)
          val bestMove = Try(options.minBy({ case (loc, dist) =>
            val distFromTargets = targets.map { target => distance(baseStats, curStats, target, loc) }.min
            (distFromTargets, dist)
          })).toOption
          bestMove.map { case (loc,_) => loc }
        }

        def distanceFromTargets(piece: Piece): Int = {
          val board = game.boards(0).curState
          val targets = targetLocs(piece.curStats(board).attackRange)
          targets.map { target => distance(piece.baseStats, piece.curStats(board), target, piece.loc) }.min
        }

        val sortedPieces = game.boards(0).curState.pieceById.values.filter { piece => piece.side == S1 }.toList.sortBy { p => distanceFromTargets(p) }

          // Move and attack with units
          sortedPieces.foreach { piece =>
            val board = game.boards(0).curState
            val pieceShouldStay = board.tiles(piece.loc).terrain == Graveyard || piece.curStats(board).isNecromancer
            if(!pieceShouldStay) {
              val moves = board.legalMoves(piece)
              val best = bestMove(piece.baseStats, piece.curStats(board), moves)
              board.findPathForUI(piece,pathBias=List(),isRotationPath=false) { case (loc,_) => Some(loc) == best } match {
                case None => ()
                case Some((path, pathMovements)) =>
                  val filteredPathMovements = pathMovements.filter { case (_,path) => path.length > 1 }
                  if(filteredPathMovements.nonEmpty) {
                    val movements = Movements(filteredPathMovements.map { case (pieceSpec,path) => Movement(pieceSpec,path.toVector) })
                    out ! Protocol.DoBoardAction(0, PlayerActions(List(movements), makeActionId()))
                    Thread.sleep(100)
                  } else if(path.length > 1) {
                    val movements = Movements(List(Movement(piece.spec, path.toVector)))
                    out ! Protocol.DoBoardAction(0, PlayerActions(List(movements), makeActionId()))
                    Thread.sleep(100)
                  }
              }
            }

            for(i <- 0 to piece.curStats(board).numAttacks) {
              val b1 = game.boards(0).curState
              val p1 = b1.findPiece(piece.spec).get
              b1.pieces.foreach { enemyPieces =>
                enemyPieces.foreach { enemyPiece =>
                  if(b1.tryLegality(Attack(p1.spec, enemyPiece.spec), game.externalInfo).isSuccess) {
                    out ! Protocol.DoBoardAction(0, PlayerActions(List(Attack(p1.spec, enemyPiece.spec)), makeActionId()))
                    Thread.sleep(100)
                  }
                }
              }
            }
          }

          val reinforcements = scala.util.Random.shuffle(game.boards(0).curState.reinforcements(S1).toSeq.flatMap { case (piecename, n) =>
            List.fill(n)(piecename)
          })

          reinforcements.foreach { piecename =>
            val board = game.boards(0).curState
            val stats = game.externalInfo.pieceMap(piecename)
            val locs = board.legalSpawnLocs(stats).map { x => (x -> 0) }.toMap
            bestMove(stats, stats, locs) match {
              case None => ()
              case Some(bestLoc) =>
                out ! Protocol.DoBoardAction(0, PlayerActions(List(Spawn(bestLoc, piecename)), makeActionId()))
                Thread.sleep(100)
            }
          }

          if(techs(2).level(S1) != TechAcquired) { // Open with initiates
            out ! Protocol.DoGameAction(BuyExtraTechAndSpell(S1))
            out ! Protocol.DoGameAction(PerformTech(S1, 2))
            out ! Protocol.DoGameAction(PerformTech(S1, 2))
          } else {
            val availableTechs = techs.indices.filter { i =>
              techs(i).level(S1) != TechAcquired && (i==0 || techs(i-1).level(S1) != TechLocked) && techs(i).level(S0)!=TechAcquired
            }
            if(availableTechs.length > 0) {
              val chosenTech = availableTechs(aiRand.nextInt(availableTechs.length))
              out ! Protocol.DoGameAction(PerformTech(S1, chosenTech))
            }
          }

          out ! Protocol.DoGameAction(SetBoardDone(0, true))
      }
    case _ => {
      if(doTutorial) {
        val board = game.boards(0).curState
        val techs = game.game.techLine
        if(tutorialStep == 0) {
          tutorialStep = 1
          chat(s"Welcome to Minions of Darkness!")
          chat("You currently occupy the top left of the board.")
          chat("You begin the game with your necromancer and six zombies.")
          chat("Move units by clicking on them and dragging them to a new hex.")
          chat("Undo by right-clicking on the unit.")
          chat("For now, move zombies onto the two nearby graveyards.")
        } else if(tutorialStep == 1 && board.endOfTurnSouls(S0) == 4) {
          tutorialStep = 2
          chat("")
          chat("You now control the first graveyard.")
          chat("Your souls per turn went from 3 to 4.")
          chat("You can see your souls and souls per turn near the top-left of the screen.")
          chat("Each graveyard you control gives you +1 soul at the end of your turn.")
          chat("Souls are the most important resource in the game.")
          chat("You can spend them to buy more units or unlock new types of units.")
          chat("Claim the other nearby graveyard.")
        } else if(tutorialStep <=2 && board.endOfTurnSouls(S0) == 5) {
          tutorialStep = 3
          chat("")
          chat("You now control both graveyards, and earn 5 souls per turn.")
          chat("Just above the board is the spell row. You can choose from one of two spells each turn.")
          chat("Choose a spell now.")
        } else if(tutorialStep == 3 && game.game.boardsWithSpells.getOrElse(0, 0) == 1) {
          tutorialStep = 4
          chat("")
          chat("The spell appears in your hand, to the left of the board.")
          chat("You get one spell per turn, and you can play as many as you want on your turn.")
          chat("You play a spell by click-dragging it to a valid target.")
          chat("You can undo casting the spell by right clicking it near the bottom of the screen")
          chat("You can also undo your choice of spell (if you decide you want the other one) by right-clicking the spell you chose.")
          chat("(You can right-click the spell you chose either in your hand or in the spell row).")
          chat("That's almost it for your first turn! Move your necromancer and other zombies forward.")
          chat("When you're ready, click 'End Turn' (near the upper-right corner of the board).")
        } else if(tutorialStep == 4 && game.game.turnNumber == 2) {
          tutorialStep = 5
          chat("")
          chat("I took my turn; now it's your second turn. Like you, I occupied the nearby graveyards.")
          chat("I also unlocked the 'Initiate' unit")
          chat("You can see the tech line above the spell row")
          chat("There are 23 different units to unlock, and three starting units")
          chat("")
          chat("The first starting unit is the 'Zombie', which you started with 6 of.")
          chat("Zombies are really weak. Their biggest problem is that they are lumbering")
          chat("They can't move and attack in the same turn, so they can only attack things that started next to them")
          chat("They're good at standing on graveyards though!")
          chat("")
          chat("The second starting unit is the 'Acolyte'")
          chat("They can't fight at all, but they are fast, and you can place units around them.")
          chat("(Normally you can only place units next to your Necromancer)")
          chat("Try buying one now by clicking on the Acolyte hex in the tech row")
        } else if(tutorialStep == 5 && board.reinforcements(S0).getOrElse(Units.acolyte.name, 0) == 1) {
          tutorialStep = 6
          chat("")
          chat("The acolyte appears in your reinforcements, to the left of the board")
          chat("Place the acolyte next to your necromancer")
        } else if(tutorialStep == 6 && board.pieceById.values.exists { piece => piece.baseStats.name == Units.acolyte.name && piece.side == S0 }) {
          tutorialStep = 7
          chat("")
          chat("Newly-placed units can't act the turn you place them, but next turn your acolyte will be ready for action.")
          chat("Units cost souls to create (you can see your souls near the upper-left corner of the screen).")
          chat("You had 5 souls from your necromancer and two graveyards, but you spent them all on the acolyte.")
          chat("We're going to spend the souls on teching instead, so we need to un-buy the acolyte.")
          chat("Right click on the acolyte to move it back to your reinforcements.")
        } else if(tutorialStep == 7 && board.reinforcements(S0).getOrElse(Units.acolyte.name, 0) == 1) {
          tutorialStep = 8
          chat("")
          chat("Right click on the acolyte in your reinforcements to get your souls back")
        } else if(tutorialStep == 8 && game.game.souls(S0) == 5) {
          tutorialStep = 9
          chat("")
          chat("The third basic unit is the spire")
          chat("Spires can't move at all, but they have high health and damage, and you can spawn units next to them")
          chat("")
          chat("The unit I unlocked is the 'Initiate', which is an upgrade to the acolyte.")
          chat("Like the acolyte, the initiate moves 2 and has spawn, but he is also a good fighter, even better than the spire.")
          chat("Initiates have one big drawback: like zombies, they are lumbering.")
          chat("")
          chat("To defeat my initiates, you'll want to unlock skeletons.")
          chat("Click on the skeleton hex in the tech line")
        } else if(tutorialStep == 9 && techs(3).level(S0) == TechUnlocked) {
          tutorialStep = 10
          chat("")
          chat("The blue rectangle in the skeleton hex means you've put one point into skeletons.")
          chat("One point lets you go on to the next tech.")
          chat("You need two points in a tech to actually build the unit.")
          chat("You get one free tech point each turn, but you can buy more")
          chat("Click on 'Buy Extra Tech + Spell', to the right of the tech line")
        } else if(tutorialStep == 10 && game.game.extraTechsAndSpellsThisTurn == 1) {
          tutorialStep = 11
          chat("")
          chat("What makes skeletons strong against initiates? Look at the stats of both units.")
          chat("Skeleton do 5 damage per attack, enough to kill an initiate in one hit.")
          chat("And they are *not* lumbering; they can move and attack in the same turn.")
          chat("So in a fight between an initiate and a skeleton, the skeleton will always get the first attack.")
          chat("You can't build units the turn you tech to them.")
          chat("Now click 'Skeleton' again to unlock them")
        } else if(tutorialStep == 11 && techs(3).level(S0) == TechAcquired) {
          tutorialStep = 12
          chat("")
          chat("You've unlocked skeletons, but you can't build them until next turn.")
          chat("You can never build units the turn you tech to them.")
          chat("")
          chat("Every unit counters and is countered by other units. Which ones?")
          chat("Look at the number in the tech hex for a unit.")
          chat("Unit N is strong against units N-1,N-2, and N+3.")
          chat("(And therefore weak against N+1,N+2, and N-3)")
          chat("Build units that counter your enemy's units to win.")
          chat("")
          chat("You should be able to finish this game on your own.")
          chat("Occupy the graveyards, build up an army, and win.")
          chat("There are two ways to win:")
          chat("1) Kill the enemy necromancer")
          chat("2) Occupy 8 of the 10 graveyards at the *start* of your turn")
          chat("Good luck!")
        }
      }
    }
  }
}

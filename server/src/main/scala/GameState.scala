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

sealed trait ScheduleReason
case object NewTurn extends ScheduleReason
case class Pause(isPaused:Boolean) extends ScheduleReason

object Log {
  val timeFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSSZ")
  def log(s: String): Unit = {
    println(timeFormat.format(Calendar.getInstance().getTime()) + " " + s)
  }
}

case class GameState (
  val password: Option[String],
  var secondsPerTurn: SideArray[Double],
  var allMessages: List[String],
  var teamMessages: SideArray[List[String]],
  var spectatorMessages: List[String],
  var isPaused: Boolean,
  val randSeed: Long,
  // TODO serialize Rands
  val numBoards: Int,
  val game: Game,
  var gameSequence: Int,
  //These get repopulated when empty when we need to draw one
  val specialNecrosRemaining: SideArray[List[String]],
  //These get repopulated when empty when we need to draw one
  val spellsRemaining: SideArray[List[String]],
  var nextSpellId: Int,
  var spellMap: Map[Int,SpellName],
  val revealedSpellIds: SideArray[Set[Int]],
  val externalInfo: ExternalInfo,
  val boards: Array[Board],
  val boardNames: Array[String],
  val boardSequences: Array[Int]
) {
  val config = ConfigFactory.parseFile(new java.io.File(Paths.applicationConf))
  val clientHeartbeatPeriodInSeconds = config.getDouble("akka.http.server.clientHeartbeatRate")
  var usernameOfSession: Map[Int,String] = Map()
  var userSides: Map[Int,Option[Side]] = Map()
  var userOuts: Map[Int,ActorRef] = Map()

  val spellRands = SideArray.createTwo(
    Rand(RandUtils.sha256Long(randSeed + "#spell0")),
    Rand(RandUtils.sha256Long(randSeed + "#spell1"))
  )
  val necroRands = SideArray.createTwo(
    Rand(RandUtils.sha256Long(randSeed + "#necro0")),
    Rand(RandUtils.sha256Long(randSeed + "#necro1"))
  )
  def broadcastToSpectators(response: Protocol.Response): Unit = {
    userOuts.foreach { case (sid,out) =>
      if(userSides(sid).isEmpty) out ! response
    }
  }
  def broadcastToSide(response: Protocol.Response, side: Side): Unit = {
    userOuts.foreach { case (sid,out) =>
      if(userSides(sid).contains(side)) out ! response
    }
  }
  def broadcastAll(response: Protocol.Response): Unit = {
    userOuts.foreach { case (_,out) =>
      out ! response
    }
  }
  def broadcastPlayers(): Unit = {
    var spectators = List[String]()
    val players = SideArray.create(List[String]())

    userSides.foreach { case (sid, side) =>
      val username = usernameOfSession(sid)
      side match {
        case None => spectators = spectators :+ username
        case Some(side) => players(side) = players(side) :+ username
      }
    }
    broadcastToSide(Protocol.Players(players,spectators),S0)
    broadcastToSide(Protocol.Players(players,spectators),S1)
    broadcastToSpectators(Protocol.Players(players,spectators))
  }
  def broadcastMessages(): Unit = {
    broadcastToSide(Protocol.Messages(allMessages, teamMessages(S0)), S0)
    broadcastToSide(Protocol.Messages(allMessages, teamMessages(S1)), S1)
    broadcastToSpectators(Protocol.Messages(allMessages, spectatorMessages))
  }

  private def performAndBroadcastGameActionIfLegal(gameAction: GameAction): Try[Unit] = {
    game.doAction(gameAction).map { case () =>
      gameAction match {
        case PayForReinforcement(_, _) | UnpayForReinforcement(_, _) => ()
        case ChooseSpell(_, _) | UnchooseSpell(_, _) => ()
        case BuyExtraTechAndSpell(_) | UnbuyExtraTechAndSpell(_) => ()
        case PerformTech(_, _) |  UndoTech(_, _) | SetBoardDone(_, _) => ()
        case AddUpcomingSpells(_,_) => ()
        case AddWin(side, boardIdx) =>
          allMessages = allMessages :+ ("GAME: Team " + side.toColorName + " won board " + (boardIdx+1) + "!")
          broadcastMessages()
        case ResignBoard(_) =>
          assertUnreachable()
      }
      //If successful, report the event
      gameSequence += 1
      broadcastAll(Protocol.ReportGameAction(gameAction,gameSequence))
    }
  }

  //Called upon performing a sucessful board action - unsets any user flag that the
  //board is done.
  private def maybeUnsetBoardDone(boardIdx: Int): Unit = {
    if(game.isBoardDone(boardIdx)) {
      val gameAction: GameAction = SetBoardDone(boardIdx,false)
      val (_: Try[Unit]) = performAndBroadcastGameActionIfLegal(gameAction)
    }
  }

  private def doResetBoard(boardIdx: Int, canMove: Boolean): Unit = {
    // TODO: Another matter is how exactly you get your advanced Necromancer. The draw 3 choose 1 system is what we're using now, but large numbers of boards + Swarm being active can cause weird edge cases here (in 4 boards with a Swarm active, you only have your old Captain left to choose, rather degenerately)
    // TODO: Pick a unit to keep on reset
    Side.foreach { side =>
      if(specialNecrosRemaining(side).isEmpty)
        specialNecrosRemaining(side) = necroRands(side).shuffle(Units.specialNecromancers.toList).map(_.name)
    }
    val necroNames = SideArray.createFn(side => specialNecrosRemaining(side).head)
    Side.foreach { side =>
      specialNecrosRemaining(side) = specialNecrosRemaining(side).tail
    }
    val reinforcements = SideArray.createFn({ side =>
        val unlocked_initiate =
          game.piecesAcquired(side).get(Units.initiate.name) match {
            case None => false
            case Some(techState) => techState.level(side) == TechAcquired
          }
        if(unlocked_initiate) {
          Map(Units.initiate.name -> 1)
        } else {
          Map(Units.acolyte.name -> 1)
        }
    })
    boards(boardIdx).resetBoard(necroNames, canMove, reinforcements)
    broadcastAll(Protocol.ReportResetBoard(boardIdx,necroNames, canMove, reinforcements))
  }

  private def maybeDoEndOfTurn(scheduleEndOfTurn: ScheduleReason => Unit): Unit = {
    if(game.isBoardDone.forall { isDone => isDone })
      doEndOfTurn(scheduleEndOfTurn)
  }

  private def doAddWin(side: Side, boardIdx: Int): Unit = {
    val gameAction: GameAction = AddWin(side,boardIdx)
    val (_: Try[Unit]) = performAndBroadcastGameActionIfLegal(gameAction)
  }

  private def revealSpellsToSide(side: Side, spellIds: Array[SpellId], revealToSpectators: Boolean = false): Unit = {
    val spellIdsAndNames =
      spellIds.flatMap { spellId =>
        if(revealedSpellIds(side).contains(spellId))
          None
        else
          Some((spellId,spellMap(spellId)))
      }

    spellIdsAndNames.foreach { case (spellId,_) =>
      revealedSpellIds(side) = revealedSpellIds(side) + spellId
    }

    externalInfo.revealSpells(spellIdsAndNames)
    broadcastToSide(Protocol.ReportRevealSpells(spellIdsAndNames),side)
    if(revealToSpectators)
      broadcastToSpectators(Protocol.ReportRevealSpells(spellIdsAndNames))
  }

  def refillUpcomingSpells(): Unit = {
    //Reveal extra spells beyond the end - players get to look ahead a little in the deck
    val extraSpellsRevealed = 10
    Side.foreach { side =>
      var newUpcomingSpells: Vector[Int] = Vector()

      val numSpellsToAdd = numBoards + 1 + extraSpellsRevealed - game.upcomingSpells(side).length
      for(i <- 0 until numSpellsToAdd) {
        val _ = i
        if(spellsRemaining(side).isEmpty)
          spellsRemaining(side) = spellRands(side).shuffle(Spells.createDeck())

        val spellName = spellsRemaining(side)(0)
        val spellId = nextSpellId
        spellsRemaining(side) = spellsRemaining(side).drop(1)
        nextSpellId += 1
        spellMap = spellMap + (spellId -> spellName)
        newUpcomingSpells = newUpcomingSpells :+ spellId
      }
      revealSpellsToSide(side,newUpcomingSpells.toArray)

      val gameAction: GameAction = AddUpcomingSpells(side,newUpcomingSpells.toArray)
      val (_: Try[Unit]) = performAndBroadcastGameActionIfLegal(gameAction)
    }
  }

  def doEndOfTurn(scheduleEndOfTurn: ScheduleReason => Unit): Unit = {
    val oldSide = game.curSide
    val newSide = game.curSide.opp

    //Check win condition and reset boards as needed
    for(boardIdx <- 0 until boards.length) {
      val board = boards(boardIdx)
      if(board.curState.hasWon) {
        doAddWin(oldSide,boardIdx)
        if(game.winner.isEmpty) {
          doResetBoard(boardIdx, true)
        }
      }
    }

    //Accumulate souls on all the boards for the side about to move
    val souls = boards.foldLeft(game.extraSoulsPerTurn(newSide)) { case (sum,board) =>
      sum + board.curState.soulsThisRound(newSide)
    }
    game.addSouls(newSide,souls)

    //Automatically tech if it hasn't happened yet, as a convenience
    var moreAutoTechsToBuy = true
    while(moreAutoTechsToBuy && game.numTechsThisTurn < game.extraTechsAndSpellsThisTurn + 1) {
      val idx = game.techLine.indexWhere { techState => techState.level(oldSide) == TechLocked}
      if(idx >= 0) { //-1 if not found
        performAndBroadcastGameActionIfLegal(PerformTech(oldSide,idx)) match {
          case Success(()) => ()
          case Failure(_) =>
            moreAutoTechsToBuy = false
        }
      }
      else {
        moreAutoTechsToBuy = false
      }
    }

    //Automatically choose spells if it hasn't happened yet, as a convenience
    for(boardIdx <- 0 until numBoards) {
      val board = boards(boardIdx)
      if(!board.curState.hasGainedSpell) {
        game.spellsToChoose.find { spellId => !game.spellsChosen.contains(spellId)}.foreach { spellId =>
          val gameAction: GameAction = ChooseSpell(game.curSide,spellId)
          performAndBroadcastGameActionIfLegal(gameAction)
          val boardAction: BoardAction = DoGeneralBoardAction(GainSpell(spellId),"autospell")
          board.doAction(boardAction,externalInfo)
          boardSequences(boardIdx) += 1
          broadcastAll(Protocol.ReportBoardAction(boardIdx,boardAction,boardSequences(boardIdx)))
        }
      }
    }

    //Discard spells to meet mana requirements
    for(boardIdx <- 0 until numBoards) {
      val board = boards(boardIdx)
      val spellIdsToDiscard = board.curState.spellsToAutoDiscardBeforeEndTurn(externalInfo)
      if(spellIdsToDiscard.nonEmpty) {
        revealSpellsToSide(game.curSide.opp,spellIdsToDiscard.toArray, revealToSpectators = true)
        spellIdsToDiscard.foreach { spellId =>
          val boardAction: BoardAction = PlayerActions(List(DiscardSpell(spellId)),"autodiscard")
          boards(boardIdx).doAction(boardAction,externalInfo)
          boardSequences(boardIdx) += 1
          broadcastAll(Protocol.ReportBoardAction(boardIdx,boardAction,boardSequences(boardIdx)))
        }
      }
    }

    game.endTurn()
    boards.foreach { board => board.endTurn() }

    refillUpcomingSpells()

    for(boardIdx <- 0 until boards.length) {
      val board = boards(boardIdx)
      if(board.curState.hasWon) {
        if(game.winner.isEmpty) {
          doAddWin(newSide,boardIdx)
          if(game.winner.isEmpty) {
            doResetBoard(boardIdx, false)
          }
        }
      }
    }
    broadcastAll(Protocol.ReportNewTurn(newSide))

    //Schedule the next end of turn
    scheduleEndOfTurn(NewTurn)
    game.winner match {
      case Some(winner) =>
        allMessages = allMessages :+ ("GAME: Team " + winner.toColorName + " won the game!")
      case None =>
        game.newTechsThisTurn.foreach { case (side,tech) =>
          allMessages = allMessages :+ ("GAME: Team " + side.toColorName + " acquired new tech: " + tech.displayName)
        }
        allMessages = allMessages :+ ("GAME: Beginning " + newSide.toColorName + " team turn (turn #" + game.turnNumber + ")")
    }
    broadcastMessages()
  }

  def handleQuery(query: Protocol.Query, out: ActorRef, side: Option[Side], scheduleEndOfTurn: ScheduleReason => Unit): Unit = {
    query match {
      case Protocol.Heartbeat(i) =>
        out ! Protocol.OkHeartbeat(i)
      case Protocol.RequestPause(newIsPaused) =>
        scheduleEndOfTurn(Pause(newIsPaused))
      case Protocol.RequestBoardHistory(boardIdx) =>
        if(boardIdx < 0 || boardIdx >= numBoards)
          out ! Protocol.QueryError("Invalid boardIdx")
        else {
          out ! Protocol.ReportBoardHistory(
            boardIdx,
            boards(boardIdx).toSummary(),
            boardSequences(boardIdx)
          )
        }

      case Protocol.DoBoardAction(boardIdx,boardAction) =>
        Log.log("Received board " + boardIdx + " action " + boardAction)
        side match {
          case None =>
            out ! Protocol.QueryError("Cannot perform actions as a spectator")
          case Some(side) =>
            if(boardIdx < 0 || boardIdx >= numBoards)
              out ! Protocol.QueryError("Invalid boardIdx")
            else if(game.winner.nonEmpty)
              out ! Protocol.QueryError("Game is over")
            else if(boards(boardIdx).curState().side != side)
              out ! Protocol.QueryError("Currently the other team's turn")
            else {
              //Some board actions are special and are meant to be server -> client only, or need extra checks
              val specialResult: Try[Unit] = boardAction match {
                case (_: PlayerActions) => Success(())
                case (_: LocalPieceUndo) => Success(())
                case (_: SpellUndo) => Success(())
                case (_: Redo) => Success(())
                case BuyReinforcementUndo(pieceName,_) =>
                  //Check ahead of time if it's legal
                  boards(boardIdx).tryLegality(boardAction,externalInfo).flatMap { case () =>
                    //And if so, go ahead and recover the cost of the unit
                    val gameAction: GameAction = UnpayForReinforcement(side,pieceName)
                    performAndBroadcastGameActionIfLegal(gameAction)
                  }
                case GainSpellUndo(spellId,_) =>
                  //Check ahead of time if it's legal
                  boards(boardIdx).tryLegality(boardAction,externalInfo).flatMap { case () =>
                    //And if so, go ahead and recover the cost of the unit
                    val gameAction: GameAction = UnchooseSpell(side,spellId)
                    performAndBroadcastGameActionIfLegal(gameAction)
                  }
                case DoGeneralBoardAction(generalBoardAction,_) =>
                  generalBoardAction match {
                    case BuyReinforcement(pieceName) =>
                      //Pay for the cost of the unit
                      val gameAction: GameAction = PayForReinforcement(side,pieceName)
                      performAndBroadcastGameActionIfLegal(gameAction)
                    case GainSpell(spellId) =>
                      //Check ahead of time if it's legal
                      boards(boardIdx).tryLegality(boardAction,externalInfo).flatMap { case () =>
                        //Make sure the spell can be chosen
                        val gameAction: GameAction = ChooseSpell(side,spellId)
                        performAndBroadcastGameActionIfLegal(gameAction)
                      }
                  }
              }

              specialResult.flatMap { case () => boards(boardIdx).doAction(boardAction,externalInfo) } match {
                case Failure(e) =>
                  out ! Protocol.QueryError(e.getLocalizedMessage)
                case Success(()) =>
                  //When someone plays or discards a spell legally/successfully, reveal it to the other side.
                  boardAction match {
                    case PlayerActions(actions,_) =>
                      actions.foreach {
                        case PlaySpell(spellId,_) => revealSpellsToSide(game.curSide.opp,Array(spellId), revealToSpectators = true)
                        case DiscardSpell(spellId) => revealSpellsToSide(game.curSide.opp,Array(spellId), revealToSpectators = true)
                        case (_: Movements) | (_: Attack) | (_: Spawn) | (_: ActivateTile) | (_: ActivateAbility) | (_: Blink) | (_: Teleport) => ()
                      }
                    case (_: LocalPieceUndo) | (_: SpellUndo) | (_: BuyReinforcementUndo) | (_: GainSpellUndo) | (_: DoGeneralBoardAction) | (_: Redo) => ()
                  }

                  //If this board was set as done, then since we did an action on it, unset it.
                  maybeUnsetBoardDone(boardIdx)

                  boardSequences(boardIdx) += 1
                  out ! Protocol.OkBoardAction(boardIdx,boardSequences(boardIdx))
                  broadcastAll(Protocol.ReportBoardAction(boardIdx,boardAction,boardSequences(boardIdx)))
              }
            }
        }

      case Protocol.DoGameAction(gameAction) =>
        Log.log("Received game action " + gameAction)
        side match {
          case None =>
            out ! Protocol.QueryError("Cannot perform actions as a spectator")
          case Some(side) =>
            if(game.winner.nonEmpty)
              out ! Protocol.QueryError("Game is over")
            else if(game.curSide != side)
              out ! Protocol.QueryError("Currently the other team's turn")
            else {
              //Some game actions are special and are meant to be server -> client only, or need extra checks
              val specialResult: Try[Unit] = gameAction match {
                case (_: PerformTech) | (_: UndoTech) | (_: SetBoardDone) => Success(())
                case BuyExtraTechAndSpell(_) =>
                  refillUpcomingSpells()
                  Success(())
                case UnbuyExtraTechAndSpell(_) => Success(())
                case ResignBoard(boardIdx) =>
                  //Check ahead of time if it's legal
                  game.tryIsLegal(gameAction).map { case () =>
                    //And if so, reset the board
                    doResetBoard(boardIdx, true)
                    allMessages = allMessages :+ ("GAME: Team " + game.curSide.toColorName + " resigned board " + (boardIdx+1) + "!")
                    broadcastMessages()
                  }
                case (_: PayForReinforcement) | (_: UnpayForReinforcement) | (_: AddWin) | (_: AddUpcomingSpells) |
                    (_: ChooseSpell) | (_: UnchooseSpell) =>
                  Failure(new Exception("Only server allowed to send this action"))
              }
              specialResult.flatMap { case () => game.doAction(gameAction) } match {
                case Failure(e) =>
                  out ! Protocol.QueryError(e.getLocalizedMessage)
                case Success(()) =>
                  gameSequence += 1
                  out ! Protocol.OkGameAction(gameSequence)
                  broadcastAll(Protocol.ReportGameAction(gameAction,gameSequence))
                  game.winner.foreach { winner =>
                    allMessages = allMessages :+ ("GAME: Team " + winner.toColorName + " won the game!")
                    broadcastMessages()
                  }
                  maybeDoEndOfTurn(scheduleEndOfTurn)
              }
            }
        }

      case Protocol.Chat(username, side, allChat, message) =>
        if(allChat) {
          allMessages = allMessages :+ (username + ": " + message)
        } else {
          side match {
            case None => spectatorMessages = spectatorMessages :+ (username + ": " + message)
            case Some(side) => teamMessages(side) = teamMessages(side) :+ (username + ": " + message)
          }
        }
        broadcastMessages()
    }
  }

  def terminateWebsocket(out: ActorRef): Unit = {
    //Websocket closes if you send it Status.Success
    out ! Status.Success("")
  }

  def handleUserJoined(sessionId: Int, username: String, side: Option[Side], out:ActorRef) = {
      usernameOfSession = usernameOfSession + (sessionId -> username)
      userSides = userSides + (sessionId -> side)
      userOuts = userOuts + (sessionId -> out)

      out ! Protocol.Version(CurrentVersion.version)
      out ! Protocol.ClientHeartbeatRate(periodInSeconds=clientHeartbeatPeriodInSeconds)

      val spellIds = side match {
        case None => revealedSpellIds(S0).intersect(revealedSpellIds(S1))
        case Some(side) => revealedSpellIds(side)
      }
      val spellIdsAndNames = spellIds.toArray.map { spellId => (spellId,spellMap(spellId)) }
      out ! Protocol.ReportRevealSpells(spellIdsAndNames)

      out ! Protocol.Initialize(game, boards.map { board => board.toSummary()}, boardNames, boardSequences.clone())
      out ! Protocol.ReportPause(isPaused)
      Log.log("UserJoined: " + username + " Side: " + side)
      broadcastAll(Protocol.UserJoined(username,side))
      broadcastPlayers()
      broadcastMessages()
  }

  def handleUserLeft(sessionId: Int) = {
    if(usernameOfSession.contains(sessionId)) {
      val username = usernameOfSession(sessionId)
      val side = userSides(sessionId)
      broadcastAll(Protocol.UserLeft(username,side))
      val out = userOuts(sessionId)
      usernameOfSession = usernameOfSession - sessionId
      userSides = userSides - sessionId
      userOuts = userOuts - sessionId
      terminateWebsocket(out)
      Log.log("UserLeft: " + username + " Side: " + side)
      broadcastAll(Protocol.UserLeft(username,side))
      broadcastPlayers()
      broadcastMessages()
    }
  }

  def currentSideSecondsPerTurn(): Double = {
    secondsPerTurn(game.curSide)
  }
}

object GameState {
  import minionsgame.core.Protocol._
  implicit val gameStateFormat = Json.format[GameState]

  def create(
    secondsPerTurn: SideArray[Double],
    startingSoulsPerBoard: SideArray[Int],
    extraSoulsPerTurn: SideArray[Int],
    targetWins: Int,
    techSouls: Int,
    maps_opt: Option[List[String]],
    seed_opt: Option[Long],
    password: Option[String]
  ): GameState = {
    val config = ConfigFactory.parseFile(new java.io.File(Paths.applicationConf))

    // Random seeds
    val randSeed:Long = {
      seed_opt match {
        case None =>
          val secureRandom = new SecureRandom()
          secureRandom.nextLong()
        case Some(seed) => seed
      }
    }
    val setupRand = Rand(randSeed)

    //----------------------------------------------------------------------------------
    //GAME AND BOARD SETUP

    val chosenMaps =
      maps_opt match {
        case None =>
          val availableMaps = {
            if(config.getBoolean("app.includeAdvancedMaps"))
              BoardMaps.basicMaps.toList ++ BoardMaps.advancedMaps.toList
            else
              BoardMaps.basicMaps.toList
          }

          if(targetWins > availableMaps.length)
            throw new Exception("Configured for " + targetWins + " boards but only " + availableMaps.length + " available")
          val chosenMaps = setupRand.shuffle(availableMaps).take(targetWins)
        chosenMaps
        case Some(chosenMaps) =>
          val maps = BoardMaps.basicMaps ++ BoardMaps.advancedMaps
          chosenMaps.map { mapName => (mapName, maps(mapName)) }
      }

    val numBoards = chosenMaps.length
    val game = {
      val targetNumWins = targetWins
      val startingSouls = startingSoulsPerBoard.map(x => x*numBoards)
      val techsAlwaysAcquired: Array[Tech] =
        Units.alwaysAcquiredPieces.map { piece => PieceTech(piece.name) }
      val lockedTechs: Array[(Tech,Int)] = {
        val pieceTechs = Units.techPieces.map { piece => PieceTech(piece.name) }
        val allTechs = pieceTechs :+ Copycat
        val techsWithIdx = allTechs.zipWithIndex.map { case (tech, idx) => (tech, idx) }
        if(!config.getBoolean("app.randomizeTechLine"))
          techsWithIdx.toArray
        else {
          //First few techs are always the same
          val numFixedTechs = config.getInt("app.numFixedTechs")
          val fixedTechs = techsWithIdx.take(numFixedTechs).toArray
          //Partition remaining ones randomly into two sets of the appropriate size, the first one getting the rounding up
          val randomized = setupRand.shuffle(techsWithIdx.drop(numFixedTechs).toList)
          var set1 = randomized.take((randomized.length+1) / 2)
          var set2 = randomized.drop((randomized.length+1) / 2)
          //Sort each set independently
          set1 = set1.sortBy { case (_,origIdx) => origIdx }
          set2 = set2.sortBy { case (_,origIdx) => origIdx }
          //Interleave them
          val set1Opt = set1.map { case (tech,origIdx) => Some((tech,origIdx)) }
          val set2Opt = set2.map { case (tech,origIdx) => Some((tech,origIdx)) }
          val interleaved = set1Opt.zipAll(set2Opt,None,None).flatMap { case (s1,s2) => List(s1,s2) }.flatten.toArray
          (fixedTechs ++ interleaved).map { case (tech,origIdx) => (tech,origIdx+1) }
        }
      }
      val extraTechCost = techSouls * numBoards

      val game = Game(
        numBoards = numBoards,
        targetNumWins = targetNumWins,
        startingSide = S0,
        startingSouls = startingSouls,
        extraTechCost = extraTechCost,
        extraSoulsPerTurn = extraSoulsPerTurn,
        techsAlwaysAcquired = techsAlwaysAcquired,
        lockedTechs = lockedTechs
      )
      game
    }

    val (boards,boardNames): (Array[Board],Array[String]) = {
      val boardsAndNames = chosenMaps.toArray.map { case (boardName, map) =>
        val state = map()
        val necroNames = SideArray.create(Units.necromancer.name)
        state.resetBoard(necroNames, true, SideArray.create(Map()))

        //Testing
        {
          /*state.spawnPieceInitial(S0, Units.hell_hound.name, Loc(3,3))
          state.spawnPieceInitial(S0, Units.hell_hound.name, Loc(3,3))
          state.spawnPieceInitial(S0, Units.hell_hound.name, Loc(3,3))
          state.spawnPieceInitial(S0, Units.serpent.name, Loc(3,4))
          state.spawnPieceInitial(S0, Units.void.name, Loc(2,4))
          state.spawnPieceInitial(S0, Units.haunt.name, Loc(4,3))
          state.spawnPieceInitial(S0, Units.elemental.name, Loc(4,4))
          state.mana = 5*/

         /*
         state.spawnPieceInitial(S0, Units.hell_hound.name, Loc(3,4))
         state.spawnPieceInitial(S0, Units.hell_hound.name, Loc(3,4))
         state.spawnPieceInitial(S0, Units.hell_hound.name, Loc(2,4))
         state.spawnPieceInitial(S0, Units.hell_hound.name, Loc(2,4))
         state.spawnPieceInitial(S0, Units.bone_rat.name, Loc(4,3))
         state.spawnPieceInitial(S0, Units.bone_rat.name, Loc(4,3))
         state.spawnPieceInitial(S0, Units.bone_rat.name, Loc(4,3))
         state.spawnPieceInitial(S0, Units.bone_rat.name, Loc(4,4))
         state.spawnPieceInitial(S0, Units.bone_rat.name, Loc(4,4))
          */
         /*state.tiles.foreachi { (loc, tile) =>
           if (tile.terrain == Graveyard) {
             val _ = state.spawnPieceInitial(S0, Units.serpent.name, loc)
           }
         }*/
        /*
        state.spawnPieceInitial(S0, Units.shrieker.name, Loc(5,4))
        state.spawnPieceInitial(S0, Units.witch.name, Loc(6,4))
        state.spawnPieceInitial(S0, Units.fallen_angel.name, Loc(7,4))
        state.spawnPieceInitial(S0, Units.dark_tower.name, Loc(5,5))
        state.spawnPieceInitial(S0, Units.lich.name, Loc(6,5))

        state.spawnPieceInitial(S0, Units.haunt.name, Loc(5,6))

        state.spawnPieceInitial(S1, Units.wight.name, Loc(6,6))

        state.addReinforcementInitial(S0,"zombie")
        state.addReinforcementInitial(S0,"bat")
        state.addReinforcementInitial(S0,"bat")
        state.addReinforcementInitial(S0,"bat")

        state.addReinforcementInitial(S1,"zombie")
        state.addReinforcementInitial(S1,"zombie")
        state.addReinforcementInitial(S1,"bat")
        state.addReinforcementInitial(S1,"bat")
         */
        }

      (Board.create(state), boardName)
    }

    (boardsAndNames.map(_._1),boardsAndNames.map(_._2))
  }
  val boardSequences: Array[Int] = (0 until numBoards).toArray.map { _ => 0}

  new GameState(
    password = password,
    secondsPerTurn = secondsPerTurn,
    allMessages = List(),
    teamMessages = SideArray.create(List()),
    spectatorMessages = List(),
    isPaused = true,
    randSeed = randSeed,
    numBoards = numBoards,
    game = game,
    gameSequence = 0,
    specialNecrosRemaining = SideArray.create(List()),
    spellsRemaining = SideArray.create(List()),
    nextSpellId = 0,
    spellMap = Map(),
    revealedSpellIds = SideArray.create(Set()),
    externalInfo = ExternalInfo.create(),
    boards = boards,
    boardNames = boardNames,
    boardSequences = boardSequences,
  )
  }
}

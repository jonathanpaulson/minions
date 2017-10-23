package minionsgame.server

import scala.util.{Try,Success,Failure}
import scala.concurrent.Future
import java.util.concurrent.atomic.AtomicInteger
import com.typesafe.config.ConfigFactory
import java.util.Calendar
import java.text.SimpleDateFormat

import akka.actor.{ActorSystem, Actor, ActorRef, Cancellable, Terminated, Props, Status}
import akka.stream.{ActorMaterializer,OverflowStrategy}
import akka.stream.scaladsl.{Flow,Sink,Source}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.ws.{Message,TextMessage}
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.Directives._
import akka.event.Logging

import minionsgame.core._
import RichImplicits._

object Paths {
  val applicationConf = "./application.conf"
  val mainPage = "./web/index.html"
  val webjs = "./web/js/"
  val webimg = "./web/img/"
}

object ServerMain extends App {
  //----------------------------------------------------------------------------------
  //LOAD STUFF

  val config = ConfigFactory.parseFile(new java.io.File(Paths.applicationConf))

  implicit val actorSystem = ActorSystem("gameSystem",config)
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = actorSystem.dispatcher

  val timeFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSSZ")
  def log(s: String): Unit = {
    println(timeFormat.format(Calendar.getInstance().getTime()) + " " + s)
  }

  val cwd = new java.io.File(".").getCanonicalPath
  log("Running in " + cwd)

  val interface = config.getString("app.interface")
  val port = config.getInt("app.port")
  val password = if(config.hasPath("app.password")) Some(config.getString("app.password")) else None
  val clientHeartbeatPeriodInSeconds = config.getDouble("akka.http.server.clientHeartbeatRate")

  val rand = Rand()

  //----------------------------------------------------------------------------------
  //GAME AND BOARD SETUP

  val numBoards = config.getInt("app.numBoards")
  val game = {
    val targetNumWins = config.getInt("app.targetNumWins")
    val startingMana = SideArray.create(0)
    startingMana(S0) = config.getInt("app.s0StartingManaPerBoard") * numBoards
    startingMana(S1) = config.getInt("app.s1StartingManaPerBoard") * numBoards
    val techsAlwaysAcquired: Array[Tech] =
      Units.alwaysAcquiredPieces.map { piece => PieceTech(piece.name) }
    val lockedTechs: Array[(Tech,Int)] = {
      if(!config.getBoolean("app.randomizeTechLine"))
        Units.techPieces.zipWithIndex.map { case (piece,i) => (PieceTech(piece.name),i+1) }.toArray
      else {
        //First few techs are always the same
        val numFixedTechs = config.getInt("app.numFixedTechs")
        val fixedTechs = Units.techPieces.zipWithIndex.take(numFixedTechs).toArray
        //Partition remaining ones randomly into two sets of the appropriate size, the first one getting the rounding up
        val randomized = rand.shuffle(Units.techPieces.zipWithIndex.drop(numFixedTechs).toList)
        var set1 = randomized.take((randomized.length+1) / 2)
        var set2 = randomized.drop((randomized.length+1) / 2)
        //Sort each set independently
        set1 = set1.sortBy { case (_,origIdx) => origIdx }
        set2 = set2.sortBy { case (_,origIdx) => origIdx }
        //Interleave them
        val set1Opt = set1.map { case (piece,origIdx) => Some((piece,origIdx)) }
        val set2Opt = set2.map { case (piece,origIdx) => Some((piece,origIdx)) }
        val interleaved = set1Opt.zipAll(set2Opt,None,None).flatMap { case (s1,s2) => List(s1,s2) }.flatten.toArray
        (fixedTechs ++ interleaved).map { case (piece,origIdx) => (PieceTech(piece.name),origIdx+1) }
      }
    }
    val extraTechCost = config.getInt("app.extraTechCostPerBoard") * numBoards
    val extraManaPerTurn = config.getInt("app.extraManaPerTurn")

    val game = Game(
      numBoards = numBoards,
      targetNumWins = targetNumWins,
      startingSide = S0,
      startingMana = startingMana,
      extraTechCost = extraTechCost,
      extraManaPerTurn = extraManaPerTurn,
      techsAlwaysAcquired = techsAlwaysAcquired,
      lockedTechs = lockedTechs
    )
    game
  }
  var gameSequence: Int = 0

  //These get repopulated when empty when we need to draw one
  val specialNecrosRemaining: SideArray[List[String]] = SideArray.create(List())

  //These get repopulated when empty when we need to draw one
  val spellsRemaining: SideArray[List[String]] = SideArray.create(List())
  var nextSpellId: Int = 0
  var spellMap: Map[Int,SpellName] = Map()
  val revealedSpellIds: SideArray[Set[Int]] = SideArray.create(Set())
  val externalInfo: ExternalInfo = ExternalInfo()

  val (boards,boardNames): (Array[Board],Array[String]) = {
    val availableMaps = {
      if(config.getBoolean("app.includeAdvancedMaps"))
        BoardMaps.basicMaps.toList ++ BoardMaps.advancedMaps.toList
      else
        BoardMaps.basicMaps.toList
    }

    if(numBoards > availableMaps.length)
      throw new Exception("Configured for " + numBoards + " boards but only " + availableMaps.length + " available")

    val chosenMaps = rand.shuffle(availableMaps).take(numBoards)

    val boardsAndNames = chosenMaps.toArray.map { case (boardName,map) =>
      val state = map()
      val necroNames = SideArray.create(Units.necromancer.name)
      state.resetBoard(necroNames, true)

      //Testing
      {
       /*state.spawnPieceInitial(S0, Units.hell_hound.name, Loc(3,3))
       state.spawnPieceInitial(S0, Units.hell_hound.name, Loc(3,3))
       state.spawnPieceInitial(S0, Units.hell_hound.name, Loc(3,3))

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
            val _ = state.spawnPieceInitial(S0, Units.fallen_angel.name, loc)
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

  //----------------------------------------------------------------------------------
  //TIME LIMITS
  val secondsPerTurn = config.getDouble("app.secondsPerTurn")
  //Server reports time left every this often
  val reportTimePeriod = 5.0
  def getNow(): Double = {
    System.currentTimeMillis.toDouble / 1000.0
  }
  var endOfTurnTime: Option[Double] = None


  //----------------------------------------------------------------------------------
  //GAME ACTOR - singleton actor that maintains the state of the game being played

  sealed trait GameActorEvent
  case class UserJoined(val sessionId: Int, val username: String, val side: Option[Side], val out: ActorRef) extends GameActorEvent
  case class UserLeft(val sessionId: Int) extends GameActorEvent
  case class QueryStr(val sessionId: Int, val queryStr: String) extends GameActorEvent
  case class ShouldEndTurn(val turnNumberToEnd: Int) extends GameActorEvent
  case class ShouldReportTimeLeft() extends GameActorEvent
  case class StartGame() extends GameActorEvent

  private class GameActor extends Actor {
    //The actor refs are basically the writer end of a pipe where we can stick messages to go out
    //to the players logged into the server
    var usernameOfSession: Map[Int,String] = Map()
    var userSides: Map[Int,Option[Side]] = Map()
    var userOuts: Map[Int,ActorRef] = Map()
    var messages : List[String] = List()

    val timeReportJob: Cancellable = {
      import scala.concurrent.duration._
      import scala.language.postfixOps
      actorSystem.scheduler.schedule(reportTimePeriod seconds, reportTimePeriod seconds) {
        self ! ShouldReportTimeLeft()
      }
    }

    private def broadcastToSpectators(response: Protocol.Response): Unit = {
      userOuts.foreach { case (sid,out) =>
        if(userSides(sid).isEmpty) out ! response
      }
    }
    private def broadcastToSide(response: Protocol.Response, side: Side): Unit = {
      userOuts.foreach { case (sid,out) =>
        if(userSides(sid).contains(side)) out ! response
      }
    }
    private def broadcastAll(response: Protocol.Response): Unit = {
      userOuts.foreach { case (_,out) =>
        out ! response
      }
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
            messages = messages :+ ("Team " + side.toColorName + " won board " + boardIdx + "!")
            broadcastAll(Protocol.Messages(messages))
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
      Side.foreach { side =>
        if(specialNecrosRemaining(side).isEmpty)
          specialNecrosRemaining(side) = rand.shuffle(Units.specialNecromancers.toList).map(_.name)
      }
      val necroNames = SideArray.ofArrayInplace(Array(specialNecrosRemaining(S0).head,specialNecrosRemaining(S1).head))
      specialNecrosRemaining(S0) = specialNecrosRemaining(S0).tail
      specialNecrosRemaining(S1) = specialNecrosRemaining(S1).tail
      boards(boardIdx).resetBoard(necroNames, canMove)
      broadcastAll(Protocol.ReportResetBoard(boardIdx,necroNames, canMove))
    }

    private def maybeDoEndOfTurn(): Unit = {
      if(game.isBoardDone.forall { isDone => isDone })
        doEndOfTurn()
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

    private def refillUpcomingSpells(): Unit = {
      //Reveal extra spells beyond the end - players get to look ahead a little in the deck
      val extraSpellsRevealed = 10
      Side.foreach { side =>
        var newUpcomingSpells: Vector[Int] = Vector()

        val numSpellsToAdd = numBoards + 1 + extraSpellsRevealed - game.upcomingSpells(side).length
        for(i <- 0 until numSpellsToAdd) {
          val _ = i
          if(spellsRemaining(side).isEmpty)
            spellsRemaining(side) = rand.shuffle(Spells.createDeck())

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

    private def doEndOfTurn(): Unit = {
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

      //Accumulate mana on all the boards for the side about to move
      val mana = boards.foldLeft(0) { case (sum,board) =>
        sum + board.curState.manaThisRound(newSide)
      }
      game.addMana(newSide,mana)

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

      game.endTurn()
      boards.foreach { board => board.endTurn() }
      broadcastAll(Protocol.ReportNewTurn(newSide))

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

      //Schedule the next end of turn
      scheduleEndOfTurn(game.turnNumber)
      game.winner match {
        case Some(winner) =>
          messages = messages :+ ("Team " + winner.toColorName + " won the game!")
        case None =>
          game.newTechsThisTurn.foreach { case (side,tech) =>
            messages = messages :+ ("Team " + side.toColorName + " acquired new tech: " + tech.displayName)
          }
          messages = messages :+ ("Beginning " + newSide.toColorName + " team turn (turn #" + game.turnNumber + ")")
      }
      broadcastAll(Protocol.Messages(messages))
    }

    private def getTimeLeftEvent(): Option[Protocol.Response] = {
      if(game.winner.isEmpty) {
        val timeLeft = endOfTurnTime.map { endOfTurnTime => endOfTurnTime - getNow() }
        Some(Protocol.ReportTimeLeft(timeLeft))
      }
      else {
        val (_: Boolean) = timeReportJob.cancel()
        None
      }
    }

    private def maybeBroadcastTimeLeft(): Unit = {
      getTimeLeftEvent().foreach { response =>
        broadcastAll(response)
      }
    }

    private def scheduleEndOfTurn(turnNumber: Int): Unit = {
      import scala.concurrent.duration._
      import scala.language.postfixOps
      endOfTurnTime = Some(getNow() + secondsPerTurn)
      maybeBroadcastTimeLeft()
      val (_: Cancellable) = actorSystem.scheduler.scheduleOnce(secondsPerTurn seconds) {
        //Make sure to do this via sending event to self, rather than directly, to
        //take advantage of the actor's synchronization
        self ! ShouldEndTurn(turnNumber)
      }
    }

    private def handleQuery(query: Protocol.Query, out: ActorRef, side: Option[Side]): Unit = {
      query match {
        case Protocol.Heartbeat(i) =>
          out ! Protocol.OkHeartbeat(i)

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
          log("Received board " + boardIdx + " action " + boardAction)
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
                    out ! Protocol.QueryError("Illegal action: " + e.getLocalizedMessage)
                  case Success(()) =>
                    //When someone plays or discards a spell legally/successfully, reveal it to the other side.
                    boardAction match {
                      case PlayerActions(actions,_) =>
                        actions.foreach {
                          case PlaySpell(spellId,_) => revealSpellsToSide(game.curSide.opp,Array(spellId), revealToSpectators = true)
                          case DiscardSpell(spellId) => revealSpellsToSide(game.curSide.opp,Array(spellId), revealToSpectators = true)
                          case (_: Movements) | (_: Attack) | (_: Spawn) | (_: ActivateTile) | (_: ActivateAbility) | (_: Teleport) => ()
                        }
                      case (_: LocalPieceUndo) | (_: SpellUndo) | (_: BuyReinforcementUndo) | (_: GainSpellUndo) | (_: DoGeneralBoardAction) => ()
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
          log("Received game action " + gameAction)
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
                      messages = messages :+ ("Team " + game.curSide.toColorName + " resigned board " + boardIdx + "!")
                      broadcastAll(Protocol.Messages(messages))
                    }
                  case (_: PayForReinforcement) | (_: UnpayForReinforcement) | (_: AddWin) | (_: AddUpcomingSpells) |
                      (_: ChooseSpell) | (_: UnchooseSpell) =>
                    Failure(new Exception("Only server allowed to send this action"))
                }
                specialResult.flatMap { case () => game.doAction(gameAction) } match {
                  case Failure(e) =>
                    out ! Protocol.QueryError("Illegal action: " + e.getLocalizedMessage)
                  case Success(()) =>
                    gameSequence += 1
                    out ! Protocol.OkGameAction(gameSequence)
                    broadcastAll(Protocol.ReportGameAction(gameAction,gameSequence))
                    game.winner.foreach { winner =>
                      messages = messages :+ ("Team " + winner.toColorName + " won the game!")
                    }
                    broadcastAll(Protocol.Messages(messages))
                    maybeDoEndOfTurn()
                }
              }
          }

      }
    }

    def terminateWebsocket(out: ActorRef): Unit = {
      //Websocket closes if you send it Status.Success
      out ! Status.Success("")
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
        log("UserLeft: " + username + " Side: " + side)
      }
    }

    def joinedOrLeftMessage(username : String, side : Option[Side], joined : Boolean) : String = {
      val sideStr = side match {
        case None => "as a spectator"
        case Some(side) => "team " + side.toColorName
      }
      val joinedOrLeft = if(joined) "joined" else "left"
      return username + " " + joinedOrLeft + " " + sideStr
    }

    override def receive: Receive = {
      case UserJoined(sessionId, username, side, out) =>
        usernameOfSession = usernameOfSession + (sessionId -> username)
        userSides = userSides + (sessionId -> side)
        userOuts = userOuts + (sessionId -> out)

        messages = messages :+ joinedOrLeftMessage(username, side, true)

        out ! Protocol.Version(CurrentVersion.version)
        out ! Protocol.ClientHeartbeatRate(periodInSeconds=clientHeartbeatPeriodInSeconds)

        val spellIds = side match {
          case None => revealedSpellIds(S0).intersect(revealedSpellIds(S1))
          case Some(side) => revealedSpellIds(side)
        }
        val spellIdsAndNames = spellIds.toArray.map { spellId => (spellId,spellMap(spellId)) }
        out ! Protocol.ReportRevealSpells(spellIdsAndNames)

        out ! Protocol.Initialize(game, boards.map { board => board.toSummary()}, boardNames, boardSequences.clone())
        getTimeLeftEvent().foreach { response => out ! response }
        broadcastAll(Protocol.UserJoined(username,side))
        broadcastAll(Protocol.Messages(messages))
        log("UserJoined: " + username + " Side: " + side)

      case UserLeft(sessionId) =>
        if(usernameOfSession.contains(sessionId)) {
          val username = usernameOfSession(sessionId)
          val side = userSides(sessionId)
          messages = messages :+ joinedOrLeftMessage(username, side, false)
          broadcastAll(Protocol.UserLeft(username,side))
          broadcastAll(Protocol.Messages(messages))
          val out = userOuts(sessionId)
          usernameOfSession = usernameOfSession - sessionId
          userSides = userSides - sessionId
          userOuts = userOuts - sessionId
          out ! Status.Success("")
          log("UserLeft: " + username + " Side: " + side)
        }
      case QueryStr(sessionId, queryStr) =>
        if(usernameOfSession.contains(sessionId)) {
          val out = userOuts(sessionId)
          import play.api.libs.json._
          Try(Json.parse(queryStr)) match {
            case Failure(err) => out ! Protocol.QueryError("Could not parse as json: " + err.getLocalizedMessage)
            case Success(json) =>
              json.validate[Protocol.Query] match {
                case (e: JsError) => out ! Protocol.QueryError("Could not parse as query: " + JsError.toJson(e).toString())
                case (s: JsSuccess[Protocol.Query]) =>
                  val query = s.get
                  handleQuery(query, out, userSides(sessionId))
              }
          }
        }
      case ShouldEndTurn(turnNumberToEnd) =>
        if(game.turnNumber == turnNumberToEnd && game.winner.isEmpty) {
          doEndOfTurn()
        }

      case ShouldReportTimeLeft() =>
        maybeBroadcastTimeLeft()

      case StartGame() =>
        refillUpcomingSpells()
        game.startGame()
    }

  }

  val gameActor = actorSystem.actorOf(Props(classOf[GameActor]))
  gameActor ! StartGame()

  //----------------------------------------------------------------------------------
  //WEBSOCKET MESSAGE HANDLING

  val nextSessionId = new AtomicInteger()

  def websocketMessageFlow(username: String, sideStr: Option[String]) : Flow[Message, Message, _] = {
    val side: Option[Side] = sideStr match {
      case Some("0") => Some(S0)
      case Some("1") => Some(S1)
      case Some(s) => throw new Exception("Invalid side: " + s)
      case None => None
    }

    val sessionId = nextSessionId.getAndIncrement()

    //Create output stream for the given user
    val responseBufferSize = 128 //Buffer messages to the user before failing

    //Specifies a sink where the values are made by a flow of Messages
    //and mapping them and then feeding them to the GameActor
    val in: Sink[Message,_] = {
      Flow[Message].collect { message: Message =>
        message match {
          case TextMessage.Strict(text) =>
            Future.successful(text)
          case TextMessage.Streamed(textStream) =>
            textStream.runFold("")(_ + _)
        }
      } .mapAsync(1)((str:Future[String]) => str)
        .map { (str: String) => QueryStr(sessionId,str): GameActorEvent }
        .to(Sink.actorRef[GameActorEvent](gameActor, onCompleteMessage = UserLeft(sessionId)))
    }

    //Specifies a source made by materializing an Actor, where the source's values are those that
    //are fed to the Actor, followed by a map that converts them to text messages
    val out: Source[Message,_] = {
      Source.actorRef[Protocol.Response](responseBufferSize, OverflowStrategy.fail)
        .mapMaterializedValue(actorRef => gameActor ! UserJoined(sessionId,username,side,actorRef))
        .map { response: Protocol.Response =>
          import play.api.libs.json._
          TextMessage(Json.stringify(Json.toJson(response))) : Message
        }
    }

    Flow.fromSinkAndSource(in, out)
  }

  //----------------------------------------------------------------------------------
  //DEFINE WEB SERVER ROUTES

  def maybeRequirePassword(password: Option[String])(f: => Route) = {
    parameter("password".?) { user_password =>
      (user_password, password) match {
        case (None, Some(_)) => complete("Please provide 'password=' in URL")
        case (Some(_), None) | (None, None) => f
        case (Some(x), Some(y)) =>
          if(x==y) f
          else complete("Wrong password")
      }
    }
  }

  val route = get {
    pathEndOrSingleSlash {
      maybeRequirePassword(password) {
        parameter("username".?) { username =>
          username match {
            case None => complete("Please provide 'username=' in URL")
            case Some(_) =>
              getFromFile(Paths.mainPage)
          }
        }
      }
    } ~
    pathPrefix("js") {
      getFromDirectory(Paths.webjs)
    } ~
    pathPrefix("img") {
      getFromDirectory(Paths.webimg)
    }
  } ~
  path("playGame") {
    maybeRequirePassword(password) {
      parameter("username") { username =>
        parameter("side".?) { side =>
          Try(websocketMessageFlow(username,side)) match {
            case Failure(exn) => complete(exn.getLocalizedMessage)
            case Success(flow) => handleWebSocketMessages(flow)
          }
        }
      }
    }
  }

  //----------------------------------------------------------------------------------
  //HERE WE GO!

  val binding = Http().bindAndHandle(route, interface, port)

  binding.onComplete {
    case Failure(e) =>
      log(s"Server http binding failed ${e.getMessage}")
      actorSystem.terminate()
    case Success(binding) =>
      val localAddress = binding.localAddress
      log(s"Server is listening on ${localAddress.getHostName}:${localAddress.getPort}")
      scala.io.StdIn.readLine()
      log("Done")
      actorSystem.terminate()
  }

}

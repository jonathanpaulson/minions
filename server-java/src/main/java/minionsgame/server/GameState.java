package minionsgame.server;

import java.security.SecureRandom;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;

import minionsgame.core.*;
import scala.Option;
import scala.Some;
import scala.None$;
import scala.Tuple2;
import scala.collection.JavaConverters;
import scala.reflect.ClassTag;
import scala.reflect.ClassTag$;
import scala.util.Try;
import scala.util.Success;
import scala.util.Failure;

// ---------------------------------------------------------------------------
// ScheduleReason
// ---------------------------------------------------------------------------

enum ScheduleReasonType {
  NEW_TURN,
  NEW_LIMITS,
  PAUSE
}

class ScheduleReason {
  public final ScheduleReasonType type;
  public final boolean isPaused; // only meaningful when type == PAUSE

  private ScheduleReason(ScheduleReasonType type, boolean isPaused) {
    this.type = type;
    this.isPaused = isPaused;
  }

  public static final ScheduleReason NEW_TURN = new ScheduleReason(ScheduleReasonType.NEW_TURN, false);
  public static final ScheduleReason NEW_LIMITS = new ScheduleReason(ScheduleReasonType.NEW_LIMITS, false);

  public static ScheduleReason pause(boolean isPaused) {
    return new ScheduleReason(ScheduleReasonType.PAUSE, isPaused);
  }

  @Override
  public String toString() {
    switch (type) {
      case NEW_TURN: return "NewTurn";
      case NEW_LIMITS: return "NewLimits";
      case PAUSE: return "Pause(" + isPaused + ")";
      default: return "Unknown";
    }
  }
}

// ---------------------------------------------------------------------------
// Log
// ---------------------------------------------------------------------------

class Log {
  static final SimpleDateFormat timeFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSSZ");

  static void log(String s) {
    System.out.println(timeFormat.format(Calendar.getInstance().getTime()) + " " + s);
  }
}

// ---------------------------------------------------------------------------
// GameState
// ---------------------------------------------------------------------------

@SuppressWarnings("unchecked")
public class GameState {

  // ---------- Configuration ----------
  private final Config config;
  private final double clientHeartbeatPeriodInSeconds;

  // ---------- Fields from Scala case class ----------
  private final Option<String> password;
  private SideArray<Object> secondsPerTurn; // SideArray[Double]
  private List<String> allMessages;
  private final SideArray<Object> teamMessages; // SideArray[List[String]] (Scala List)
  private List<String> spectatorMessages;
  private boolean isPaused;
  private final long randSeed;
  private final int numBoards;
  private final Game game;
  private int gameSequence;
  private final SideArray<Object> spellsRemaining; // SideArray[List[String]] (Scala List)
  private int nextSpellId;
  private scala.collection.immutable.Map<Object, String> spellMap; // Map[Int, SpellName]
  private final SideArray<Object> revealedSpellIds; // SideArray[Set[Int]] (Scala Set)
  private final ExternalInfo externalInfo;
  private final Board[] boards;
  private final String[] boardNames;
  private final int[] boardSequences;

  // ---------- Server-side session management (Java maps) ----------
  private Map<Integer, String> usernameOfSession = new HashMap<>();
  private Map<Integer, Option<Side>> userSides = new HashMap<>();
  private Map<Integer, Consumer<Protocol.Response>> userOuts = new HashMap<>();
  private Map<Integer, Integer> userBoardViewed = new HashMap<>();

  // ---------- Derived random state ----------
  private final SideArray<Rand> spellRands;
  private final SideArray<Rand> necroRands;

  // ---------- Accessors for AI and external code ----------
  public long randSeed() { return randSeed; }
  public Game game() { return game; }
  public Board[] boards() { return boards; }
  public ExternalInfo externalInfo() { return externalInfo; }
  public int numBoards() { return numBoards; }
  public boolean isPaused() { return isPaused; }
  public void setIsPaused(boolean paused) { this.isPaused = paused; }
  public Option<String> password() { return password; }
  public Map<Integer, String> getUsernameOfSession() { return usernameOfSession; }
  public Map<Integer, Option<Side>> getUserSides() { return userSides; }
  public boolean hasSession(int sessionId) { return usernameOfSession.containsKey(sessionId); }
  public Consumer<Protocol.Response> getSessionOut(int sessionId) { return userOuts.get(sessionId); }

  @SuppressWarnings("unchecked")
  public double currentSideSecondsPerTurn() {
    return (Double) secondsPerTurn.apply(game.curSide());
  }

  public SideArray<Object> secondsPerTurn() { return secondsPerTurn; }
  public void setSecondsPerTurn(SideArray<Object> spt) { this.secondsPerTurn = spt; }

  // ---------- Constructor ----------
  public GameState(
      Option<String> password,
      SideArray<Object> secondsPerTurn,
      List<String> allMessages,
      SideArray<Object> teamMessages,
      List<String> spectatorMessages,
      boolean isPaused,
      long randSeed,
      int numBoards,
      Game game,
      int gameSequence,
      SideArray<Object> spellsRemaining,
      int nextSpellId,
      scala.collection.immutable.Map<Object, String> spellMap,
      SideArray<Object> revealedSpellIds,
      ExternalInfo externalInfo,
      Board[] boards,
      String[] boardNames,
      int[] boardSequences
  ) {
    this.password = password;
    this.secondsPerTurn = secondsPerTurn;
    this.allMessages = allMessages;
    this.teamMessages = teamMessages;
    this.spectatorMessages = spectatorMessages;
    this.isPaused = isPaused;
    this.randSeed = randSeed;
    this.numBoards = numBoards;
    this.game = game;
    this.gameSequence = gameSequence;
    this.spellsRemaining = spellsRemaining;
    this.nextSpellId = nextSpellId;
    this.spellMap = spellMap;
    this.revealedSpellIds = revealedSpellIds;
    this.externalInfo = externalInfo;
    this.boards = boards;
    this.boardNames = boardNames;
    this.boardSequences = boardSequences;

    this.config = ConfigFactory.parseFile(new java.io.File(AppPaths.applicationConf));
    this.clientHeartbeatPeriodInSeconds = config.getDouble("akka.http.server.clientHeartbeatRate");

    ClassTag<Rand> randTag = ClassTag$.MODULE$.apply(Rand.class);
    this.spellRands = SideArray$.MODULE$.createTwo(
        Rand.create(RandUtils.sha256Long(randSeed + "#spell0")),
        Rand.create(RandUtils.sha256Long(randSeed + "#spell1")),
        randTag
    );
    this.necroRands = SideArray$.MODULE$.createTwo(
        Rand.create(RandUtils.sha256Long(randSeed + "#necro0")),
        Rand.create(RandUtils.sha256Long(randSeed + "#necro1")),
        randTag
    );
  }

  // ---------- Side helpers ----------
  private static Side S0() { return S0$.MODULE$; }
  private static Side S1() { return S1$.MODULE$; }

  // ---------- Scala List helpers ----------
  @SuppressWarnings("unchecked")
  private static <T> scala.collection.immutable.List<T> scalaListNil() {
    return (scala.collection.immutable.List<T>) scala.collection.immutable.Nil$.MODULE$;
  }

  @SuppressWarnings("unchecked")
  private static <T> scala.collection.immutable.List<T> javaListToScala(List<T> javaList) {
    scala.collection.immutable.List<T> result = scalaListNil();
    for (int i = javaList.size() - 1; i >= 0; i--) {
      result = result.$colon$colon(javaList.get(i));
    }
    return result;
  }

  @SuppressWarnings("unchecked")
  private static <T> List<T> scalaListToJava(scala.collection.immutable.List<T> scalaList) {
    List<T> result = new ArrayList<>();
    scala.collection.Iterator<T> iter = scalaList.iterator();
    while (iter.hasNext()) {
      result.add(iter.next());
    }
    return result;
  }

  // ---------- Broadcast methods ----------
  public void broadcastToSpectators(Protocol.Response response) {
    for (Map.Entry<Integer, Consumer<Protocol.Response>> entry : userOuts.entrySet()) {
      int sid = entry.getKey();
      if (userSides.get(sid).isEmpty()) {
        entry.getValue().accept(response);
      }
    }
  }

  public void broadcastToSide(Protocol.Response response, Side side) {
    for (Map.Entry<Integer, Consumer<Protocol.Response>> entry : userOuts.entrySet()) {
      int sid = entry.getKey();
      Option<Side> userSide = userSides.get(sid);
      if (userSide.isDefined() && userSide.get().equals(side)) {
        entry.getValue().accept(response);
      }
    }
  }

  public void broadcastAll(Protocol.Response response) {
    for (Consumer<Protocol.Response> out : userOuts.values()) {
      out.accept(response);
    }
  }

  @SuppressWarnings("unchecked")
  public void broadcastPlayers() {
    List<String> spectators = new ArrayList<>();
    ClassTag<scala.collection.immutable.List> listTag =
        (ClassTag<scala.collection.immutable.List>) (Object) ClassTag$.MODULE$.apply(scala.collection.immutable.List.class);
    SideArray<scala.collection.immutable.List<Tuple2<String, Object>>> playersAndViewedBoards =
        (SideArray<scala.collection.immutable.List<Tuple2<String, Object>>>) (Object)
        SideArray$.MODULE$.create(scalaListNil(), listTag);

    for (Map.Entry<Integer, Option<Side>> entry : userSides.entrySet()) {
      int sid = entry.getKey();
      Option<Side> side = entry.getValue();
      String username = usernameOfSession.get(sid);
      if (side.isEmpty()) {
        spectators.add(username);
      } else {
        Side s = side.get();
        Tuple2<String, Object> pair = new Tuple2<>(username, (Object) userBoardViewed.get(sid));
        scala.collection.immutable.List<Tuple2<String, Object>> current = playersAndViewedBoards.apply(s);
        playersAndViewedBoards.update(s, current.$colon$colon(pair));
      }
    }

    scala.collection.immutable.List<String> scalaSpectators = javaListToScala(spectators);
    Protocol.Players msg = new Protocol.Players(
        (SideArray<scala.collection.immutable.List<Tuple2<String, Object>>>) (Object) playersAndViewedBoards,
        scalaSpectators
    );
    broadcastToSide(msg, S0());
    broadcastToSide(msg, S1());
    broadcastToSpectators(msg);
  }

  @SuppressWarnings("unchecked")
  public void broadcastMessages() {
    scala.collection.immutable.List<String> scalaAllMessages = javaListToScala(allMessages);
    scala.collection.immutable.List<String> scalaSpectatorMessages = javaListToScala(spectatorMessages);

    scala.collection.immutable.List<String> teamS0 =
        (scala.collection.immutable.List<String>) teamMessages.apply(S0());
    scala.collection.immutable.List<String> teamS1 =
        (scala.collection.immutable.List<String>) teamMessages.apply(S1());

    broadcastToSide(new Protocol.Messages(scalaAllMessages, teamS0), S0());
    broadcastToSide(new Protocol.Messages(scalaAllMessages, teamS1), S1());
    broadcastToSpectators(new Protocol.Messages(scalaAllMessages, scalaSpectatorMessages));
  }

  // ---------- Core game logic ----------

  private boolean performAndBroadcastGameActionIfLegal(GameAction gameAction) {
    @SuppressWarnings("unchecked")
    Try<Object> result = (Try<Object>) (Object) game.doAction(gameAction);
    if (result.isSuccess()) {
      if (gameAction instanceof AddWin) {
        AddWin aw = (AddWin) gameAction;
        allMessages.add("GAME: Team " + aw.side().toColorName() + " won board " + (aw.boardIdx() + 1) + "!");
        broadcastMessages();
      } else if (gameAction instanceof ResignBoard) {
        throw new RuntimeException("Bug - reached code that should be unreachable");
      }
      // PayForReinforcement, UnpayForReinforcement, ChooseSpell, UnchooseSpell,
      // BuyExtraTechAndSpell, UnbuyExtraTechAndSpell, SellTech, UnsellTech,
      // PerformTech, UndoTech, SetBoardDone, AddUpcomingSpells -> no special handling
      gameSequence += 1;
      broadcastAll(new Protocol.ReportGameAction(gameAction, gameSequence));
      return true;
    }
    return false;
  }

  private void maybeUnsetBoardDone(int boardIdx) {
    if (game.isBoardDone()[boardIdx]) {
      GameAction gameAction = new SetBoardDone(boardIdx, false);
      performAndBroadcastGameActionIfLegal(gameAction);
    }
  }

  @SuppressWarnings("unchecked")
  private void doResetBoard(int boardIdx, boolean canMoveFirstTurn, boolean turnEndingImmediatelyAfterReset) {
    int nNecros = 3;
    ClassTag<scala.collection.immutable.List> listTag =
        (ClassTag<scala.collection.immutable.List>) (Object) ClassTag$.MODULE$.apply(scala.collection.immutable.List.class);

    // necroNames = SideArray.createFn(side => necroRands(side).shuffle(Units.specialNecromancers.toList).map(_.name).take(nNecros))
    // We need to use the Java Rand to shuffle the Scala list of special necromancers
    SideArray<scala.collection.immutable.List<String>> necroNames =
        (SideArray<scala.collection.immutable.List<String>>) (Object)
        SideArray$.MODULE$.createTwo(
            shuffleAndTakeNecroNames(S0(), nNecros),
            shuffleAndTakeNecroNames(S1(), nNecros),
            listTag
        );

    ClassTag<scala.collection.immutable.Map> mapTag =
        (ClassTag<scala.collection.immutable.Map>) (Object) ClassTag$.MODULE$.apply(scala.collection.immutable.Map.class);
    SideArray<scala.collection.immutable.Map<String, Object>> reinforcements =
        (SideArray<scala.collection.immutable.Map<String, Object>>) (Object)
        SideArray$.MODULE$.create(
            scala.collection.immutable.Map$.MODULE$.empty(),
            mapTag
        );

    boards[boardIdx].resetBoard(necroNames, canMoveFirstTurn, turnEndingImmediatelyAfterReset, reinforcements, externalInfo);
    broadcastAll(new Protocol.ReportResetBoard(boardIdx, necroNames, canMoveFirstTurn, turnEndingImmediatelyAfterReset, reinforcements));
  }

  @SuppressWarnings("unchecked")
  private scala.collection.immutable.List<String> shuffleAndTakeNecroNames(Side side, int n) {
    PieceStats[] specialNecros = Units$.MODULE$.specialNecromancers();
    List<String> names = new ArrayList<>();
    for (PieceStats ps : specialNecros) {
      names.add(ps.name());
    }
    Rand rand = necroRands.apply(side);
    List<String> shuffled = rand.shuffle(names);
    List<String> taken = shuffled.subList(0, Math.min(n, shuffled.size()));
    return javaListToScala(taken);
  }

  private void maybeDoEndOfTurn(Consumer<ScheduleReason> scheduleEndOfTurn) {
    boolean allDone = true;
    for (boolean isDone : game.isBoardDone()) {
      if (!isDone) { allDone = false; break; }
    }
    if (allDone) {
      doEndOfTurn(scheduleEndOfTurn);
    }
  }

  private void doAddWin(Side side, int boardIdx) {
    performAndBroadcastGameActionIfLegal(new AddWin(side, boardIdx));
  }

  @SuppressWarnings("unchecked")
  private void revealSpellsToSide(Side side, int[] spellIds, boolean revealToSpectators) {
    scala.collection.immutable.Set<Object> revealed =
        (scala.collection.immutable.Set<Object>) revealedSpellIds.apply(side);

    List<Tuple2<Object, String>> spellIdsAndNamesList = new ArrayList<>();
    for (int spellId : spellIds) {
      if (!revealed.contains(spellId)) {
        Option<String> nameOpt = spellMap.get(spellId);
        if (nameOpt.isDefined()) {
          spellIdsAndNamesList.add(new Tuple2<>((Object) spellId, nameOpt.get()));
        }
      }
    }

    Tuple2<Object, String>[] spellIdsAndNames = spellIdsAndNamesList.toArray(new Tuple2[0]);

    for (Tuple2<Object, String> pair : spellIdsAndNames) {
      revealed = (scala.collection.immutable.Set<Object>) revealed.$plus(pair._1());
    }
    revealedSpellIds.update(side, (Object) revealed);

    externalInfo.revealSpells(spellIdsAndNames);
    broadcastToSide(new Protocol.ReportRevealSpells(spellIdsAndNames), side);
    if (revealToSpectators) {
      broadcastToSpectators(new Protocol.ReportRevealSpells(spellIdsAndNames));
    }
  }

  private void revealSpellsToSide(Side side, int[] spellIds) {
    revealSpellsToSide(side, spellIds, false);
  }

  @SuppressWarnings("unchecked")
  public void refillUpcomingSpells() {
    int extraSpellsRevealed = 10;
    Side[] sides = { S0(), S1() };
    for (Side side : sides) {
      List<Integer> newUpcomingSpells = new ArrayList<>();

      scala.collection.immutable.Vector<Object> upcomingForSide =
          (scala.collection.immutable.Vector<Object>) game.upcomingSpells().apply(side);
      int numSpellsToAdd = numBoards + 1 + extraSpellsRevealed - upcomingForSide.length();

      for (int i = 0; i < numSpellsToAdd; i++) {
        scala.collection.immutable.List<String> remaining =
            (scala.collection.immutable.List<String>) spellsRemaining.apply(side);

        if (remaining.isEmpty()) {
          // Reshuffle a new deck
          scala.collection.immutable.List<String> deck = Spells$.MODULE$.createDeck();
          List<String> javaDeck = scalaListToJava(deck);
          Rand rand = spellRands.apply(side);
          List<String> shuffled = rand.shuffle(javaDeck);
          remaining = javaListToScala(shuffled);
        }

        String spellName = remaining.head();
        int spellId = nextSpellId;
        remaining = (scala.collection.immutable.List<String>) remaining.tail();
        spellsRemaining.update(side, (Object) remaining);
        nextSpellId += 1;
        spellMap = spellMap.$plus(new Tuple2<>((Object) spellId, spellName));
        newUpcomingSpells.add(spellId);
      }

      int[] newSpellIdsArray = new int[newUpcomingSpells.size()];
      for (int i = 0; i < newUpcomingSpells.size(); i++) {
        newSpellIdsArray[i] = newUpcomingSpells.get(i);
      }
      revealSpellsToSide(side, newSpellIdsArray);

      GameAction gameAction = new AddUpcomingSpells(side, newSpellIdsArray);
      performAndBroadcastGameActionIfLegal(gameAction);
    }
  }

  @SuppressWarnings("unchecked")
  public void doEndOfTurn(Consumer<ScheduleReason> scheduleEndOfTurn) {
    Side oldSide = game.curSide();
    Side newSide = game.curSide().opp();

    // Discard spells to meet mana requirements
    for (int boardIdx = 0; boardIdx < numBoards; boardIdx++) {
      Board board = boards[boardIdx];
      scala.collection.immutable.List<Object> spellIdsToDiscard =
          board.curState().spellsToAutoDiscardBeforeEndTurn(externalInfo);
      Log.log("Discarding " + spellIdsToDiscard);

      if (spellIdsToDiscard.nonEmpty()) {
        // Convert to int array for reveal
        List<Integer> discardList = new ArrayList<>();
        scala.collection.Iterator<Object> iter = spellIdsToDiscard.iterator();
        while (iter.hasNext()) {
          discardList.add((Integer) iter.next());
        }
        int[] discardArray = new int[discardList.size()];
        for (int i = 0; i < discardList.size(); i++) {
          discardArray[i] = discardList.get(i);
        }

        revealSpellsToSide(game.curSide().opp(), discardArray, true);

        for (int spellId : discardArray) {
          scala.collection.immutable.List<PlayerAction> discardActions =
              scalaListNil();
          discardActions = discardActions.$colon$colon(new DiscardSpell(spellId));
          BoardAction boardAction = new PlayerActions(discardActions, "autodiscard");
          boards[boardIdx].doAction(boardAction, externalInfo);
          boardSequences[boardIdx] += 1;
          broadcastAll(new Protocol.ReportBoardAction(boardIdx, boardAction, boardSequences[boardIdx]));
        }
      }
    }

    // Check win condition and reset boards as needed
    for (int boardIdx = 0; boardIdx < boards.length; boardIdx++) {
      Board board = boards[boardIdx];
      if (board.curState().hasWon()) {
        doAddWin(oldSide, boardIdx);
        if (game.winner().isEmpty()) {
          doResetBoard(boardIdx, true, true);
        }
      }
    }

    // Accumulate souls
    int souls = (Integer) game.extraSoulsPerTurn().apply(newSide);
    for (Board board : boards) {
      souls += (Integer) board.curState().soulsThisRound().apply(newSide);
    }
    game.addSouls(newSide, souls);

    // Automatically tech if it hasn't happened yet
    boolean moreAutoTechsToBuy = true;
    while (moreAutoTechsToBuy && game.usedTechsThisTurn() < game.techsThisTurn()) {
      TechState[] techLine = game.techLine();
      int idx = -1;
      for (int i = 0; i < techLine.length; i++) {
        if (techLine[i].level().apply(oldSide).equals(TechLocked$.MODULE$)) {
          idx = i;
          break;
        }
      }
      if (idx >= 0) {
        boolean success = performAndBroadcastGameActionIfLegal(new PerformTech(oldSide, idx));
        if (!success) {
          moreAutoTechsToBuy = false;
        }
      } else {
        moreAutoTechsToBuy = false;
      }
    }

    // Automatically choose spells if it hasn't happened yet
    for (int boardIdx = 0; boardIdx < numBoards; boardIdx++) {
      Board board = boards[boardIdx];
      Option<Object> bwsOpt = game.boardsWithSpells().get(boardIdx);
      int bwsCount = bwsOpt.isDefined() ? (Integer) bwsOpt.get() : 0;
      if (bwsCount == 0) {
        // Find a spell to choose
        scala.collection.immutable.Vector<Object> spellsToChoose = game.spellsToChoose();
        scala.collection.immutable.Set<Object> spellsChosen = game.spellsChosen();
        scala.collection.Iterator<Object> stcIter = spellsToChoose.iterator();
        while (stcIter.hasNext()) {
          int spellId = (Integer) stcIter.next();
          if (!spellsChosen.contains(spellId)) {
            GameAction chooseAction = new ChooseSpell(game.curSide(), spellId, boardIdx);
            performAndBroadcastGameActionIfLegal(chooseAction);

            BoardAction boardAction = new DoGeneralBoardAction(new GainSpell(spellId), "autospell");
            board.doAction(boardAction, externalInfo);
            boardSequences[boardIdx] += 1;
            broadcastAll(new Protocol.ReportBoardAction(boardIdx, boardAction, boardSequences[boardIdx]));
            break;
          }
        }
      }
    }

    game.endTurn();
    for (Board board : boards) {
      board.endTurn(externalInfo);
    }
    broadcastAll(new Protocol.ReportNewTurn(newSide));

    refillUpcomingSpells();

    // Win at start of turn due to graveyards
    for (int boardIdx = 0; boardIdx < boards.length; boardIdx++) {
      Board board = boards[boardIdx];
      if (board.curState().hasWon()) {
        if (game.winner().isEmpty()) {
          doAddWin(newSide, boardIdx);
          if (game.winner().isEmpty()) {
            doResetBoard(boardIdx, false, false);
          }
        }
      }
    }

    // Schedule the next end of turn
    if (game.winner().isDefined()) {
      Side winner = game.winner().get();
      allMessages.add("GAME: Team " + winner.toColorName() + " won the game!");
      scheduleEndOfTurn.accept(ScheduleReason.pause(true));
    } else {
      // Report new techs
      scala.collection.immutable.Vector<Tuple2<Side, Tech>> newTechs = game.newTechsThisTurn();
      scala.collection.Iterator<Tuple2<Side, Tech>> ntIter = newTechs.iterator();
      while (ntIter.hasNext()) {
        Tuple2<Side, Tech> pair = ntIter.next();
        Side tSide = pair._1();
        Tech tech = pair._2();
        allMessages.add("GAME: Team " + tSide.toColorName() + " acquired new tech: " + tech.displayName(externalInfo.pieceMap()));
      }
      allMessages.add("GAME: Beginning " + newSide.toColorName() + " team turn (turn #" + game.turnNumber() + ")");
      scheduleEndOfTurn.accept(ScheduleReason.NEW_TURN);
    }
    broadcastMessages();
  }

  private void changeUserBoardViewed(int sessionId, int boardIdx) {
    if (userBoardViewed.get(sessionId) != boardIdx) {
      userBoardViewed.put(sessionId, boardIdx);
      broadcastPlayers();
    }
  }

  // ---------- Handle incoming queries ----------
  @SuppressWarnings("unchecked")
  public void handleQuery(
      Protocol.Query query,
      Consumer<Protocol.Response> out,
      int sessionId,
      Consumer<ScheduleReason> scheduleEndOfTurn
  ) {
    Option<Side> side = userSides.get(sessionId);

    if (query instanceof Protocol.Heartbeat) {
      Protocol.Heartbeat hb = (Protocol.Heartbeat) query;
      out.accept(new Protocol.OkHeartbeat(hb.i()));

    } else if (query instanceof Protocol.RequestPause) {
      Protocol.RequestPause rp = (Protocol.RequestPause) query;
      scheduleEndOfTurn.accept(ScheduleReason.pause(rp.isPaused()));

    } else if (query instanceof Protocol.ReportViewedBoard) {
      Protocol.ReportViewedBoard rvb = (Protocol.ReportViewedBoard) query;
      changeUserBoardViewed(sessionId, rvb.boardIdx());

    } else if (query instanceof Protocol.RequestBoardHistory) {
      Protocol.RequestBoardHistory rbh = (Protocol.RequestBoardHistory) query;
      int boardIdx = rbh.boardIdx();
      if (boardIdx < 0 || boardIdx >= numBoards) {
        out.accept(new Protocol.QueryError("Invalid boardIdx"));
      } else {
        out.accept(new Protocol.ReportBoardHistory(
            boardIdx,
            boards[boardIdx].toSummary(),
            boardSequences[boardIdx]
        ));
      }

    } else if (query instanceof Protocol.DoBoardAction) {
      Protocol.DoBoardAction dba = (Protocol.DoBoardAction) query;
      int boardIdx = dba.boardIdx();
      BoardAction boardAction = dba.boardAction();

      Log.log("Received board " + boardIdx + " action " + boardAction);

      if (side.isEmpty()) {
        out.accept(new Protocol.QueryError("Cannot perform actions as a spectator"));
      } else {
        Side playerSide = side.get();
        if (boardIdx < 0 || boardIdx >= numBoards) {
          out.accept(new Protocol.QueryError("Invalid boardIdx"));
        } else if (game.winner().isDefined()) {
          out.accept(new Protocol.QueryError("Game is over"));
        } else if (!boards[boardIdx].curState().side().equals(playerSide)) {
          out.accept(new Protocol.QueryError("Currently the other team's turn"));
        } else {
          changeUserBoardViewed(sessionId, boardIdx);
          handleDoBoardAction(boardIdx, boardAction, playerSide, out, scheduleEndOfTurn);
        }
      }

    } else if (query instanceof Protocol.DoGameAction) {
      Protocol.DoGameAction dga = (Protocol.DoGameAction) query;
      GameAction gameAction = dga.gameAction();

      Log.log("Received game action " + gameAction);

      if (side.isEmpty()) {
        out.accept(new Protocol.QueryError("Cannot perform actions as a spectator"));
      } else {
        Side playerSide = side.get();
        if (game.winner().isDefined()) {
          out.accept(new Protocol.QueryError("Game is over"));
        } else if (!game.curSide().equals(playerSide)) {
          out.accept(new Protocol.QueryError("Currently the other team's turn"));
        } else {
          handleDoGameAction(gameAction, playerSide, out, scheduleEndOfTurn);
        }
      }

    } else if (query instanceof Protocol.Chat) {
      Protocol.Chat chat = (Protocol.Chat) query;
      String username = chat.username();
      Option<Side> chatSide = chat.side();
      boolean allChat = chat.allChat();
      String message = chat.message();

      if (allChat) {
        allMessages.add(username + ": " + message);
      } else {
        if (chatSide.isEmpty()) {
          spectatorMessages.add(username + ": " + message);
        } else {
          Side s = chatSide.get();
          scala.collection.immutable.List<String> current =
              (scala.collection.immutable.List<String>) teamMessages.apply(s);
          teamMessages.update(s, (Object) current.$colon$colon(username + ": " + message));
        }
      }
      broadcastMessages();
    }
  }

  @SuppressWarnings("unchecked")
  private void handleDoBoardAction(
      int boardIdx,
      BoardAction boardAction,
      Side playerSide,
      Consumer<Protocol.Response> out,
      Consumer<ScheduleReason> scheduleEndOfTurn
  ) {
    // Some board actions are special and need extra checks
    boolean specialSuccess = true;
    String specialError = null;

    if (boardAction instanceof PlayerActions
        || boardAction instanceof LocalPieceUndo
        || boardAction instanceof SpellUndo
        || boardAction instanceof Redo) {
      // No special handling needed
    } else if (boardAction instanceof BuyReinforcementUndo) {
      BuyReinforcementUndo bru = (BuyReinforcementUndo) boardAction;
      String pieceName = bru.pieceName();
      // Check legality ahead of time
      @SuppressWarnings("unchecked")
      Try<Object> legalResult = (Try<Object>) (Object) boards[boardIdx].tryLegality(boardAction, externalInfo);
      if (legalResult.isFailure()) {
        specialSuccess = false;
        specialError = legalResult.failed().get().getLocalizedMessage();
      } else {
        Option<GeneralBoardAction> undoActionOpt = boards[boardIdx].findBuyReinforcementUndoAction(pieceName);
        if (undoActionOpt.isEmpty()) {
          specialSuccess = false;
          specialError = "BUG? Could not find buy reinforcement action that would be undone";
        } else {
          GeneralBoardAction undoAction = undoActionOpt.get();
          if (undoAction instanceof BuyReinforcement) {
            BuyReinforcement br = (BuyReinforcement) undoAction;
            if (!br.free()) {
              GameAction gameAction = new UnpayForReinforcement(playerSide, pieceName);
              if (!performAndBroadcastGameActionIfLegal(gameAction)) {
                specialSuccess = false;
                specialError = "Failed to unpay for reinforcement";
              }
            }
          } else {
            specialSuccess = false;
            specialError = "BUG? Buy reinforcement action that would be undone is wrong type";
          }
        }
      }
    } else if (boardAction instanceof GainSpellUndo) {
      GainSpellUndo gsu = (GainSpellUndo) boardAction;
      int spellId = gsu.spellId();
      @SuppressWarnings("unchecked")
      Try<Object> legalResult = (Try<Object>) (Object) boards[boardIdx].tryLegality(boardAction, externalInfo);
      if (legalResult.isFailure()) {
        specialSuccess = false;
        specialError = legalResult.failed().get().getLocalizedMessage();
      } else {
        GameAction gameAction = new UnchooseSpell(playerSide, spellId, boardIdx);
        if (!performAndBroadcastGameActionIfLegal(gameAction)) {
          specialSuccess = false;
          specialError = "Failed to unchoose spell";
        }
      }
    } else if (boardAction instanceof DoGeneralBoardAction) {
      DoGeneralBoardAction dgba = (DoGeneralBoardAction) boardAction;
      GeneralBoardAction generalBoardAction = dgba.action();

      if (generalBoardAction instanceof BuyReinforcement) {
        BuyReinforcement br = (BuyReinforcement) generalBoardAction;
        @SuppressWarnings("unchecked")
      Try<Object> legalResult = (Try<Object>) (Object) boards[boardIdx].tryLegality(boardAction, externalInfo);
        if (legalResult.isFailure()) {
          specialSuccess = false;
          specialError = legalResult.failed().get().getLocalizedMessage();
        } else if (!br.free()) {
          GameAction gameAction = new PayForReinforcement(playerSide, br.pieceName());
          if (!performAndBroadcastGameActionIfLegal(gameAction)) {
            specialSuccess = false;
            specialError = "Failed to pay for reinforcement";
          }
        }
      } else if (generalBoardAction instanceof GainSpell) {
        GainSpell gs = (GainSpell) generalBoardAction;
        @SuppressWarnings("unchecked")
      Try<Object> legalResult = (Try<Object>) (Object) boards[boardIdx].tryLegality(boardAction, externalInfo);
        if (legalResult.isFailure()) {
          specialSuccess = false;
          specialError = legalResult.failed().get().getLocalizedMessage();
        } else {
          GameAction gameAction = new ChooseSpell(playerSide, gs.spellId(), boardIdx);
          if (!performAndBroadcastGameActionIfLegal(gameAction)) {
            specialSuccess = false;
            specialError = "Failed to choose spell";
          }
        }
      }
    }

    if (!specialSuccess) {
      out.accept(new Protocol.QueryError(specialError != null ? specialError : "Unknown error"));
      return;
    }

    // Now perform the actual board action
    @SuppressWarnings("unchecked")
    Try<Object> result = (Try<Object>) (Object) boards[boardIdx].doAction(boardAction, externalInfo);
    if (result.isFailure()) {
      out.accept(new Protocol.QueryError(result.failed().get().getLocalizedMessage()));
    } else {
      // When someone plays or discards a spell, reveal it to the other side
      if (boardAction instanceof PlayerActions) {
        PlayerActions pa = (PlayerActions) boardAction;
        scala.collection.Iterator<PlayerAction> actIter = pa.actions().iterator();
        while (actIter.hasNext()) {
          PlayerAction action = actIter.next();
          if (action instanceof PlaySpell) {
            PlaySpell ps = (PlaySpell) action;
            revealSpellsToSide(game.curSide().opp(), new int[]{ps.spellId()}, true);
          } else if (action instanceof DiscardSpell) {
            DiscardSpell ds = (DiscardSpell) action;
            revealSpellsToSide(game.curSide().opp(), new int[]{ds.spellId()}, true);
          }
          // Movements, Attack, Spawn, ActivateTile, ActivateAbility, Blink, Teleport -> no reveal
        }
      }

      // If this board was set as done, unset it
      maybeUnsetBoardDone(boardIdx);

      boardSequences[boardIdx] += 1;
      out.accept(new Protocol.OkBoardAction(boardIdx, boardSequences[boardIdx]));
      broadcastAll(new Protocol.ReportBoardAction(boardIdx, boardAction, boardSequences[boardIdx]));
    }
  }

  private void handleDoGameAction(
      GameAction gameAction,
      Side playerSide,
      Consumer<Protocol.Response> out,
      Consumer<ScheduleReason> scheduleEndOfTurn
  ) {
    // Some game actions are special
    boolean specialSuccess = true;
    String specialError = null;

    if (gameAction instanceof PerformTech
        || gameAction instanceof UndoTech
        || gameAction instanceof SetBoardDone
        || gameAction instanceof SellTech
        || gameAction instanceof UnsellTech) {
      // No special handling needed
    } else if (gameAction instanceof BuyExtraTechAndSpell) {
      refillUpcomingSpells();
      // Success
    } else if (gameAction instanceof UnbuyExtraTechAndSpell) {
      // No special handling needed
    } else if (gameAction instanceof ResignBoard) {
      ResignBoard rb = (ResignBoard) gameAction;
      @SuppressWarnings("unchecked")
        Try<Object> legalResult = (Try<Object>) (Object) game.tryIsLegal(gameAction);
      if (legalResult.isFailure()) {
        specialSuccess = false;
        specialError = legalResult.failed().get().getLocalizedMessage();
      } else {
        doResetBoard(rb.boardIdx(), true, false);
        allMessages.add("GAME: Team " + game.curSide().toColorName() + " resigned board " + (rb.boardIdx() + 1) + "!");
        broadcastMessages();
      }
    } else if (gameAction instanceof PayForReinforcement
        || gameAction instanceof UnpayForReinforcement
        || gameAction instanceof AddWin
        || gameAction instanceof AddUpcomingSpells
        || gameAction instanceof ChooseSpell
        || gameAction instanceof UnchooseSpell) {
      specialSuccess = false;
      specialError = "Only server allowed to send this action";
    }

    if (!specialSuccess) {
      out.accept(new Protocol.QueryError(specialError != null ? specialError : "Unknown error"));
      return;
    }

    @SuppressWarnings("unchecked")
    Try<Object> result2 = (Try<Object>) (Object) game.doAction(gameAction);
    if (result2.isFailure()) {
      out.accept(new Protocol.QueryError(result2.failed().get().getLocalizedMessage()));
    } else {
      gameSequence += 1;
      out.accept(new Protocol.OkGameAction(gameSequence));
      broadcastAll(new Protocol.ReportGameAction(gameAction, gameSequence));

      if (game.winner().isDefined()) {
        Side winner = game.winner().get();
        allMessages.add("GAME: Team " + winner.toColorName() + " won the game!");
        broadcastMessages();
      }
      maybeDoEndOfTurn(scheduleEndOfTurn);
    }
  }

  // ---------- User session management ----------

  @SuppressWarnings("unchecked")
  public void handleUserJoined(
      int sessionId,
      String username,
      Option<Side> side,
      Consumer<Protocol.Response> out
  ) {
    usernameOfSession.put(sessionId, username);
    userSides.put(sessionId, side);
    userOuts.put(sessionId, out);
    userBoardViewed.put(sessionId, 0);

    out.accept(new Protocol.Version(CurrentVersion$.MODULE$.version()));
    out.accept(new Protocol.ClientHeartbeatRate(clientHeartbeatPeriodInSeconds));

    // Determine which spells to reveal based on side
    scala.collection.immutable.Set<Object> spellIds;
    if (side.isEmpty()) {
      scala.collection.immutable.Set<Object> s0Revealed =
          (scala.collection.immutable.Set<Object>) revealedSpellIds.apply(S0());
      scala.collection.immutable.Set<Object> s1Revealed =
          (scala.collection.immutable.Set<Object>) revealedSpellIds.apply(S1());
      spellIds = (scala.collection.immutable.Set<Object>) s0Revealed.intersect(s1Revealed);
    } else {
      spellIds = (scala.collection.immutable.Set<Object>) revealedSpellIds.apply(side.get());
    }

    // Convert spellIds to array of (SpellId, SpellName)
    List<Tuple2<Object, String>> spellIdsAndNamesList = new ArrayList<>();
    scala.collection.Iterator<Object> idIter = spellIds.iterator();
    while (idIter.hasNext()) {
      Object spellIdObj = idIter.next();
      int spellId = (Integer) spellIdObj;
      Option<String> nameOpt = spellMap.get(spellId);
      if (nameOpt.isDefined()) {
        spellIdsAndNamesList.add(new Tuple2<>(spellIdObj, nameOpt.get()));
      }
    }
    Tuple2<Object, String>[] spellIdsAndNames = spellIdsAndNamesList.toArray(new Tuple2[0]);

    // Build board summaries
    BoardSummary[] summaries = new BoardSummary[boards.length];
    for (int i = 0; i < boards.length; i++) {
      summaries[i] = boards[i].toSummary();
    }

    out.accept(new Protocol.Initialize(
        game,
        summaries,
        boardNames,
        boardSequences.clone(),
        spellIdsAndNames
    ));

    out.accept(new Protocol.ReportPause(isPaused));
    Log.log("UserJoined: " + username + " Side: " + side);
    broadcastAll(new Protocol.UserJoined(username, side));
    broadcastPlayers();
    broadcastMessages();
  }

  public void handleUserLeft(int sessionId) {
    if (usernameOfSession.containsKey(sessionId)) {
      String username = usernameOfSession.get(sessionId);
      Option<Side> side = userSides.get(sessionId);
      broadcastAll(new Protocol.UserLeft(username, side));
      usernameOfSession.remove(sessionId);
      userSides.remove(sessionId);
      userOuts.remove(sessionId);
      userBoardViewed.remove(sessionId);
      Log.log("UserLeft: " + username + " Side: " + side);
      broadcastAll(new Protocol.UserLeft(username, side));
      broadcastPlayers();
      broadcastMessages();
    }
  }

  // =========================================================================
  // Static factory methods
  // =========================================================================

  @SuppressWarnings("unchecked")
  public static GameState createNormal(
      SideArray<Object> secondsPerTurn,
      SideArray<Object> startingSoulsPerBoard,
      SideArray<Object> extraSoulsPerTurn,
      int targetWins,
      int techSouls,
      Option<scala.collection.immutable.List<String>> mapsOpt,
      Option<Object> seedOpt,
      Option<String> password,
      boolean testingSetup
  ) {
    Config config = ConfigFactory.parseFile(new java.io.File(AppPaths.applicationConf));

    long randSeed;
    if (seedOpt.isDefined()) {
      randSeed = (Long) seedOpt.get();
    } else {
      randSeed = new SecureRandom().nextLong();
    }

    Rand setupRand = Rand.create(randSeed);

    // techsAlwaysAcquired
    PieceStats[] alwaysAcquired = Units$.MODULE$.alwaysAcquiredPieces();
    Tech[] techsAlwaysAcquired = new Tech[alwaysAcquired.length];
    for (int i = 0; i < alwaysAcquired.length; i++) {
      techsAlwaysAcquired[i] = new PieceTech(alwaysAcquired[i].name());
    }

    // otherTechs
    PieceStats[] techPieces = Units$.MODULE$.techPieces();
    int totalTechs = techPieces.length + 3; // +3 for Copycat, TechSeller, Metamagic
    List<Tuple2<Tech, Integer>> techsWithIdx = new ArrayList<>();
    for (int i = 0; i < techPieces.length; i++) {
      techsWithIdx.add(new Tuple2<>(new PieceTech(techPieces[i].name()), i));
    }
    techsWithIdx.add(new Tuple2<>(Copycat$.MODULE$, techPieces.length));
    techsWithIdx.add(new Tuple2<>(TechSeller$.MODULE$, techPieces.length + 1));
    techsWithIdx.add(new Tuple2<>(Metamagic$.MODULE$, techPieces.length + 2));

    TechState[] otherTechs;
    if (!config.getBoolean("app.randomizeTechLine")) {
      otherTechs = new TechState[techsWithIdx.size()];
      for (int i = 0; i < techsWithIdx.size(); i++) {
        Tuple2<Tech, Integer> pair = techsWithIdx.get(i);
        Tech tech = pair._1();
        ClassTag<TechLevel> techLevelTag = ClassTag$.MODULE$.apply(TechLevel.class);
        otherTechs[i] = new TechState(
            tech.shortDisplayName(Units$.MODULE$.pieceMap()),
            tech.displayName(Units$.MODULE$.pieceMap()),
            tech,
            new Some<Object>(i + 1),
            SideArray$.MODULE$.create(TechLocked$.MODULE$, techLevelTag),
            SideArray$.MODULE$.create(TechLocked$.MODULE$, techLevelTag)
        );
      }
    } else {
      int numFixedTechs = config.getInt("app.numFixedTechs");
      List<Tuple2<Tech, Integer>> fixedTechs = new ArrayList<>(techsWithIdx.subList(0, numFixedTechs));
      List<Tuple2<Tech, Integer>> remaining = new ArrayList<>(techsWithIdx.subList(numFixedTechs, techsWithIdx.size()));

      // Shuffle remaining
      List<Tuple2<Tech, Integer>> randomized = setupRand.shuffle(remaining);

      // Partition into two sets
      int halfSize = (randomized.size() + 1) / 2;
      List<Tuple2<Tech, Integer>> set1 = new ArrayList<>(randomized.subList(0, halfSize));
      List<Tuple2<Tech, Integer>> set2 = new ArrayList<>(randomized.subList(halfSize, randomized.size()));

      // Sort each set by original index
      set1.sort((a, b) -> Integer.compare(a._2(), b._2()));
      set2.sort((a, b) -> Integer.compare(a._2(), b._2()));

      // Interleave
      List<Tuple2<Tech, Integer>> interleaved = new ArrayList<>();
      int maxLen = Math.max(set1.size(), set2.size());
      for (int i = 0; i < maxLen; i++) {
        if (i < set1.size()) interleaved.add(set1.get(i));
        if (i < set2.size()) interleaved.add(set2.get(i));
      }

      // Combine fixed + interleaved, update indices
      List<Tuple2<Tech, Integer>> ordered = new ArrayList<>();
      for (Tuple2<Tech, Integer> ft : fixedTechs) {
        ordered.add(new Tuple2<>(ft._1(), ft._2() + 1));
      }
      for (Tuple2<Tech, Integer> it : interleaved) {
        ordered.add(new Tuple2<>(it._1(), it._2() + 1));
      }

      ClassTag<TechLevel> techLevelTag = ClassTag$.MODULE$.apply(TechLevel.class);
      otherTechs = new TechState[ordered.size()];
      for (int i = 0; i < ordered.size(); i++) {
        Tuple2<Tech, Integer> pair = ordered.get(i);
        Tech tech = pair._1();
        int techNumber = pair._2();
        otherTechs[i] = new TechState(
            tech.shortDisplayName(Units$.MODULE$.pieceMap()),
            tech.displayName(Units$.MODULE$.pieceMap()),
            tech,
            new Some<Object>(techNumber),
            SideArray$.MODULE$.create(TechLocked$.MODULE$, techLevelTag),
            SideArray$.MODULE$.create(TechLocked$.MODULE$, techLevelTag)
        );
      }
    }

    return create(secondsPerTurn, startingSoulsPerBoard, extraSoulsPerTurn,
        targetWins, techSouls, mapsOpt, seedOpt, password, testingSetup,
        techsAlwaysAcquired, otherTechs, Units$.MODULE$.pieceMap());
  }

  @SuppressWarnings("unchecked")
  public static GameState create(
      SideArray<Object> secondsPerTurn,
      SideArray<Object> startingSoulsPerBoard,
      SideArray<Object> extraSoulsPerTurn,
      int targetWins,
      int techSouls,
      Option<scala.collection.immutable.List<String>> mapsOpt,
      Option<Object> seedOpt,
      Option<String> password,
      boolean testingSetup,
      Tech[] techsAlwaysAcquired,
      TechState[] otherTechs,
      scala.collection.immutable.Map<String, PieceStats> pieceMap
  ) {
    Config config = ConfigFactory.parseFile(new java.io.File(AppPaths.applicationConf));

    long randSeed;
    if (seedOpt.isDefined()) {
      randSeed = (Long) seedOpt.get();
    } else {
      randSeed = new SecureRandom().nextLong();
    }

    Rand setupRand = Rand.create(randSeed);

    // Choose maps
    scala.collection.immutable.List<Tuple2<String, scala.Function0<BoardState>>> chosenMaps;

    if (mapsOpt.isEmpty()) {
      scala.collection.immutable.Map<String, scala.Function0<BoardState>> availableMapsScala;
      if (config.getBoolean("app.includeAdvancedMaps")) {
        availableMapsScala = (scala.collection.immutable.Map<String, scala.Function0<BoardState>>)
            BoardMaps$.MODULE$.basicMaps().$plus$plus(BoardMaps$.MODULE$.advancedMaps());
      } else {
        availableMapsScala = BoardMaps$.MODULE$.basicMaps();
      }

      // Convert to Java list, shuffle, take targetWins
      Map<String, scala.Function0<BoardState>> javaAvailable =
          JavaConverters.mapAsJavaMapConverter(availableMapsScala).asJava();

      if (targetWins > javaAvailable.size()) {
        throw new RuntimeException("Configured for " + targetWins + " boards but only " + javaAvailable.size() + " available");
      }

      List<Tuple2<String, scala.Function0<BoardState>>> mapList = new ArrayList<>();
      for (Map.Entry<String, scala.Function0<BoardState>> entry : javaAvailable.entrySet()) {
        mapList.add(new Tuple2<>(entry.getKey(), entry.getValue()));
      }
      List<Tuple2<String, scala.Function0<BoardState>>> shuffled = setupRand.shuffle(mapList);
      List<Tuple2<String, scala.Function0<BoardState>>> chosen = shuffled.subList(0, targetWins);

      // Convert back to scala list
      scala.collection.immutable.List<Tuple2<String, scala.Function0<BoardState>>> result = scalaListNil();
      for (int i = chosen.size() - 1; i >= 0; i--) {
        result = result.$colon$colon(chosen.get(i));
      }
      chosenMaps = result;
    } else {
      scala.collection.immutable.List<String> mapNames = mapsOpt.get();
      scala.collection.immutable.Map<String, scala.Function0<BoardState>> allMaps =
          (scala.collection.immutable.Map<String, scala.Function0<BoardState>>)
          BoardMaps$.MODULE$.basicMaps().$plus$plus(BoardMaps$.MODULE$.advancedMaps());

      List<Tuple2<String, scala.Function0<BoardState>>> mapList = new ArrayList<>();
      scala.collection.Iterator<String> nameIter = mapNames.iterator();
      while (nameIter.hasNext()) {
        String mapName = nameIter.next();
        Option<scala.Function0<BoardState>> mapFnOpt =
            (Option<scala.Function0<BoardState>>) (Object) allMaps.get(mapName);
        if (mapFnOpt.isDefined()) {
          mapList.add(new Tuple2<>(mapName, mapFnOpt.get()));
        }
      }

      scala.collection.immutable.List<Tuple2<String, scala.Function0<BoardState>>> result = scalaListNil();
      for (int i = mapList.size() - 1; i >= 0; i--) {
        result = result.$colon$colon(mapList.get(i));
      }
      chosenMaps = result;
    }

    // Count maps
    int numBoards = chosenMaps.length();

    ExternalInfo externalInfo = ExternalInfo$.MODULE$.create(pieceMap);

    // Create Game
    int targetNumWins = targetWins;
    ClassTag<Object> intTag = (ClassTag<Object>) (Object) ClassTag$.MODULE$.Int();
    SideArray<Object> startingSouls = startingSoulsPerBoard.map(
        new scala.runtime.AbstractFunction1<Object, Object>() {
          public Object apply(Object x) {
            return ((Integer) x) * numBoards;
          }
        },
        intTag
    );

    SideArray<Object> finalStartingSouls;
    if (testingSetup) {
      int s0Souls = (Integer) startingSouls.apply(S0()) + 1000;
      int s1Souls = (Integer) startingSouls.apply(S1());
      finalStartingSouls = SideArray$.MODULE$.createTwo(s0Souls, s1Souls, intTag);
    } else {
      finalStartingSouls = startingSouls;
    }

    int extraTechCost = techSouls * numBoards;

    Game game = Game$.MODULE$.apply(
        numBoards,
        targetNumWins,
        S0(),
        (SideArray<Object>) (Object) finalStartingSouls,
        extraTechCost,
        (SideArray<Object>) (Object) extraSoulsPerTurn,
        techsAlwaysAcquired,
        otherTechs,
        pieceMap
    );

    // Create boards
    List<Tuple2<String, scala.Function0<BoardState>>> chosenMapsList = scalaListToJava(chosenMaps);
    Board[] boards = new Board[numBoards];
    String[] boardNames = new String[numBoards];

    ClassTag<scala.collection.immutable.List> listTag =
        (ClassTag<scala.collection.immutable.List>) (Object) ClassTag$.MODULE$.apply(scala.collection.immutable.List.class);
    ClassTag<scala.collection.immutable.Map> mapTag =
        (ClassTag<scala.collection.immutable.Map>) (Object) ClassTag$.MODULE$.apply(scala.collection.immutable.Map.class);

    for (int i = 0; i < chosenMapsList.size(); i++) {
      Tuple2<String, scala.Function0<BoardState>> mapEntry = chosenMapsList.get(i);
      String boardName = mapEntry._1();
      BoardState state = mapEntry._2().apply();

      scala.collection.immutable.List<String> necroNameList =
          scalaListNil();
      necroNameList = necroNameList.$colon$colon(Units$.MODULE$.necromancer().name());
      SideArray<scala.collection.immutable.List<String>> necroNames =
          (SideArray<scala.collection.immutable.List<String>>) (Object)
          SideArray$.MODULE$.create(necroNameList, listTag);

      SideArray<scala.collection.immutable.Map<String, Object>> reinforcements =
          (SideArray<scala.collection.immutable.Map<String, Object>>) (Object)
          SideArray$.MODULE$.create(scala.collection.immutable.Map$.MODULE$.empty(), mapTag);

      state.resetBoard(necroNames, true, false, reinforcements, externalInfo);

      if (testingSetup) {
        state.tiles().foreachi(new scala.runtime.AbstractFunction2<Loc, Tile, scala.runtime.BoxedUnit>() {
          public scala.runtime.BoxedUnit apply(Loc loc, Tile tile) {
            if (tile.terrain().equals(Graveyard$.MODULE$)) {
              state.spawnPieceInitial(S0(), Units$.MODULE$.horror(), loc);
            }
            return scala.runtime.BoxedUnit.UNIT;
          }
        });
      }

      boards[i] = Board$.MODULE$.create(state);
      boardNames[i] = boardName;
    }

    int[] boardSequences = new int[numBoards];

    // Build initial (empty) Scala collections for the GameState fields
    ClassTag<Object> objectTag = ClassTag$.MODULE$.apply(Object.class);

    SideArray<Object> teamMessagesArr = SideArray$.MODULE$.create(
        (Object) scalaListNil(), objectTag);
    SideArray<Object> spellsRemainingArr = SideArray$.MODULE$.create(
        (Object) scalaListNil(), objectTag);

    ClassTag<Object> setTag = (ClassTag<Object>) (Object) ClassTag$.MODULE$.apply(scala.collection.immutable.Set.class);
    SideArray<Object> revealedSpellIdsArr = SideArray$.MODULE$.create(
        (Object) scala.collection.immutable.Set$.MODULE$.empty(), objectTag);

    return new GameState(
        password,
        secondsPerTurn,
        new ArrayList<>(),    // allMessages
        teamMessagesArr,
        new ArrayList<>(),    // spectatorMessages
        true,                 // isPaused
        randSeed,
        numBoards,
        game,
        0,                    // gameSequence
        spellsRemainingArr,
        0,                    // nextSpellId
        (scala.collection.immutable.Map<Object, String>) (Object) scala.collection.immutable.Map$.MODULE$.empty(),
        revealedSpellIdsArr,
        externalInfo,
        boards,
        boardNames,
        boardSequences
    );
  }

  @SuppressWarnings("unchecked")
  public static GameState createVacuum(
      SideArray<Object> secondsPerTurn,
      SideArray<Object> startingSoulsPerBoard,
      SideArray<Object> extraSoulsPerTurn,
      int targetWins,
      int techSouls,
      Option<scala.collection.immutable.List<String>> mapsOpt,
      Option<Object> seedOpt,
      Option<String> password,
      PieceStats blueUnit,
      PieceStats redUnit
  ) {
    // techsAlwaysAcquired
    PieceStats[] alwaysAcquired = Units$.MODULE$.alwaysAcquiredPieces();
    Tech[] techsAlwaysAcquired = new Tech[alwaysAcquired.length];
    for (int i = 0; i < alwaysAcquired.length; i++) {
      techsAlwaysAcquired[i] = new PieceTech(alwaysAcquired[i].name());
    }

    ClassTag<TechLevel> techLevelTag = ClassTag$.MODULE$.apply(TechLevel.class);

    TechState[] otherTechs = new TechState[] {
        new TechState(
            blueUnit.shortDisplayName(),
            blueUnit.displayName(),
            new PieceTech(blueUnit.name()),
            new Some<Object>(1),
            SideArray$.MODULE$.createTwo(TechAcquired$.MODULE$, TechLocked$.MODULE$, techLevelTag),
            SideArray$.MODULE$.createTwo(TechAcquired$.MODULE$, TechLocked$.MODULE$, techLevelTag)
        ),
        new TechState(
            redUnit.shortDisplayName(),
            redUnit.displayName(),
            new PieceTech(redUnit.name()),
            new Some<Object>(2),
            SideArray$.MODULE$.createTwo(TechLocked$.MODULE$, TechAcquired$.MODULE$, techLevelTag),
            SideArray$.MODULE$.createTwo(TechLocked$.MODULE$, TechAcquired$.MODULE$, techLevelTag)
        )
    };

    // Build pieceMap
    PieceStats[] specialNecros = Units$.MODULE$.specialNecromancers();
    List<PieceStats> pieces = new ArrayList<>();
    pieces.add(Units$.MODULE$.necromancer());
    for (PieceStats ps : specialNecros) { pieces.add(ps); }
    for (PieceStats ps : alwaysAcquired) { pieces.add(ps); }
    pieces.add(blueUnit);
    pieces.add(redUnit);

    @SuppressWarnings("unchecked")
    scala.collection.immutable.Map<String, PieceStats> pieceMap =
        (scala.collection.immutable.Map<String, PieceStats>) (Object) scala.collection.immutable.Map$.MODULE$.empty();
    for (PieceStats ps : pieces) {
      pieceMap = (scala.collection.immutable.Map<String, PieceStats>) (Object) pieceMap.$plus(new Tuple2<>(ps.name(), ps));
    }

    return create(secondsPerTurn, startingSoulsPerBoard, extraSoulsPerTurn,
        targetWins, techSouls, mapsOpt, seedOpt, password, false,
        techsAlwaysAcquired, otherTechs, pieceMap);
  }
}

// ---------------------------------------------------------------------------
// AppPaths - must be accessible from GameState
// ---------------------------------------------------------------------------
class AppPaths {
  static final String applicationConf = "./application.conf";
  static final String mainPage = "./web/index.html";
  static final String favicon = "./web/img/favicon.jpg";
}

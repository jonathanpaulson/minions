package minionsgame.server;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;

import minionsgame.core.*;
import scala.Option;
import scala.Some;
import scala.Tuple2;
import scala.collection.JavaConverters;
import scala.collection.immutable.Vector;
import scala.util.Try;

public class AI {
  private final Consumer<Protocol.Query> out;
  private final GameState game;
  private final boolean doTutorial;

  private String name = "Igor";
  private Option<Side> side = new Some<>(S1$.MODULE$);
  private int tutorialStep = 0;
  private final Rand aiRand;
  private int nextActionIdSuffix = 0;

  public AI(Consumer<Protocol.Query> out, GameState game, boolean doTutorial) {
    this.out = out;
    this.game = game;
    this.doTutorial = doTutorial;
    this.aiRand = Rand.create(RandUtils.sha256Long(game.randSeed() + "#ai"));
  }

  private String makeActionId() {
    nextActionIdSuffix = nextActionIdSuffix + 1;
    return name + nextActionIdSuffix;
  }

  private void chat(String message) {
    out.accept(new Protocol.Chat(name, side, true, message));
  }

  private Side S0() { return S0$.MODULE$; }
  private Side S1() { return S1$.MODULE$; }

  // Compute pathfinding distance between two locations for a given piece
  private int distance(PieceStats baseStats, PieceStats curStats, Loc l1, Loc l2) {
    final int[] ans = {1000};
    BoardState board = game.boards()[0].curState();
    board.tiles().topology().forEachReachable(l1, new scala.runtime.AbstractFunction2<Loc, Object, Object>() {
      public Object apply(Loc loc, Object distObj) {
        int dist = (Integer) distObj;
        if (board.inBounds(loc) && board.canWalkOnTile(baseStats, curStats, board.tiles().apply(loc))) {
          if (loc.equals(l2)) {
            ans[0] = dist;
          }
          return true;
        } else {
          return false;
        }
      }
    });
    return ans[0];
  }

  // Find target locations: hexes within attack range of enemies or unoccupied graveyards
  private scala.collection.immutable.List<Loc> targetLocs(int attackRange) {
    BoardState board = game.boards()[0].curState();
    return board.pieces().filterLocs(new scala.runtime.AbstractFunction1<Loc, Object>() {
      public Object apply(Loc loc) {
        // Check if within range of enemy
        boolean withinRangeOfEnemy = false;
        Map<Object, Piece> javaMap = JavaConverters.mapAsJavaMapConverter(board.pieceById()).asJava();
        for (Piece enemyPiece : javaMap.values()) {
          if (enemyPiece.side().equals(S0()) && board.topology().distance(enemyPiece.loc(), loc) <= attackRange) {
            withinRangeOfEnemy = true;
            break;
          }
        }

        // Check if graveyard not already occupied by us
        boolean alreadyOccupiedByUs = false;
        for (Piece piece : javaMap.values()) {
          if (piece.side().equals(S1()) && piece.loc().equals(loc)) {
            alreadyOccupiedByUs = true;
            break;
          }
        }

        boolean graveyard = board.tiles().apply(loc).terrain().equals(Graveyard$.MODULE$);
        return withinRangeOfEnemy || (graveyard && !alreadyOccupiedByUs);
      }
    });
  }

  // Find the best move for a piece from a set of options
  private Option<Loc> bestMove(PieceStats baseStats, PieceStats curStats, scala.collection.immutable.Map<Loc, Object> options) {
    try {
      scala.collection.immutable.List<Loc> targets = targetLocs(curStats.attackRange());
      if (targets.isEmpty()) return Option.empty();

      Map<Loc, Integer> javaOptions = new HashMap<>();
      @SuppressWarnings("unchecked")
      java.util.Map<Loc, Object> rawMap = JavaConverters.mapAsJavaMapConverter(
          (scala.collection.Map<Loc, Object>) options).asJava();
      for (java.util.Map.Entry<Loc, Object> entry : rawMap.entrySet()) {
        javaOptions.put(entry.getKey(), (Integer) entry.getValue());
      }

      if (javaOptions.isEmpty()) return Option.empty();

      Loc bestLoc = null;
      int bestDistFromTargets = Integer.MAX_VALUE;
      int bestDist = Integer.MAX_VALUE;

      for (Map.Entry<Loc, Integer> entry : javaOptions.entrySet()) {
        Loc loc = entry.getKey();
        int dist = entry.getValue();

        // Find minimum distance from targets
        int minDistFromTargets = Integer.MAX_VALUE;
        scala.collection.Iterator<Loc> iter = targets.iterator();
        while (iter.hasNext()) {
          Loc target = iter.next();
          int d = distance(baseStats, curStats, target, loc);
          if (d < minDistFromTargets) {
            minDistFromTargets = d;
          }
        }

        if (minDistFromTargets < bestDistFromTargets ||
            (minDistFromTargets == bestDistFromTargets && dist < bestDist)) {
          bestDistFromTargets = minDistFromTargets;
          bestDist = dist;
          bestLoc = loc;
        }
      }

      if (bestLoc != null) {
        return new Some<>(bestLoc);
      }
      return Option.empty();
    } catch (Exception e) {
      return Option.empty();
    }
  }

  // Overload for Java Map
  private Option<Loc> bestMove(PieceStats baseStats, PieceStats curStats, Map<Loc, Integer> javaOptions) {
    try {
      scala.collection.immutable.List<Loc> targets = targetLocs(curStats.attackRange());
      if (targets.isEmpty()) return Option.empty();
      if (javaOptions.isEmpty()) return Option.empty();

      Loc bestLoc = null;
      int bestDistFromTargets = Integer.MAX_VALUE;
      int bestDist = Integer.MAX_VALUE;

      for (Map.Entry<Loc, Integer> entry : javaOptions.entrySet()) {
        Loc loc = entry.getKey();
        int dist = entry.getValue();

        int minDistFromTargets = Integer.MAX_VALUE;
        scala.collection.Iterator<Loc> iter = targets.iterator();
        while (iter.hasNext()) {
          Loc target = iter.next();
          int d = distance(baseStats, curStats, target, loc);
          if (d < minDistFromTargets) {
            minDistFromTargets = d;
          }
        }

        if (minDistFromTargets < bestDistFromTargets ||
            (minDistFromTargets == bestDistFromTargets && dist < bestDist)) {
          bestDistFromTargets = minDistFromTargets;
          bestDist = dist;
          bestLoc = loc;
        }
      }

      if (bestLoc != null) {
        return new Some<>(bestLoc);
      }
      return Option.empty();
    } catch (Exception e) {
      return Option.empty();
    }
  }

  // Compute distance from targets for sorting pieces
  private int distanceFromTargets(Piece piece) {
    BoardState board = game.boards()[0].curState();
    PieceStats cs = piece.curStats(board);
    scala.collection.immutable.List<Loc> targets = targetLocs(cs.attackRange());
    if (targets.isEmpty()) return 1000;
    int minDist = Integer.MAX_VALUE;
    scala.collection.Iterator<Loc> iter = targets.iterator();
    while (iter.hasNext()) {
      Loc target = iter.next();
      int d = distance(piece.baseStats(), cs, target, piece.loc());
      if (d < minDist) {
        minDist = d;
      }
    }
    return minDist;
  }

  @SuppressWarnings("unchecked")
  public void handleMessage(Protocol.Response message) {
    if (message instanceof Protocol.ReportTimeLeft) {
      // Ignore
      return;
    }

    if (message instanceof Protocol.ReportNewTurn) {
      Protocol.ReportNewTurn rnt = (Protocol.ReportNewTurn) message;
      if (rnt.newSide().equals(S1())) {
        handleNewTurn();
        return;
      }
    }

    // Default case: handle tutorial
    if (doTutorial) {
      handleTutorial();
    }
  }

  @SuppressWarnings("unchecked")
  private void handleNewTurn() {
    if (game.game().winner().isDefined()) {
      return;
    }

    // Buy extra tech and spell if souls > 10
    if (((SideArray<Object>) (Object) game.game().souls()).apply(S1()) instanceof Integer) {
      int souls = (Integer) ((SideArray<Object>) (Object) game.game().souls()).apply(S1());
      if (souls > 10) {
        out.accept(new Protocol.DoGameAction(new BuyExtraTechAndSpell(S1())));
      }
    }

    TechState[] techs = game.game().techLine();

    // Find unlocked techs
    List<Integer> unlockedTechIndices = new ArrayList<>();
    for (int i = 0; i < techs.length; i++) {
      SideArray<TechLevel> levelArr = techs[i].level();
      TechLevel level = levelArr.apply(S1());
      if (level.equals(TechAcquired$.MODULE$)) {
        unlockedTechIndices.add(i);
      }
    }

    // Find unlocked units that the AI can build
    List<PieceStats> unlockedUnits = new ArrayList<>();
    Map<String, PieceStats> pieceMap = JavaConverters.mapAsJavaMapConverter(game.externalInfo().pieceMap()).asJava();

    for (int idx : unlockedTechIndices) {
      Tech tech = techs[idx].tech();
      if (tech instanceof PieceTech) {
        PieceTech pt = (PieceTech) tech;
        String pieceName = pt.pieceName();
        PieceStats stats = pieceMap.get(pieceName);
        if (stats != null &&
            !stats.name().equals("zombie") &&
            stats.moveRange() > 0 &&
            stats.cost() <= (Integer) ((SideArray<Object>) (Object) game.game().souls()).apply(S1()) &&
            stats.attackEffect().isDefined()) {
          unlockedUnits.add(stats);
        }
      }
      // Copycat, TechSeller, Metamagic -> skip
    }

    // Buy units
    boolean keepBuying = true;
    while (keepBuying && !unlockedUnits.isEmpty()) {
      PieceStats chosenUnit = unlockedUnits.get(aiRand.nextInt(unlockedUnits.size()));
      if (game.boards()[0].curState().canFreeBuyPiece(S1(), chosenUnit.name())) {
        out.accept(new Protocol.DoBoardAction(0,
            new DoGeneralBoardAction(new BuyReinforcement(chosenUnit.name(), true), makeActionId())));
        sleep(100);
        keepBuying = true;
      } else if ((Integer) ((SideArray<Object>) (Object) game.game().souls()).apply(S1()) >= chosenUnit.cost()) {
        out.accept(new Protocol.DoBoardAction(0,
            new DoGeneralBoardAction(new BuyReinforcement(chosenUnit.name(), false), makeActionId())));
        sleep(100);
        keepBuying = true;
      } else {
        keepBuying = false;
      }
    }

    // Sort pieces by distance from targets (closest first)
    BoardState boardForSort = game.boards()[0].curState();
    Map<Object, Piece> allPiecesMap = JavaConverters.mapAsJavaMapConverter(boardForSort.pieceById()).asJava();
    List<Piece> sortedPieces = new ArrayList<>();
    for (Piece piece : allPiecesMap.values()) {
      if (piece.side().equals(S1())) {
        sortedPieces.add(piece);
      }
    }
    sortedPieces.sort((a, b) -> Integer.compare(distanceFromTargets(a), distanceFromTargets(b)));

    // Move and attack with units
    for (Piece piece : sortedPieces) {
      BoardState board = game.boards()[0].curState();
      boolean pieceShouldStay = board.tiles().apply(piece.loc()).terrain().equals(Graveyard$.MODULE$)
          || piece.curStats(board).isNecromancer();

      if (!pieceShouldStay) {
        scala.collection.immutable.Map<Loc, Object> moves = board.legalMoves(piece);
        PieceStats curStats = piece.curStats(board);
        Option<Loc> best = bestMove(piece.baseStats(), curStats, moves);

        // findPathForUI call
        final Option<Loc> bestFinal = best;
        Option<Tuple2<scala.collection.immutable.List<Loc>, scala.collection.immutable.List<Tuple2<PieceSpec, scala.collection.immutable.List<Loc>>>>> pathResult =
            board.findPathForUI(piece,
                scala.collection.immutable.List$.MODULE$.<Loc>empty(),
                false,
                new scala.runtime.AbstractFunction2<Loc, scala.collection.immutable.List<Tuple2<PieceSpec, scala.collection.immutable.List<Loc>>>, Object>() {
                  public Object apply(Loc loc, scala.collection.immutable.List<Tuple2<PieceSpec, scala.collection.immutable.List<Loc>>> movements) {
                    if (bestFinal.isDefined()) {
                      return loc.equals(bestFinal.get());
                    }
                    return false;
                  }
                });

        if (pathResult.isDefined()) {
          Tuple2<scala.collection.immutable.List<Loc>, scala.collection.immutable.List<Tuple2<PieceSpec, scala.collection.immutable.List<Loc>>>> result = pathResult.get();
          scala.collection.immutable.List<Loc> path = result._1();
          scala.collection.immutable.List<Tuple2<PieceSpec, scala.collection.immutable.List<Loc>>> pathMovements = result._2();

          // Filter path movements to those with path length > 1
          List<Tuple2<PieceSpec, scala.collection.immutable.List<Loc>>> filteredPathMovements = new ArrayList<>();
          scala.collection.Iterator<Tuple2<PieceSpec, scala.collection.immutable.List<Loc>>> pmIter = pathMovements.iterator();
          while (pmIter.hasNext()) {
            Tuple2<PieceSpec, scala.collection.immutable.List<Loc>> pm = pmIter.next();
            if (pm._2().length() > 1) {
              filteredPathMovements.add(pm);
            }
          }

          if (!filteredPathMovements.isEmpty()) {
            // Build Movements from filtered path movements
            scala.collection.immutable.List<Movement> movementsList = scala.collection.immutable.List$.MODULE$.<Movement>empty();
            for (int i = filteredPathMovements.size() - 1; i >= 0; i--) {
              Tuple2<PieceSpec, scala.collection.immutable.List<Loc>> pm = filteredPathMovements.get(i);
              Vector<Loc> pathVec = pm._2().toVector();
              movementsList = movementsList.$colon$colon(new Movement(pm._1(), pathVec));
            }
            Movements movements = new Movements(movementsList);
            scala.collection.immutable.List<PlayerAction> actionsList =
                scala.collection.immutable.List$.MODULE$.<PlayerAction>empty().$colon$colon(movements);
            out.accept(new Protocol.DoBoardAction(0, new PlayerActions(actionsList, makeActionId())));
            sleep(100);
          } else if (path.length() > 1) {
            Vector<Loc> pathVec = path.toVector();
            scala.collection.immutable.List<Movement> movementsList =
                scala.collection.immutable.List$.MODULE$.<Movement>empty().$colon$colon(new Movement(piece.spec(), pathVec));
            Movements movements = new Movements(movementsList);
            scala.collection.immutable.List<PlayerAction> actionsList =
                scala.collection.immutable.List$.MODULE$.<PlayerAction>empty().$colon$colon(movements);
            out.accept(new Protocol.DoBoardAction(0, new PlayerActions(actionsList, makeActionId())));
            sleep(100);
          }
        }
      }

      // Attack with the piece
      PieceStats pieceCurStats = piece.curStats(game.boards()[0].curState());
      for (int i = 0; i <= pieceCurStats.numAttacks(); i++) {
        BoardState b1 = game.boards()[0].curState();
        Option<Piece> p1Opt = b1.findPiece(piece.spec());
        if (!p1Opt.isDefined()) break;
        Piece p1 = p1Opt.get();

        // Iterate over all pieces on the board
        b1.pieces().foreach(new scala.runtime.AbstractFunction1<scala.collection.immutable.List<Piece>, scala.runtime.BoxedUnit>() {
          public scala.runtime.BoxedUnit apply(scala.collection.immutable.List<Piece> enemyPieces) {
            scala.collection.Iterator<Piece> epIter = enemyPieces.iterator();
            while (epIter.hasNext()) {
              Piece enemyPiece = epIter.next();
              Attack attack = new Attack(p1.spec(), enemyPiece.spec());
              @SuppressWarnings("unchecked")
              Try<Object> result = (Try<Object>) (Object) b1.tryLegality(attack, game.externalInfo());
              if (result.isSuccess()) {
                scala.collection.immutable.List<PlayerAction> actionsList =
                    scala.collection.immutable.List$.MODULE$.<PlayerAction>empty().$colon$colon(attack);
                out.accept(new Protocol.DoBoardAction(0, new PlayerActions(actionsList, makeActionId())));
                sleep(100);
              }
            }
            return scala.runtime.BoxedUnit.UNIT;
          }
        });
      }
    }

    // Spawn reinforcements
    BoardState spawnBoard = game.boards()[0].curState();
    SideArray<scala.collection.immutable.Map<String, Object>> reinforcements = spawnBoard.reinforcements();
    scala.collection.immutable.Map<String, Object> myReinforcements = reinforcements.apply(S1());
    Map<String, Object> javaReinf = JavaConverters.mapAsJavaMapConverter(myReinforcements).asJava();

    List<String> reinforcementList = new ArrayList<>();
    for (Map.Entry<String, Object> entry : javaReinf.entrySet()) {
      String piecename = entry.getKey();
      int n = (Integer) entry.getValue();
      for (int i = 0; i < n; i++) {
        reinforcementList.add(piecename);
      }
    }
    // Shuffle reinforcements
    reinforcementList = aiRand.shuffle(reinforcementList);

    for (String piecename : reinforcementList) {
      BoardState rb = game.boards()[0].curState();
      PieceStats stats = pieceMap.get(piecename);
      if (stats == null) continue;

      scala.collection.immutable.List<Loc> spawnLocs = rb.legalSpawnLocs(stats);
      Map<Loc, Integer> locsMap = new HashMap<>();
      scala.collection.Iterator<Loc> slIter = spawnLocs.iterator();
      while (slIter.hasNext()) {
        Loc loc = slIter.next();
        locsMap.put(loc, 0);
      }

      Option<Loc> bestLocOpt = bestMove(stats, stats, locsMap);
      if (bestLocOpt.isDefined()) {
        Loc bestLoc = bestLocOpt.get();
        scala.collection.immutable.List<PlayerAction> actionsList =
            scala.collection.immutable.List$.MODULE$.<PlayerAction>empty().$colon$colon(new Spawn(bestLoc, piecename));
        out.accept(new Protocol.DoBoardAction(0, new PlayerActions(actionsList, makeActionId())));
        sleep(100);
      }
    }

    // Tech
    TechState[] techsForTech = game.game().techLine();
    SideArray<TechLevel> level2 = techsForTech.length > 2 ? techsForTech[2].level() : null;
    if (level2 != null && !level2.apply(S1()).equals(TechAcquired$.MODULE$)) {
      // Open with initiates
      out.accept(new Protocol.DoGameAction(new BuyExtraTechAndSpell(S1())));
      out.accept(new Protocol.DoGameAction(new PerformTech(S1(), 2)));
      out.accept(new Protocol.DoGameAction(new PerformTech(S1(), 2)));
    } else {
      List<Integer> availableTechs = new ArrayList<>();
      for (int i = 0; i < techsForTech.length; i++) {
        TechLevel myLevel = techsForTech[i].level().apply(S1());
        boolean prevOk = (i == 0) || !techsForTech[i - 1].level().apply(S1()).equals(TechLocked$.MODULE$);
        boolean enemyNotAcquired = !techsForTech[i].level().apply(S0()).equals(TechAcquired$.MODULE$);
        if (!myLevel.equals(TechAcquired$.MODULE$) && prevOk && enemyNotAcquired) {
          availableTechs.add(i);
        }
      }
      if (!availableTechs.isEmpty()) {
        int chosenTech = availableTechs.get(aiRand.nextInt(availableTechs.size()));
        out.accept(new Protocol.DoGameAction(new PerformTech(S1(), chosenTech)));
      }
    }

    // End turn
    out.accept(new Protocol.DoGameAction(new SetBoardDone(0, true)));
  }

  @SuppressWarnings("unchecked")
  private void handleTutorial() {
    BoardState board = game.boards()[0].curState();
    TechState[] techs = game.game().techLine();

    if (tutorialStep == 0) {
      tutorialStep = 1;
      chat("Welcome to Minions of Darkness!");
      chat("You currently occupy the top left of the board.");
      chat("You begin the game with your necromancer and six zombies.");
      chat("Move units by clicking on them and dragging them to a new hex.");
      chat("Undo by right-clicking on the unit.");
      chat("For now, move zombies onto the two nearby graveyards.");
    } else if (tutorialStep == 1 && board.endOfTurnSouls(S0()) == 4) {
      tutorialStep = 2;
      chat("");
      chat("You now control the first graveyard.");
      chat("Your souls per turn went from 3 to 4.");
      chat("You can see your souls and souls per turn near the top-left of the screen.");
      chat("Each graveyard you control gives you +1 soul at the end of your turn.");
      chat("Souls are the most important resource in the game.");
      chat("You can spend them to buy more units or unlock new types of units.");
      chat("Claim the other nearby graveyard.");
    } else if (tutorialStep <= 2 && board.endOfTurnSouls(S0()) == 5) {
      tutorialStep = 3;
      chat("");
      chat("You now control both graveyards, and earn 5 souls per turn.");
      chat("Just above the board is the spell row. You can choose from one of two spells each turn.");
      chat("Choose a spell now.");
    } else if (tutorialStep == 3) {
      scala.collection.immutable.Map<Object, Object> bws = game.game().boardsWithSpells();
      Option<Object> bws0 = bws.get(0);
      if (bws0.isDefined() && ((Integer) bws0.get()) == 1) {
        tutorialStep = 4;
        chat("");
        chat("The spell appears in your hand, to the left of the board.");
        chat("You get one spell per turn, and you can play as many as you want on your turn.");
        chat("You play a spell by click-dragging it to a valid target.");
        chat("You can undo casting the spell by right clicking it near the bottom of the screen");
        chat("You can also undo your choice of spell (if you decide you want the other one) by right-clicking the spell you chose.");
        chat("(You can right-click the spell you chose either in your hand or in the spell row).");
        chat("That's almost it for your first turn! Move your necromancer and other zombies forward.");
        chat("When you're ready, click 'End Turn' (near the upper-right corner of the board).");
      }
    } else if (tutorialStep == 4 && game.game().turnNumber() == 2) {
      tutorialStep = 5;
      chat("");
      chat("I took my turn; now it's your second turn. Like you, I occupied the nearby graveyards.");
      chat("I also unlocked the 'Initiate' unit");
      chat("You can see the tech line above the spell row");
      chat("There are 23 different units to unlock, and three starting units");
      chat("");
      chat("The first starting unit is the 'Zombie', which you started with 6 of.");
      chat("Zombies are really weak. Their biggest problem is that they are lumbering");
      chat("They can't move and attack in the same turn, so they can only attack things that started next to them");
      chat("They're good at standing on graveyards though!");
      chat("");
      chat("The second starting unit is the 'Acolyte'");
      chat("They can't fight at all, but they are fast, and you can place units around them.");
      chat("(Normally you can only place units next to your Necromancer)");
      chat("Try buying one now by clicking on the Acolyte hex in the tech row");
    } else if (tutorialStep == 5) {
      // Check if acolyte is in reinforcements
      SideArray<scala.collection.immutable.Map<String, Object>> reinf = board.reinforcements();
      scala.collection.immutable.Map<String, Object> s0Reinf = reinf.apply(S0());
      Option<Object> acolyteCount = s0Reinf.get(Units$.MODULE$.acolyte().name());
      if (acolyteCount.isDefined() && ((Integer) acolyteCount.get()) == 1) {
        tutorialStep = 6;
        chat("");
        chat("The acolyte appears in your reinforcements, to the left of the board");
        chat("Place the acolyte next to your necromancer");
      }
    } else if (tutorialStep == 6) {
      // Check if acolyte exists on board for S0
      Map<Object, Piece> pieces = JavaConverters.mapAsJavaMapConverter(board.pieceById()).asJava();
      boolean acolyteOnBoard = false;
      for (Piece p : pieces.values()) {
        if (p.baseStats().name().equals(Units$.MODULE$.acolyte().name()) && p.side().equals(S0())) {
          acolyteOnBoard = true;
          break;
        }
      }
      if (acolyteOnBoard) {
        tutorialStep = 7;
        chat("");
        chat("Newly-placed units can't act the turn you place them, but next turn your acolyte will be ready for action.");
        chat("Units cost souls to create (you can see your souls near the upper-left corner of the screen).");
        chat("You had 5 souls from your necromancer and two graveyards, but you spent them all on the acolyte.");
        chat("We're going to spend the souls on teching instead, so we need to un-buy the acolyte.");
        chat("Right click on the acolyte to move it back to your reinforcements.");
      }
    } else if (tutorialStep == 7) {
      SideArray<scala.collection.immutable.Map<String, Object>> reinf = board.reinforcements();
      scala.collection.immutable.Map<String, Object> s0Reinf = reinf.apply(S0());
      Option<Object> acolyteCount = s0Reinf.get(Units$.MODULE$.acolyte().name());
      if (acolyteCount.isDefined() && ((Integer) acolyteCount.get()) == 1) {
        tutorialStep = 8;
        chat("");
        chat("Right click on the acolyte in your reinforcements to get your souls back");
      }
    } else if (tutorialStep == 8) {
      int s0Souls = (Integer) ((SideArray<Object>) (Object) game.game().souls()).apply(S0());
      if (s0Souls == 5) {
        tutorialStep = 9;
        chat("");
        chat("The third basic unit is the spire");
        chat("Spires can't move at all, but they have high health and damage, and you can spawn units next to them");
        chat("");
        chat("The unit I unlocked is the 'Initiate', which is an upgrade to the acolyte.");
        chat("Like the acolyte, the initiate moves 2 and has spawn, but he is also a good fighter, even better than the spire.");
        chat("Initiates have one big drawback: like zombies, they are lumbering.");
        chat("");
        chat("To defeat my initiates, you'll want to unlock skeletons.");
        chat("Click on the skeleton hex in the tech line");
      }
    } else if (tutorialStep == 9) {
      if (techs.length > 3 && techs[3].level().apply(S0()).equals(TechUnlocked$.MODULE$)) {
        tutorialStep = 10;
        chat("");
        chat("The blue rectangle in the skeleton hex means you've put one point into skeletons.");
        chat("One point lets you go on to the next tech.");
        chat("You need two points in a tech to actually build the unit.");
        chat("You get one free tech point each turn, but you can buy more");
        chat("Click on 'Buy Extra Tech + Spell', to the right of the tech line");
      }
    } else if (tutorialStep == 10) {
      if (game.game().extraTechsAndSpellsThisTurn() == 1) {
        tutorialStep = 11;
        chat("");
        chat("What makes skeletons strong against initiates? Look at the stats of both units.");
        chat("Skeleton do 5 damage per attack, enough to kill an initiate in one hit.");
        chat("And they are *not* lumbering; they can move and attack in the same turn.");
        chat("So in a fight between an initiate and a skeleton, the skeleton will always get the first attack.");
        chat("You can't build units the turn you tech to them.");
        chat("Now click 'Skeleton' again to unlock them");
      }
    } else if (tutorialStep == 11) {
      if (techs.length > 3 && techs[3].level().apply(S0()).equals(TechAcquired$.MODULE$)) {
        tutorialStep = 12;
        chat("");
        chat("You've unlocked skeletons, but you can't build them until next turn.");
        chat("You can never build units the turn you tech to them.");
        chat("");
        chat("Every unit counters and is countered by other units. Which ones?");
        chat("Look at the number in the tech hex for a unit.");
        chat("Unit N is strong against units N-1,N-2, and N+3.");
        chat("(And therefore weak against N+1,N+2, and N-3)");
        chat("Build units that counter your enemy's units to win.");
        chat("");
        chat("You should be able to finish this game on your own.");
        chat("Occupy the graveyards, build up an army, and win.");
        chat("There are two ways to win:");
        chat("1) Kill the enemy necromancer");
        chat("2) Occupy 8 of the 10 graveyards at the *start* of your turn");
        chat("Good luck!");
      }
    }
  }

  private void sleep(long millis) {
    try {
      Thread.sleep(millis);
    } catch (InterruptedException e) {
      Thread.currentThread().interrupt();
    }
  }
}

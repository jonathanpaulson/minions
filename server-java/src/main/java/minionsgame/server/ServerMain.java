package minionsgame.server;

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Iterator;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;

import io.javalin.Javalin;
import io.javalin.http.Context;
import io.javalin.websocket.WsContext;
import io.javalin.websocket.WsMessageContext;

import minionsgame.core.*;
import play.api.libs.json.Format;
import play.api.libs.json.JsValue;
import play.api.libs.json.Json;

import scala.Option;
import scala.Some;
import scala.None$;
import scala.collection.JavaConverters;
import scala.reflect.ClassTag$;

public class ServerMain {

    // ---------------------------------------------------------------------------
    // App paths
    // ---------------------------------------------------------------------------
    static final String APPLICATION_CONF = "./application.conf";
    static final String MAIN_PAGE = "./web/index.html";
    static final String FAVICON = "./web/img/favicon.jpg";
    static final String WEB_JS = "./web/js/";
    static final String WEB_IMG = "./web/img/";

    // ---------------------------------------------------------------------------
    // Logging
    // ---------------------------------------------------------------------------
    private static final SimpleDateFormat TIME_FORMAT = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSSZ");

    static void log(String s) {
        System.out.println(TIME_FORMAT.format(Calendar.getInstance().getTime()) + " " + s);
    }

    // ---------------------------------------------------------------------------
    // Protocol JSON helpers (calling Scala Play JSON from Java)
    // ---------------------------------------------------------------------------
    @SuppressWarnings("unchecked")
    private static final Format<Protocol.Response> responseFormat =
            (Format<Protocol.Response>) (Object) Protocol.responseFormat();

    @SuppressWarnings("unchecked")
    private static final Format<Protocol.Query> queryFormat =
            (Format<Protocol.Query>) (Object) Protocol.queryFormat();

    static String responseToJson(Protocol.Response response) {
        return Json.stringify(responseFormat.writes(response));
    }

    static Protocol.Query parseQuery(String jsonStr) {
        JsValue jsValue = Json.parse(jsonStr);
        scala.util.Either<?, ?> result = queryFormat.reads(jsValue).asEither();
        if (result.isRight()) {
            @SuppressWarnings("unchecked")
            Protocol.Query query = (Protocol.Query) ((scala.util.Right<?, ?>) result).value();
            return query;
        } else {
            throw new RuntimeException("Could not parse as query: " + result.left().get().toString());
        }
    }

    static Protocol.Response parseResponse(String jsonStr) {
        JsValue jsValue = Json.parse(jsonStr);
        scala.util.Either<?, ?> result = responseFormat.reads(jsValue).asEither();
        if (result.isRight()) {
            @SuppressWarnings("unchecked")
            Protocol.Response response = (Protocol.Response) ((scala.util.Right<?, ?>) result).value();
            return response;
        } else {
            throw new RuntimeException("Could not parse as response: " + result.left().get().toString());
        }
    }

    static String queryToJson(Protocol.Query query) {
        return Json.stringify(queryFormat.writes(query));
    }

    // ---------------------------------------------------------------------------
    // GameManager (replaces GameActor)
    // ---------------------------------------------------------------------------
    static class GameManager {
        final ScheduledExecutorService executor = Executors.newSingleThreadScheduledExecutor();
        final GameState state;
        final String gameid;
        final String rundir;

        // Time management
        private static final double REPORT_TIME_PERIOD = 5.0;
        private double now;
        private double turnTimeLeft;
        private ScheduledFuture<?> endTurnJob;
        private ScheduledFuture<?> timeReportJob;

        GameManager(GameState state, String gameid, String rundir) {
            this.state = state;
            this.gameid = gameid;
            this.rundir = rundir;
            this.now = getNow();
            this.turnTimeLeft = state.currentSideSecondsPerTurn();

            // Schedule periodic time reports
            this.timeReportJob = executor.scheduleAtFixedRate(
                    this::onShouldReportTimeLeft,
                    (long) (REPORT_TIME_PERIOD * 1000),
                    (long) (REPORT_TIME_PERIOD * 1000),
                    TimeUnit.MILLISECONDS
            );
        }

        private double getNow() {
            return System.currentTimeMillis() / 1000.0;
        }

        void submit(Runnable event) {
            executor.submit(() -> {
                try {
                    event.run();
                } catch (Throwable e) {
                    System.err.println("Exception in GameManager executor:");
                    e.printStackTrace();
                    System.err.flush();
                }
            });
        }

        private void updateTime() {
            double newNow = getNow();
            if (!state.isPaused()) {
                turnTimeLeft -= (newNow - now);
            }
            now = newNow;

            // Save game state to disk
            // TODO: Saving game state to disk requires Play JSON format for the Scala GameState.
            // The Scala version uses auto-derived Json.format[GameState] which works from Scala.
            // For now, we skip disk persistence from the Java server.
        }

        private Protocol.Response getTimeLeftEvent() {
            updateTime();
            if (state.game().winner().isEmpty()) {
                return new Protocol.ReportTimeLeft(turnTimeLeft);
            } else {
                if (timeReportJob != null) {
                    timeReportJob.cancel(false);
                }
                return null;
            }
        }

        private void maybeBroadcastTimeLeft() {
            Protocol.Response event = getTimeLeftEvent();
            if (event != null) {
                state.broadcastAll(event);
            }
        }

        // The consumer that GameState.handleQuery and doEndOfTurn call
        private final Consumer<ScheduleReason> scheduleEndOfTurn = reason -> {
            scheduleEndOfTurnInternal(reason);
        };

        private void scheduleEndOfTurnInternal(ScheduleReason reason) {
            if (endTurnJob != null) {
                endTurnJob.cancel(false);
                endTurnJob = null;
            }
            updateTime();

            switch (reason.type) {
                case NEW_TURN:
                    turnTimeLeft = state.currentSideSecondsPerTurn();
                    break;
                case NEW_LIMITS:
                    turnTimeLeft = state.currentSideSecondsPerTurn();
                    break;
                case PAUSE:
                    state.setIsPaused(reason.isPaused);
                    state.broadcastAll(new Protocol.ReportPause(state.isPaused()));
                    break;
            }

            if (!state.isPaused()) {
                int turnNumber = state.game().turnNumber();
                long delayMs = (long) (turnTimeLeft * 1000);
                if (delayMs < 0) delayMs = 0;
                endTurnJob = executor.schedule(() -> {
                    onShouldEndTurn(turnNumber);
                }, delayMs, TimeUnit.MILLISECONDS);
            } else {
                endTurnJob = null;
            }
            maybeBroadcastTimeLeft();
        }

        // --- Event handlers (all run on the single-threaded executor) ---

        void onUserJoined(int sessionId, String username, Option<Side> side, Consumer<Protocol.Response> out) {
            submit(() -> {
                state.handleUserJoined(sessionId, username, side, out);
                Protocol.Response timeEvent = getTimeLeftEvent();
                if (timeEvent != null) {
                    out.accept(timeEvent);
                }
            });
        }

        void onUserLeft(int sessionId) {
            submit(() -> {
                state.handleUserLeft(sessionId);
            });
        }

        void onQueryStr(int sessionId, String queryStr) {
            submit(() -> {
                if (state.hasSession(sessionId)) {
                    Consumer<Protocol.Response> out = state.getSessionOut(sessionId);
                    try {
                        Protocol.Query query = parseQuery(queryStr);
                        state.handleQuery(query, out, sessionId, scheduleEndOfTurn);
                    } catch (Exception e) {
                        out.accept(new Protocol.QueryError("Could not parse as query/json: " + e.getLocalizedMessage()));
                    }
                }
            });
        }

        private void onShouldEndTurn(int turnNumberToEnd) {
            // Already on executor thread since scheduled on it
            if (state.game().turnNumber() == turnNumberToEnd && state.game().winner().isEmpty()) {
                state.doEndOfTurn(scheduleEndOfTurn);
            }
        }

        private void onShouldReportTimeLeft() {
            // Already on executor thread since scheduled on it
            maybeBroadcastTimeLeft();
        }

        void onStartGame() {
            submit(() -> {
                state.refillUpcomingSpells();
                state.game().startGame();
            });
        }

        void onResetTime() {
            submit(() -> {
                scheduleEndOfTurnInternal(ScheduleReason.NEW_LIMITS);
            });
        }

        void shutdown() {
            executor.shutdownNow();
        }
    }

    // ---------------------------------------------------------------------------
    // Scala interop helpers
    // ---------------------------------------------------------------------------
    @SuppressWarnings("unchecked")
    private static scala.reflect.ClassTag<Object> doubleClassTag() {
        return (scala.reflect.ClassTag<Object>) (Object) ClassTag$.MODULE$.Double();
    }

    @SuppressWarnings("unchecked")
    private static scala.reflect.ClassTag<Object> intClassTag() {
        return (scala.reflect.ClassTag<Object>) (Object) ClassTag$.MODULE$.Int();
    }

    @SuppressWarnings("unchecked")
    private static SideArray<Object> createDoublesSideArray(double v0, double v1) {
        return SideArray$.MODULE$.createTwo((Object) v0, (Object) v1, doubleClassTag());
    }

    @SuppressWarnings("unchecked")
    private static SideArray<Object> createDoublesSideArray(double v) {
        return SideArray$.MODULE$.create((Object) v, doubleClassTag());
    }

    @SuppressWarnings("unchecked")
    private static SideArray<Object> createIntsSideArray(int v0, int v1) {
        return SideArray$.MODULE$.createTwo((Object) v0, (Object) v1, intClassTag());
    }

    @SuppressWarnings("unchecked")
    private static SideArray<Object> createIntsSideArray(int v) {
        return SideArray$.MODULE$.create((Object) v, intClassTag());
    }

    @SuppressWarnings("unchecked")
    private static <T> Option<T> none() {
        return (Option<T>) None$.MODULE$;
    }

    @SuppressWarnings("unchecked")
    private static <T> Option<T> some(T value) {
        return new Some<>(value);
    }

    @SuppressWarnings("unchecked")
    private static scala.collection.immutable.List<String> scalaList(java.util.List<String> javaList) {
        scala.collection.immutable.List<String> result =
            (scala.collection.immutable.List<String>) (Object) scala.collection.immutable.Nil$.MODULE$;
        for (int i = javaList.size() - 1; i >= 0; i--) {
            result = result.$colon$colon(javaList.get(i));
        }
        return result;
    }

    // ---------------------------------------------------------------------------
    // Global state
    // ---------------------------------------------------------------------------
    private static final ConcurrentHashMap<String, GameManager> games = new ConcurrentHashMap<>();
    private static final AtomicInteger nextSessionId = new AtomicInteger();

    // Map from WsContext session ID to game session ID
    private static final ConcurrentHashMap<String, Integer> wsSessionIds = new ConcurrentHashMap<>();
    // Map from WsContext session ID to game ID
    private static final ConcurrentHashMap<String, String> wsGameIds = new ConcurrentHashMap<>();

    private static String chooseGameName(String prefix) {
        int suffix = 0;
        while (games.containsKey(prefix + suffix)) {
            suffix++;
        }
        String ret = prefix + suffix;
        assert !games.containsKey(ret);
        return ret;
    }

    // ---------------------------------------------------------------------------
    // AI connection
    // ---------------------------------------------------------------------------
    private static void addAI(GameState gameState, String gameid, boolean doTutorial) {
        GameManager manager = games.get(gameid);
        if (manager == null) return;

        // Queue for messages from game to AI
        LinkedBlockingQueue<Protocol.Response> aiInbox = new LinkedBlockingQueue<>(128);

        // The AI sends queries back through the game manager
        int aiSessionId = nextSessionId.getAndIncrement();

        // Consumer that AI uses to send queries - converts query to JSON string
        // and feeds it as a QueryStr event to the game manager
        Consumer<Protocol.Query> aiQueryOut = query -> {
            String jsonStr = queryToJson(query);
            manager.onQueryStr(aiSessionId, jsonStr);
        };

        AI ai = new AI(aiQueryOut, gameState, doTutorial);

        // Create a consumer for sending responses to the AI (goes through the queue)
        Consumer<Protocol.Response> aiResponseConsumer = response -> {
            try {
                aiInbox.put(response);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        };

        // Register the AI as a user on the game (side 1)
        Option<Side> aiSide = some(S1$.MODULE$);
        manager.onUserJoined(aiSessionId, "igor", aiSide, aiResponseConsumer);

        // Start AI processing thread
        Thread aiThread = new Thread(() -> {
            try {
                while (!Thread.currentThread().isInterrupted()) {
                    Protocol.Response response = aiInbox.take();
                    ai.handleMessage(response);
                }
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }, "AI-" + gameid);
        aiThread.setDaemon(true);
        aiThread.start();
    }

    // ---------------------------------------------------------------------------
    // HTML helpers
    // ---------------------------------------------------------------------------

    private static String gamesListHtml() {
        StringBuilder html = new StringBuilder();
        html.append(
                "<link rel=\"icon\" href=\"/img/favicon.png?v=4\" />\n" +
                "<style>\n" +
                "  body {\n" +
                "    font-family: \"Inter var\", sans-serif;\n" +
                "    margin: 0;\n" +
                "  }\n" +
                "  .button {\n" +
                "    background-color: #4caf50; /* Green */\n" +
                "    border: none;\n" +
                "    color: white;\n" +
                "    padding: 15px 32px;\n" +
                "    text-align: center;\n" +
                "    text-decoration: none;\n" +
                "    display: inline-block;\n" +
                "    font-size: 16px;\n" +
                "  }\n" +
                "\n" +
                "  table {\n" +
                "    font-family: \"Trebuchet MS\", Arial, Helvetica, sans-serif;\n" +
                "    border-collapse: collapse;\n" +
                "    width: 100%;\n" +
                "  }\n" +
                "\n" +
                "  td,\n" +
                "  th {\n" +
                "    border: 1px solid #ddd;\n" +
                "    padding: 8px;\n" +
                "  }\n" +
                "\n" +
                "  tr:nth-child(even) {\n" +
                "    background-color: #f2f2f2;\n" +
                "  }\n" +
                "\n" +
                "  tr:hover {\n" +
                "    background-color: #ddd;\n" +
                "  }\n" +
                "\n" +
                "  th {\n" +
                "    padding-top: 12px;\n" +
                "    padding-bottom: 12px;\n" +
                "    text-align: left;\n" +
                "    background-color: #4caf50;\n" +
                "    color: white;\n" +
                "  }\n" +
                "\n" +
                "  .container {\n" +
                "    max-width: 800px;\n" +
                "    margin: 0 auto;\n" +
                "    padding: 0 16px;\n" +
                "  }\n" +
                "  .title {\n" +
                "    text-align: center;\n" +
                "    font-weight: 800;\n" +
                "    margin-bottom: 0;\n" +
                "    padding-top: 3rem;\n" +
                "  }\n" +
                "  .subtitle {\n" +
                "    text-align: center;\n" +
                "    margin: 0;\n" +
                "    padding-top: 0.75rem;\n" +
                "  }\n" +
                "  .links {\n" +
                "    text-align: center;\n" +
                "    margin: 0;\n" +
                "    padding-top: 0.25rem;\n" +
                "  }\n" +
                "  .buttons {\n" +
                "    padding-top: 1.5rem;\n" +
                "    padding-bottom: 1.5rem;\n" +
                "    display: flex;\n" +
                "    justify-content: center;\n" +
                "    margin: -8px;\n" +
                "    flex-wrap: wrap;\n" +
                "  }\n" +
                "  .buttons > a {\n" +
                "    margin: 8px;\n" +
                "  }\n" +
                "</style>\n" +
                "<link\n" +
                "  rel=\"preload\"\n" +
                "  href=\"https://rsms.me/inter/inter.css\"\n" +
                "  as=\"style\"\n" +
                "  onload=\"this.onload=null;this.rel='stylesheet'\"\n" +
                "/>\n" +
                "\n" +
                "<div class=\"container\">\n" +
                "  <h1 class=\"title\">Minions Game</h1>\n" +
                "  <p class=\"subtitle\">\n" +
                "    Minions is a multiplayer team tactics hex grid open source game.\n" +
                "  </p>\n" +
                "  <p class=\"links\">\n" +
                "    <a href=\"https://discord.gg/BsQVky\">Discord</a> &middot;\n" +
                "    <a href=\"https://github.com/jonathanpaulson/minions/\">Github</a>\n" +
                "  </p>\n" +
                "\n" +
                "  <div class=\"buttons\">\n" +
                "    <a href=\"/newGame\" class=\"button\">New Game</a>\n" +
                "    <a href=\"/ai?difficulty=10\" class=\"button\">Vs AI (1v1)</a>\n" +
                "    <a href=\"/ai?difficulty=0&tutorial=true\" class=\"button\">Tutorial</a>\n" +
                "  </div>\n"
        );

        if (!games.isEmpty()) {
            html.append("<table border=1><tr><th>Game</th><th>Access</th><th>Boards</th><th>Blue Team</th><th>Red Team</th><th>Spectators</th></tr>\n");
            for (Map.Entry<String, GameManager> entry : games.entrySet()) {
                String game = entry.getKey();
                GameState state = entry.getValue().state;

                String hasPassword = state.password().isEmpty() ? "Public" : "Password";
                int nBoards = state.numBoards();

                String blue = teamString(state, some(S0$.MODULE$));
                String red = teamString(state, some(S1$.MODULE$));
                String spectators = teamString(state, none());

                html.append("<tr>\n")
                        .append("  <td>").append(game).append(" <a href='/edit?game=").append(game).append("'>Edit</a></td>\n")
                        .append("  <td>").append(hasPassword).append("</td>\n")
                        .append("  <td>").append(nBoards).append("</td>\n")
                        .append("  <td><a href='/play?game=").append(game).append("&side=0'>Join</a> ").append(blue).append("</td>\n")
                        .append("  <td><a href='/play?game=").append(game).append("&side=1'>Join</a> ").append(red).append("</td>\n")
                        .append("  <td><a href='/play?game=").append(game).append("'>Spectate</a> ").append(spectators).append("</td>\n")
                        .append("</tr>\n");
            }
            html.append("</table>\n");
        }
        html.append("</div>\n");
        return html.toString();
    }

    private static String teamString(GameState state, Option<Side> side) {
        StringBuilder sb = new StringBuilder();
        boolean first = true;
        for (Map.Entry<Integer, Option<Side>> entry : state.getUserSides().entrySet()) {
            int sid = entry.getKey();
            Option<Side> sidSide = entry.getValue();
            if (sidSide.equals(side)) {
                if (!first) sb.append(",");
                sb.append(state.getUsernameOfSession().get(sid));
                first = false;
            }
        }
        return sb.toString();
    }

    private static String newGameFormHtml(Config config) {
        String gameName = chooseGameName("game");
        double blueSecondsPerTurn = config.getDouble("app.s0SecondsPerTurn");
        double redSecondsPerTurn = config.getDouble("app.s1SecondsPerTurn");
        int targetNumWins = config.getInt("app.targetNumWins");
        int blueStartingSouls = config.getInt("app.s0StartingSoulsPerBoard");
        int redStartingSouls = config.getInt("app.s1StartingSoulsPerBoard");
        int blueSoulsPerTurn = config.getInt("app.s0ExtraSoulsPerTurn");
        int redSoulsPerTurn = config.getInt("app.s1ExtraSoulsPerTurn");
        int extraTechCost = config.getInt("app.extraTechCostPerBoard");

        // Vacuum test HTML
        StringBuilder vacuumHtml = new StringBuilder();
        String[] attrs = {"Name", "Cost", "Rebate", "Attack", "Health", "Speed", "Range", "NumAttacks"};
        String[] bools = {"Swarm", "Lumbering", "Spawn", "Persistent", "Flying", "Blink"};
        for (String attr : attrs) {
            vacuumHtml.append("<div class=\"field\">\n")
                    .append("  <label>").append(attr).append("</label>\n")
                    .append("  <input type=\"text\" name=\"blue").append(attr).append("\" autocomplete=\"off\">\n")
                    .append("  <input type=\"text\" name=\"red").append(attr).append("\" autocomplete=\"off\">\n")
                    .append("</div>\n");
        }
        for (String attr : bools) {
            vacuumHtml.append("<div class=\"field\">\n")
                    .append("  <label>").append(attr).append("</label>\n")
                    .append("  <label class=\"checkbox\"><input type=checkbox name=\"blue").append(attr).append("\" value=\"true\"><span>Blue</span></label>\n")
                    .append("  <label class=\"checkbox\"><input type=checkbox name=\"red").append(attr).append("\" value=\"true\"><span>Red</span></label>\n")
                    .append("</div>\n");
        }
        // Ability selects
        StringBuilder abilityOptions = new StringBuilder();
        @SuppressWarnings("unchecked")
        scala.collection.immutable.Map<String, PieceAbility> abilityMap =
                (scala.collection.immutable.Map<String, PieceAbility>) (Object) Abilities$.MODULE$.abilityMap();
        Map<String, PieceAbility> javaAbilityMap = JavaConverters.mapAsJavaMapConverter(abilityMap).asJava();
        for (String name : javaAbilityMap.keySet()) {
            abilityOptions.append("<option value=\"").append(name).append("\">").append(name).append("</option>\n");
        }
        vacuumHtml.append("<div class=\"field\">\n")
                .append("  <label>Ability</label>\n")
                .append("  <select name=\"blueAbility\"><option value=\"\"></option>").append(abilityOptions).append("</select>\n")
                .append("  <select name=\"redAbility\"><option value=\"\"></option>").append(abilityOptions).append("</select>\n")
                .append("</div>\n");

        // Map checkboxes
        StringBuilder mapHtml = new StringBuilder();
        @SuppressWarnings("unchecked")
        scala.collection.immutable.Map<String, Object> basicMaps =
                (scala.collection.immutable.Map<String, Object>) (Object) BoardMaps$.MODULE$.basicMaps();
        @SuppressWarnings("unchecked")
        scala.collection.immutable.Map<String, Object> advancedMaps =
                (scala.collection.immutable.Map<String, Object>) (Object) BoardMaps$.MODULE$.advancedMaps();
        Map<String, Object> javaBasicMaps = JavaConverters.mapAsJavaMapConverter(basicMaps).asJava();
        Map<String, Object> javaAdvancedMaps = JavaConverters.mapAsJavaMapConverter(advancedMaps).asJava();
        for (String mapName : javaBasicMaps.keySet()) {
            mapHtml.append("<label class=\"checkbox\"><input type=checkbox name=map value=\"")
                    .append(mapName).append("\"></input><span>").append(mapName).append("</span></label>\n");
        }
        for (String mapName : javaAdvancedMaps.keySet()) {
            mapHtml.append("<label class=\"checkbox\"><input type=checkbox name=map value=\"")
                    .append(mapName).append("\"></input><span>").append(mapName).append("</span></label>\n");
        }

        return "<head>\n" +
                "  <link rel=\"icon\" href=\"/img/favicon.png?v=4\" />\n" +
                "  <style type=\"text/css\">\n" +
                "    * {\n" +
                "      box-sizing: border-box;\n" +
                "    }\n" +
                "\n" +
                "    body {\n" +
                "      font-family: \"Inter var\", sans-serif;\n" +
                "      margin: 0;\n" +
                "    }\n" +
                "\n" +
                "    .container {\n" +
                "      padding: 3rem 1rem;\n" +
                "    }\n" +
                "    .title {\n" +
                "      text-align: center;\n" +
                "      font-weight: 800;\n" +
                "      margin-bottom: 0;\n" +
                "      padding-bottom: 1.5rem;\n" +
                "    }\n" +
                "    form {\n" +
                "      margin: 0 auto;\n" +
                "      width: 636px; /* 220px + 8px + 200px + 8px + 200px */\n" +
                "    }\n" +
                "    .field {\n" +
                "      display: flex;\n" +
                "      align-items: center;\n" +
                "    }\n" +
                "    .field > label, .field > input, .field > select {\n" +
                "      width: 200px;\n" +
                "    }\n" +
                "    .field > label:first-child {\n" +
                "      flex-shrink: 0;\n" +
                "      text-align: right;\n" +
                "      padding-right: 8px;\n" +
                "      width: 220px;\n" +
                "    }\n" +
                "    .field:not(:first-of-type) {\n" +
                "      margin-top: 0.5rem;\n" +
                "    }\n" +
                "    .field > label {\n" +
                "      color: rgb(55,65,81);\n" +
                "      font-size: 0.875rem;\n" +
                "    }\n" +
                "    .field > input, .field > select {\n" +
                "      background-color: #fff;\n" +
                "      border: 1px solid rgb(209,213,219);\n" +
                "      border-radius: .375rem;\n" +
                "      padding-top: .25rem;\n" +
                "      padding-right: .5rem;\n" +
                "      padding-bottom: .25rem;\n" +
                "      padding-left: .5rem;\n" +
                "      font-size: 0.875rem;\n" +
                "      color: inherit;\n" +
                "      font-family: inherit;\n" +
                "      line-height: 1.25rem;\n" +
                "      box-shadow: rgba(0, 0, 0, 0.05) 0px 1px 2px 0px;\n" +
                "    }\n" +
                "    .field > *+* {\n" +
                "      margin-left: 0.5rem;\n" +
                "    }\n" +
                "\n" +
                "    .accordion {\n" +
                "      margin: 1.5rem 0;\n" +
                "    }\n" +
                "    .accordion h3 {\n" +
                "      display: flex;\n" +
                "      margin: 0;\n" +
                "      margin-bottom: 0.75rem;\n" +
                "    }\n" +
                "    .accordion h3::before {\n" +
                "      /* Thanks Bootstrap! */\n" +
                "      flex-shrink: 0;\n" +
                "      width: 1.25rem;\n" +
                "      height: 1.25rem;\n" +
                "      margin-right: 0.75rem;\n" +
                "      content: \"\";\n" +
                "      background-image: url(\"data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 16 16' fill='%23212529'%3e%3cpath fill-rule='evenodd' d='M1.646 4.646a.5.5 0 0 1 .708 0L8 10.293l5.646-5.647a.5.5 0 0 1 .708.708l-6 6a.5.5 0 0 1-.708 0l-6-6a.5.5 0 0 1 0-.708z'/%3e%3c/svg%3e\");\n" +
                "      background-repeat: no-repeat;\n" +
                "      background-size: 1.25rem;\n" +
                "      transition: transform .2s ease-in-out;\n" +
                "    }\n" +
                "    .accordion h3.ui-accordion-header-active::before {\n" +
                "      background-image: url(\"data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 16 16' fill='%230c63e4'%3e%3cpath fill-rule='evenodd' d='M1.646 4.646a.5.5 0 0 1 .708 0L8 10.293l5.646-5.647a.5.5 0 0 1 .708.708l-6 6a.5.5 0 0 1-.708 0l-6-6a.5.5 0 0 1 0-.708z'/%3e%3c/svg%3e\");\n" +
                "      transform: rotate(-180deg);\n" +
                "    }\n" +
                "\n" +
                "    .checkbox {\n" +
                "      display: flex;\n" +
                "      padding: 0.25rem 0;\n" +
                "      align-items: center;\n" +
                "    }\n" +
                "    input[type=\"checkbox\"] {\n" +
                "      -webkit-appearance: none;\n" +
                "      -moz-appearance: none;\n" +
                "      appearance: none;\n" +
                "      padding: 0;\n" +
                "      -webkit-print-color-adjust: exact;\n" +
                "      color-adjust: exact;\n" +
                "      display: inline-block;\n" +
                "      vertical-align: middle;\n" +
                "      background-origin: border-box;\n" +
                "      -webkit-user-select: none;\n" +
                "      -moz-user-select: none;\n" +
                "      -ms-user-select: none;\n" +
                "      user-select: none;\n" +
                "      flex-shrink: 0;\n" +
                "      height: 1rem;\n" +
                "      width: 1rem;\n" +
                "      color: #2563eb;\n" +
                "      background-color: #fff;\n" +
                "      border: 1px solid rgb(209,213,219);\n" +
                "      border-radius: .25rem;\n" +
                "      box-shadow: 0 1px 2px 0 rgba(0, 0, 0, 0.05);\n" +
                "    }\n" +
                "    input[type=\"checkbox\"]:checked {\n" +
                "      border-color: transparent;\n" +
                "      background-color: currentColor;\n" +
                "      background-size: 100% 100%;\n" +
                "      background-position: center;\n" +
                "      background-repeat: no-repeat;\n" +
                "      background-image: url(\"data:image/svg+xml,%3csvg viewBox='0 0 16 16' fill='white' xmlns='http://www.w3.org/2000/svg'%3e%3cpath d='M12.207 4.793a1 1 0 010 1.414l-5 5a1 1 0 01-1.414 0l-2-2a1 1 0 011.414-1.414L6.5 9.086l4.293-4.293a1 1 0 011.414 0z'/%3e%3c/svg%3e\");\n" +
                "    }\n" +
                "    .checkbox > span {\n" +
                "      margin-left: 0.5rem;\n" +
                "    }\n" +
                "\n" +
                "    .button {\n" +
                "      background-color: #4caf50; /* Green */\n" +
                "      border: none;\n" +
                "      color: white;\n" +
                "      padding: 15px 32px;\n" +
                "      text-align: center;\n" +
                "      text-decoration: none;\n" +
                "      display: inline-block;\n" +
                "      font-size: 16px;\n" +
                "    }\n" +
                "  </style>\n" +
                "  <link\n" +
                "    rel=\"preload\"\n" +
                "    href=\"https://rsms.me/inter/inter.css\"\n" +
                "    as=\"style\"\n" +
                "    onload=\"this.onload=null;this.rel='stylesheet'\"\n" +
                "  />\n" +
                "  <script src=\"https://code.jquery.com/jquery-1.12.4.js\"></script>\n" +
                "  <script src=\"https://code.jquery.com/ui/1.12.1/jquery-ui.js\"></script>\n" +
                "  <script>\n" +
                "    $(document).ready(function() {\n" +
                "      $(\".accordion\").accordion({\n" +
                "        collapsible: true,\n" +
                "        active: false\n" +
                "      });\n" +
                "    });\n" +
                "  </script>\n" +
                "</head>\n" +
                "<body>\n" +
                "  <div class=\"container\">\n" +
                "    <h1 class=\"title\">Create a Game</h1>\n" +
                "    <form method=post>\n" +
                "      <div class=\"field\">\n" +
                "        <label>Game Name</label>\n" +
                "        <input type=\"text\" name=\"game\" value=\"" + gameName + "\"></input>\n" +
                "      </div>\n" +
                "      <div class=\"field\">\n" +
                "        <label>Password (optional)</label>\n" +
                "        <input type=\"text\" name=\"password\"></input>\n" +
                "      </div>\n" +
                "      <div class=\"field\">\n" +
                "        <label>Seconds Per Turn (Blue / Red)</label>\n" +
                "        <input type=\"text\" name=blueSeconds value=\"" + blueSecondsPerTurn + "\"></input>\n" +
                "        <input type=\"text\" name=redSeconds value=\"" + redSecondsPerTurn + "\">\n" +
                "      </div>\n" +
                "      <div class=\"field\">\n" +
                "        <label>Points to win</label>\n" +
                "        <input type=\"text\" name=targetWins value=\"" + targetNumWins + "\"></input>\n" +
                "      </div>\n" +
                "\n" +
                "      <div class=\"accordion\">\n" +
                "        <h3>Maps</h3>\n" +
                "        <div>\n" +
                "          " + mapHtml.toString() + "\n" +
                "        </div>\n" +
                "      </div>\n" +
                "\n" +
                "      <div class=\"accordion\">\n" +
                "        <h3>Vacuum Test</h3>\n" +
                "        <div>\n" +
                "          <table>" + vacuumHtml.toString() + "</table>\n" +
                "        </div>\n" +
                "      </div>\n" +
                "\n" +
                "      <div class=\"accordion\">\n" +
                "        <h3>Advanced Options</h3>\n" +
                "        <div>\n" +
                "          <div class=\"field\">\n" +
                "            <label>Random Seed (optional)</label>\n" +
                "            <input type=\"text\" name=\"seed\">\n" +
                "          </div>\n" +
                "          <div class=\"field\">\n" +
                "            <label>Starting Souls per Board</label>\n" +
                "            <input type=\"text\" name=blueSouls value=" + blueStartingSouls + ">\n" +
                "            <input type=\"text\" name=redSouls value=" + redStartingSouls + ">\n" +
                "          </div>\n" +
                "          <div class=\"field\">\n" +
                "            <label>Extra Souls per Turn</label>\n" +
                "            <input type=text name=blueSoulsPerTurn value=" + blueSoulsPerTurn + ">\n" +
                "            <input type=text name=redSoulsPerTurn value=" + redSoulsPerTurn + ">\n" +
                "          </div>\n" +
                "          <div class=\"field\">\n" +
                "            <label>Tech Cost per Board</label>\n" +
                "            <input type=text name=techSouls value=" + extraTechCost + ">\n" +
                "          </div>\n" +
                "        </div>\n" +
                "      </div>\n" +
                "\n" +
                "      <button class=\"button\" type=\"submit\">Start Game</button>\n" +
                "    </form>\n" +
                "  </div>\n" +
                "</body>\n";
    }

    private static String editGameFormHtml(String gameid, GameManager manager) {
        double blueSecondsPerTurn = (Double) manager.state.secondsPerTurn().apply(S0$.MODULE$);
        double redSecondsPerTurn = (Double) manager.state.secondsPerTurn().apply(S1$.MODULE$);
        return "<head>\n" +
                "  <link rel=\"icon\" href=\"/img/favicon.png?v=4\" />\n" +
                "  <style type=\"text/css\">\n" +
                "    form  { display: table;      }\n" +
                "    p     { display: table-row;  }\n" +
                "    label { display: table-cell; }\n" +
                "    input { display: table-cell; }\n" +
                "  </style>\n" +
                "</head>\n" +
                "<body>\n" +
                "  <form method=post>\n" +
                "    <p><label>Game name </label><input type=\"text\" name=\"gameid\" value=" + gameid + " readonly></input>\n" +
                "    <p><label>Blue seconds per turn </label><input type=\"text\" name=blueSeconds value=" + blueSecondsPerTurn + "></input><br>\n" +
                "    <p><label>Red seconds per turn </label><input type=\"text\" name=redSeconds value=" + redSecondsPerTurn + "></input><br>\n" +
                "    <p><input type=\"submit\" value=\"Edit Game\"></input>\n" +
                "  </form>\n" +
                "</body>\n";
    }

    // ---------------------------------------------------------------------------
    // MAIN
    // ---------------------------------------------------------------------------
    public static void main(String[] args) {
        // Load config
        Config config = ConfigFactory.parseFile(new File(APPLICATION_CONF));

        String cwd = new File(".").getAbsolutePath();
        log("Running in " + cwd);

        String iface = config.getString("app.interface");
        int port = config.getInt("app.port");
        String rundir = config.getString("app.rundir");

        // Create Javalin app
        Javalin app = Javalin.create(javalinConfig -> {
            // We handle static files manually via routes
        });

        // -----------------------------------------------------------------------
        // ROUTES
        // -----------------------------------------------------------------------

        // GET / - Games list
        app.get("/", ctx -> {
            ctx.html(gamesListHtml());
        });

        // GET /show?game=X - Show game state JSON
        app.get("/show", ctx -> {
            String gameid = ctx.queryParam("game");
            if (gameid == null || gameid.isEmpty()) {
                ctx.result("Please provide 'game=' in URL");
                return;
            }
            GameManager manager = games.get(gameid);
            if (manager == null) {
                ctx.result("Game " + gameid + " does not exist");
                return;
            }
            // TODO: Serialize game state to JSON. This requires the Scala Play JSON format.
            ctx.result("Game state JSON serialization not yet implemented for Java server");
        });

        // GET /play?game=X&side=N&username=Y - Serve game page
        app.get("/play", ctx -> {
            ctx.header("CacheControl", "no-cache, no-store, must-revalidate");
            ctx.header("Expires", "0");
            ctx.header("Pragma", "no-cache");

            String gameid = ctx.queryParam("game");
            if (gameid == null || gameid.isEmpty()) {
                ctx.result("Please provide 'game=' in URL");
                return;
            }
            String username = ctx.queryParam("username");
            if (username == null || username.isEmpty()) {
                // Prompt for username
                ctx.html(
                        "<link rel=\"icon\" href=\"/img/favicon.png?v=4\" />\n" +
                        "<script type=\"text/javascript\">\n" +
                        "var username = window.prompt('Username?', '')\n" +
                        "if(!username || username.length == 0) {\n" +
                        "  window.history.back()\n" +
                        "} else {\n" +
                        "  window.location = window.location.href + '&username=' + username\n" +
                        "}\n" +
                        "</script>\n"
                );
                return;
            }

            GameManager manager = games.get(gameid);
            if (manager == null) {
                ctx.result("Game " + gameid + " does not exist");
                return;
            }

            String password = ctx.queryParam("password");
            Option<String> statePassword = manager.state.password();
            if (statePassword.isDefined()) {
                if (password == null || password.isEmpty()) {
                    ctx.result(gameid + " is password-protected; please provide 'password=' in URL");
                    return;
                }
                if (!password.equals(statePassword.get())) {
                    ctx.result("Wrong password for " + gameid);
                    return;
                }
            }
            // Serve the main page
            File mainPage = new File(MAIN_PAGE);
            if (mainPage.exists()) {
                ctx.html(new String(Files.readAllBytes(mainPage.toPath()), StandardCharsets.UTF_8));
            } else {
                ctx.result("Main page not found: " + MAIN_PAGE);
            }
        });

        // GET /js/* - Static JS files (Javalin 4 wildcard: <filepath> matches multiple segments)
        app.get("/js/<filepath>", ctx -> {
            String filepath = ctx.pathParam("filepath");
            File file = new File(WEB_JS + filepath).getCanonicalFile();
            if (file.exists() && file.isFile() && file.getPath().startsWith(new File(WEB_JS).getCanonicalPath())) {
                ctx.result(Files.readAllBytes(file.toPath()));
                if (filepath.endsWith(".js")) {
                    ctx.contentType("application/javascript");
                }
            } else {
                ctx.status(404).result("Not found");
            }
        });

        // GET /img/* - Static image/audio files (serves nested dirs like /img/terrain/grass0.png)
        app.get("/img/<filepath>", ctx -> {
            String filepath = ctx.pathParam("filepath");
            File file = new File(WEB_IMG + filepath).getCanonicalFile();
            if (file.exists() && file.isFile() && file.getPath().startsWith(new File(WEB_IMG).getCanonicalPath())) {
                ctx.result(Files.readAllBytes(file.toPath()));
                String contentType = guessContentType(filepath);
                if (contentType != null) {
                    ctx.contentType(contentType);
                }
            } else {
                ctx.status(404).result("Not found");
            }
        });

        // GET /ai?difficulty=N&tutorial=bool - Create AI game
        app.get("/ai", ctx -> {
            boolean doTutorial = "true".equals(ctx.queryParam("tutorial"));
            int difficulty;
            try {
                difficulty = Integer.parseInt(ctx.queryParam("difficulty") != null ? ctx.queryParam("difficulty") : "10");
            } catch (NumberFormatException e) {
                difficulty = 10;
            }

            SideArray<Object> secondsPerTurn = createDoublesSideArray(120.0);
            SideArray<Object> startingSouls = createIntsSideArray(0, 6);
            SideArray<Object> extraSoulsPerTurn = createIntsSideArray(0, difficulty);
            int targetWins = 1;
            int techSouls = 4;
            Option<scala.collection.immutable.List<String>> maps_opt = none();
            Option<Object> seed_opt = none();

            String gameid = chooseGameName("ai");
            GameState gameState = GameState.createNormal(
                    secondsPerTurn, startingSouls, extraSoulsPerTurn,
                    targetWins, techSouls, maps_opt, seed_opt, none(), false
            );
            GameManager manager = new GameManager(gameState, gameid, rundir);
            games.put(gameid, manager);
            manager.onStartGame();

            addAI(gameState, gameid, doTutorial);

            ctx.redirect("/play?game=" + gameid + "&side=0");
        });

        // GET /newGame - New game form
        app.get("/newGame", ctx -> {
            ctx.html(newGameFormHtml(config));
        });

        // POST /newGame - Create game from form
        app.post("/newGame", ctx -> {
            String gameid = ctx.formParam("game");
            String password = ctx.formParam("password");
            String seed = ctx.formParam("seed");

            if (gameid == null || gameid.isEmpty()) {
                ctx.result("Game name is required");
                return;
            }

            if (games.containsKey(gameid)) {
                ctx.result("A game named \"" + gameid + "\" already exists; pick a different name");
                return;
            }

            double blueSeconds = parseDouble(ctx.formParam("blueSeconds"), 120.0);
            double redSeconds = parseDouble(ctx.formParam("redSeconds"), 120.0);
            int targetWins = parseInt(ctx.formParam("targetWins"), 1);
            int blueSouls = parseInt(ctx.formParam("blueSouls"), 0);
            int redSouls = parseInt(ctx.formParam("redSouls"), 6);
            int techSouls = parseInt(ctx.formParam("techSouls"), 4);
            int blueSoulsPerTurn = parseInt(ctx.formParam("blueSoulsPerTurn"), 0);
            int redSoulsPerTurn = parseInt(ctx.formParam("redSoulsPerTurn"), 0);

            // Maps
            java.util.List<String> mapsList = ctx.formParams("map");
            Option<scala.collection.immutable.List<String>> maps_opt;
            if (mapsList == null || mapsList.isEmpty()) {
                maps_opt = none();
            } else {
                maps_opt = some(scalaList(mapsList));
            }

            // Seed
            Option<Object> seed_opt;
            if (seed == null || seed.isEmpty()) {
                seed_opt = none();
            } else {
                seed_opt = some((Object) Long.parseLong(seed));
            }

            // Password
            Option<String> passwordOpt;
            if (password == null || password.isEmpty()) {
                passwordOpt = none();
            } else {
                passwordOpt = some(password);
            }

            SideArray<Object> startingSouls = createIntsSideArray(blueSouls, redSouls);
            SideArray<Object> secondsPerTurn = createDoublesSideArray(blueSeconds, redSeconds);
            SideArray<Object> extraSoulsPerTurn = createIntsSideArray(blueSoulsPerTurn, redSoulsPerTurn);

            // Vacuum test units
            String blueName = ctx.formParam("blueName");
            String redName = ctx.formParam("redName");
            String blueAttack = ctx.formParam("blueAttack");
            String redAttack = ctx.formParam("redAttack");
            String blueHealth = ctx.formParam("blueHealth");
            String redHealth = ctx.formParam("redHealth");
            String blueSpeed = ctx.formParam("blueSpeed");
            String redSpeed = ctx.formParam("redSpeed");
            String blueRange = ctx.formParam("blueRange");
            String redRange = ctx.formParam("redRange");
            String blueCost = ctx.formParam("blueCost");
            String redCost = ctx.formParam("redCost");
            String blueRebate = ctx.formParam("blueRebate");
            String redRebate = ctx.formParam("redRebate");
            String blueNumAttacks = ctx.formParam("blueNumAttacks");
            String redNumAttacks = ctx.formParam("redNumAttacks");
            String blueAbility = ctx.formParam("blueAbility");
            String redAbility = ctx.formParam("redAbility");

            Option<String> blueSwarm = optFormParam(ctx, "blueSwarm");
            Option<String> redSwarm = optFormParam(ctx, "redSwarm");
            Option<String> blueLumbering = optFormParam(ctx, "blueLumbering");
            Option<String> redLumbering = optFormParam(ctx, "redLumbering");
            Option<String> blueSpawn = optFormParam(ctx, "blueSpawn");
            Option<String> redSpawn = optFormParam(ctx, "redSpawn");
            Option<String> bluePersistent = optFormParam(ctx, "bluePersistent");
            Option<String> redPersistent = optFormParam(ctx, "redPersistent");
            Option<String> blueFlying = optFormParam(ctx, "blueFlying");
            Option<String> redFlying = optFormParam(ctx, "redFlying");
            Option<String> blueBlink = optFormParam(ctx, "blueBlink");
            Option<String> redBlink = optFormParam(ctx, "redBlink");

            // null-safe defaults for form params
            if (blueName == null) blueName = "";
            if (redName == null) redName = "";
            if (blueAttack == null) blueAttack = "";
            if (redAttack == null) redAttack = "";
            if (blueHealth == null) blueHealth = "";
            if (redHealth == null) redHealth = "";
            if (blueSpeed == null) blueSpeed = "";
            if (redSpeed == null) redSpeed = "";
            if (blueRange == null) blueRange = "";
            if (redRange == null) redRange = "";
            if (blueCost == null) blueCost = "";
            if (redCost == null) redCost = "";
            if (blueRebate == null) blueRebate = "";
            if (redRebate == null) redRebate = "";
            if (blueNumAttacks == null) blueNumAttacks = "";
            if (redNumAttacks == null) redNumAttacks = "";
            if (blueAbility == null) blueAbility = "";
            if (redAbility == null) redAbility = "";

            Option<PieceStats> blueUnit = Units$.MODULE$.fromForm(
                    blueName, blueAttack, blueHealth, blueSpeed, blueRange,
                    blueCost, blueRebate, blueNumAttacks,
                    blueSwarm, blueLumbering, blueSpawn, bluePersistent,
                    blueFlying, blueBlink, blueAbility
            );
            Option<PieceStats> redUnit = Units$.MODULE$.fromForm(
                    redName, redAttack, redHealth, redSpeed, redRange,
                    redCost, redRebate, redNumAttacks,
                    redSwarm, redLumbering, redSpawn, redPersistent,
                    redFlying, redBlink, redAbility
            );

            GameState gameState;
            if (blueUnit.isDefined() && redUnit.isDefined()) {
                gameState = GameState.createVacuum(
                        secondsPerTurn, startingSouls, extraSoulsPerTurn,
                        targetWins, techSouls, maps_opt, seed_opt, passwordOpt,
                        blueUnit.get(), redUnit.get()
                );
            } else {
                gameState = GameState.createNormal(
                        secondsPerTurn, startingSouls, extraSoulsPerTurn,
                        targetWins, techSouls, maps_opt, seed_opt, passwordOpt, false
                );
            }

            GameManager manager = new GameManager(gameState, gameid, rundir);
            manager.onStartGame();
            games.put(gameid, manager);
            System.out.println("Created game " + gameid);

            ctx.html(
                    "<link rel=\"icon\" href=\"/img/favicon.png?v=4\" />\n" +
                    "<pre>\n" +
                    "  Created game " + gameid + "\n" +
                    "\n" +
                    "  password=" + password + "\n" +
                    "  seed=" + seed_opt + "\n" +
                    "  blueSeconds=" + blueSeconds + "\n" +
                    "  redSeconds=" + redSeconds + "\n" +
                    "  targetWins=" + targetWins + "\n" +
                    "  techSouls=" + techSouls + "\n" +
                    "  maps=" + maps_opt + "\n" +
                    "  seed=" + seed_opt + "\n" +
                    "\n" +
                    "  blueSouls=" + blueSouls + "\n" +
                    "  redSouls=" + redSouls + "\n" +
                    "  blueSoulsPerTurn=" + blueSoulsPerTurn + "\n" +
                    "  redSoulsPerTurn=" + redSoulsPerTurn + "\n" +
                    "</pre>\n" +
                    "\n" +
                    "<a href=\"/play?game=" + gameid + "&side=0\">Join blue</a><br>\n" +
                    "<a href=\"/play?game=" + gameid + "&side=1\">Join red</a><br>\n" +
                    "<a href=\"/play?game=" + gameid + "\">Spectate</a><br>\n" +
                    "<a href=\"/\">Back</a>\n"
            );
        });

        // GET /edit?game=X - Edit game form
        app.get("/edit", ctx -> {
            String gameid = ctx.queryParam("game");
            if (gameid == null || gameid.isEmpty()) {
                ctx.result("Please provide 'game=' in URL");
                return;
            }
            GameManager manager = games.get(gameid);
            if (manager == null) {
                ctx.result("Cannot edit nonexistent game " + gameid);
                return;
            }
            ctx.html(editGameFormHtml(gameid, manager));
        });

        // POST /edit - Apply edits
        app.post("/edit", ctx -> {
            String gameid = ctx.formParam("gameid");
            if (gameid == null || gameid.isEmpty()) {
                ctx.result("Game name is required");
                return;
            }
            GameManager manager = games.get(gameid);
            if (manager == null) {
                ctx.result("Game " + gameid + " does not exist");
                return;
            }

            double blueSeconds = parseDouble(ctx.formParam("blueSeconds"), 120.0);
            double redSeconds = parseDouble(ctx.formParam("redSeconds"), 120.0);

            manager.state.setSecondsPerTurn(createDoublesSideArray(blueSeconds, redSeconds));
            manager.onResetTime();

            ctx.html(
                    "<link rel=\"icon\" href=\"/img/favicon.png?v=4\" />\n" +
                    "<pre>\n" +
                    "  Edited game " + gameid + "\n" +
                    "  blueSeconds=" + blueSeconds + "\n" +
                    "  redSeconds=" + redSeconds + "\n" +
                    "</pre>\n" +
                    "\n" +
                    "<a href=\"/play?game=" + gameid + "&side=0\">Join blue</a><br>\n" +
                    "<a href=\"/play?game=" + gameid + "&side=1\">Join red</a><br>\n" +
                    "<a href=\"/play?game=" + gameid + "\">Spectate</a><br>\n" +
                    "<a href=\"/\">Back</a>\n"
            );
        });

        // -----------------------------------------------------------------------
        // WEBSOCKET /playGame
        // -----------------------------------------------------------------------
        app.ws("/playGame", ws -> {
            ws.onConnect(ctx -> {
                String username = ctx.queryParam("username");
                String gameid = ctx.queryParam("game");
                String sideStr = ctx.queryParam("side");

                if (username == null || gameid == null) {
                    ctx.session.close();
                    return;
                }

                GameManager manager = games.get(gameid);
                if (manager == null) {
                    ctx.session.close();
                    return;
                }

                Option<Side> side;
                if ("0".equals(sideStr)) {
                    side = some(S0$.MODULE$);
                } else if ("1".equals(sideStr)) {
                    side = some(S1$.MODULE$);
                } else {
                    side = none();
                }

                int sessionId = nextSessionId.getAndIncrement();
                String wsId = ctx.getSessionId();
                wsSessionIds.put(wsId, sessionId);
                wsGameIds.put(wsId, gameid);

                // Create the consumer that sends Protocol.Response as JSON to the WS client
                Consumer<Protocol.Response> out = response -> {
                    try {
                        if (ctx.session.isOpen()) {
                            String json = responseToJson(response);
                            ctx.send(json);
                        }
                    } catch (Exception e) {
                        log("Error sending WS message: " + e.getMessage());
                    }
                };

                manager.onUserJoined(sessionId, username, side, out);
            });

            ws.onMessage(ctx -> {
                String wsId = ctx.getSessionId();
                Integer sessionId = wsSessionIds.get(wsId);
                String gameid = wsGameIds.get(wsId);

                if (sessionId == null || gameid == null) return;

                GameManager manager = games.get(gameid);
                if (manager == null) return;

                manager.onQueryStr(sessionId, ctx.message());
            });

            ws.onClose(ctx -> {
                String wsId = ctx.getSessionId();
                Integer sessionId = wsSessionIds.remove(wsId);
                String gameid = wsGameIds.remove(wsId);

                if (sessionId == null || gameid == null) return;

                GameManager manager = games.get(gameid);
                if (manager == null) return;

                manager.onUserLeft(sessionId);
            });

            ws.onError(ctx -> {
                String wsId = ctx.getSessionId();
                Integer sessionId = wsSessionIds.remove(wsId);
                String gameid = wsGameIds.remove(wsId);
                if (sessionId != null && gameid != null) {
                    GameManager manager = games.get(gameid);
                    if (manager != null) {
                        manager.onUserLeft(sessionId);
                    }
                }
                if (ctx.error() != null) {
                    log("WebSocket error: " + ctx.error().getMessage());
                }
            });
        });

        // -----------------------------------------------------------------------
        // LOAD OLD GAMES
        // -----------------------------------------------------------------------
        // TODO: Game state persistence not yet implemented for Java server.
        // The Scala server used Play JSON auto-derived format for GameState serialization.
        // To support loading old games, we would need Jackson-based serialization.
        try {
            Path rundirPath = Paths.get(rundir);
            if (Files.exists(rundirPath) && Files.isDirectory(rundirPath)) {
                long count = Files.list(rundirPath).count();
                if (count > 0) {
                    log("Found " + count + " old game(s) in " + rundir + " but loading is not yet supported in the Java server");
                }
            }
        } catch (Exception e) {
            log("Error checking old games directory: " + e.getMessage());
        }

        // -----------------------------------------------------------------------
        // CREATE TEST AI GAME
        // -----------------------------------------------------------------------
        {
            SideArray<Object> secondsPerTurn = createDoublesSideArray(120.0);
            SideArray<Object> startingSouls = createIntsSideArray(0, 6);
            SideArray<Object> extraSoulsPerTurn = createIntsSideArray(0, 10);
            int targetWins = 2;
            int techSouls = 4;

            // Some(List("MegaPuddles"))
            java.util.List<String> mapList = new java.util.ArrayList<>();
            mapList.add("MegaPuddles");
            Option<scala.collection.immutable.List<String>> maps_opt = some(scalaList(mapList));
            Option<Object> seed_opt = none();

            String gameid = chooseGameName("ai_test");
            GameState gameState = GameState.createNormal(
                    secondsPerTurn, startingSouls, extraSoulsPerTurn,
                    targetWins, techSouls, maps_opt, seed_opt, none(), true
            );
            GameManager manager = new GameManager(gameState, gameid, rundir);
            games.put(gameid, manager);
            manager.onStartGame();

            addAI(gameState, gameid, false);
            log("Created test AI game: " + gameid);
        }

        // -----------------------------------------------------------------------
        // START SERVER
        // -----------------------------------------------------------------------
        app.start(iface, port);
        log("Server is listening on " + iface + ":" + port);
    }

    // ---------------------------------------------------------------------------
    // Utility methods
    // ---------------------------------------------------------------------------
    private static double parseDouble(String s, double defaultValue) {
        if (s == null || s.isEmpty()) return defaultValue;
        try {
            return Double.parseDouble(s);
        } catch (NumberFormatException e) {
            return defaultValue;
        }
    }

    private static int parseInt(String s, int defaultValue) {
        if (s == null || s.isEmpty()) return defaultValue;
        try {
            return Integer.parseInt(s);
        } catch (NumberFormatException e) {
            return defaultValue;
        }
    }

    private static Option<String> optFormParam(Context ctx, String name) {
        String value = ctx.formParam(name);
        if (value == null || value.isEmpty()) {
            return none();
        }
        return some(value);
    }

    private static String guessContentType(String filename) {
        if (filename.endsWith(".png")) return "image/png";
        if (filename.endsWith(".jpg") || filename.endsWith(".jpeg")) return "image/jpeg";
        if (filename.endsWith(".gif")) return "image/gif";
        if (filename.endsWith(".svg")) return "image/svg+xml";
        if (filename.endsWith(".ico")) return "image/x-icon";
        if (filename.endsWith(".webp")) return "image/webp";
        return null;
    }
}

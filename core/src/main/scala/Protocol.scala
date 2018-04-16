package minionsgame.core
import scala.reflect.ClassTag
import play.api.libs.json._

object CurrentVersion {
  val version: String = "1.0"
}

object Protocol {
  sealed trait Response
  case class Version(version: String) extends Response
  case class QueryError(err: String) extends Response
  case class Messages(all: List[String], team: List[String]) extends Response
  case class Players(players: SideArray[List[String]], spectators: List[String]) extends Response
  case class ClientHeartbeatRate(periodInSeconds: Double) extends Response
  case class OkHeartbeat(i: Int) extends Response
  case class Initialize(game: Game, summaries: Array[BoardSummary], boardNames: Array[String], boardSequences: Array[Int]) extends Response
  case class UserJoined(username: String, side: Option[Side]) extends Response
  case class UserLeft(username: String, side: Option[Side]) extends Response
  case class OkBoardAction(boardIdx: Int, newBoardSequence: Int) extends Response
  case class OkGameAction(newGameSequence: Int) extends Response
  case class ReportBoardHistory(boardIdx: Int, summary: BoardSummary, newBoardSequence: Int) extends Response
  case class ReportBoardAction(boardIdx: Int, boardAction: BoardAction, newBoardSequence: Int) extends Response
  case class ReportGameAction(gameAction: GameAction, newGameSequence: Int) extends Response
  case class ReportNewTurn(newSide: Side) extends Response
  case class ReportResetBoard(boardIdx: Int, necroNames:SideArray[PieceName], canMove: Boolean, reinforcements: SideArray[Map[PieceName, Int]]) extends Response
  case class ReportRevealSpells(spellsIdsAndNames: Array[(Int,SpellName)]) extends Response
  case class ReportTimeLeft(timeLeft: Double) extends Response
  case class ReportPause(isPaused:Boolean) extends Response

  sealed trait Query
  case class Heartbeat(i: Int) extends Query
  case class RequestBoardHistory(boardIdx: Int) extends Query
  case class DoBoardAction(boardIdx: Int, boardAction: BoardAction) extends Query
  case class DoGameAction(gameAction: GameAction) extends Query
  case class Chat(username: String, side: Option[Side], allChat: Boolean, message: String) extends Query
  case class RequestPause(isPaused:Boolean) extends Query

  //Conversions----------------------------------------------------
  def readsFromString[T](typeName: String)(f: String => T): Reads[T] = {
    new Reads[T]{
      def reads(json: JsValue): JsResult[T] = json match {
        case JsString(s) => JsSuccess(f(s))
        case _ => JsError("JSON string value expected when parsing " + typeName)
      }
    }
  }
  def writesToString[T](f: T => String): Writes[T] = {
    new Writes[T]{
      def writes(t: T): JsValue = JsString(f(t))
    }
  }

  def readsFromPair[T](typeName: String, convMap: Map[String, (JsValue => JsResult[T])]): Reads[T] = {
    new Reads[T]{
      def fail(): JsResult[T] =
        JsError("JSON (string*value) pair expected when parsing " + typeName)
      def reads(json: JsValue): JsResult[T] = json match {
        case JsArray(arr) =>
          if(arr.length != 2) fail()
          else {
            arr(0) match {
              case JsString(s) =>
                convMap.get(s) match {
                  case None => fail()
                  case Some(f) => f(arr(1))
                }
              case _ => fail()
            }
          }
        case _ => fail()
      }
    }
  }
  def jsPair(variantName: String, jsValue: JsValue): JsValue = {
    JsArray(Array(JsString(variantName),jsValue))
  }

  //CommonTypes.scala--------------------------------------------------------------------------------
  implicit val locFormat = Json.format[Loc]
  implicit val vecFormat = Json.format[Vec]

  implicit val sideFormat = {
    val reads: Reads[Side] = readsFromString[Side]("Side")(Side.ofString)
    val writes: Writes[Side] = writesToString[Side](side => side.toString)
    Format(reads,writes)
  }

  implicit val startHexFormat = Json.format[StartHex]
  implicit val spawnerFormat = Json.format[Spawner]
  implicit val terrainFormat = {
    val reads: Reads[Terrain] = readsFromPair[Terrain]("Terrain",Map(
      "Wall" -> ((_:JsValue) => JsSuccess(Wall: Terrain)),
      "Ground" -> ((_:JsValue) => JsSuccess(Ground: Terrain)),
      "Water" -> ((_:JsValue) => JsSuccess(Water: Terrain)),
      "Graveyard" -> ((_:JsValue) => JsSuccess(Graveyard: Terrain)),
      "SorceryNode" -> ((_:JsValue) => JsSuccess(SorceryNode: Terrain)),
      "Teleporter" -> ((_:JsValue) => JsSuccess(Teleporter: Terrain)),
      "StartHex" -> ((json:JsValue) => startHexFormat.reads(json)),
      "Spawner" -> ((json:JsValue) => spawnerFormat.reads(json)),
      "Mist" -> ((_:JsValue) => JsSuccess(Mist: Terrain)),
      "Earthquake" -> ((json:JsValue) => JsSuccess(Earthquake: Terrain)),
      "Firestorm" -> ((json:JsValue) => JsSuccess(Firestorm: Terrain)),
      "Flood" -> ((json:JsValue) => JsSuccess(Flood: Terrain)),
      "Whirlwind" -> ((json:JsValue) => JsSuccess(Whirlwind: Terrain)),
    ))
    val writes: Writes[Terrain] = new Writes[Terrain] {
      def writes(t: Terrain): JsValue = t match {
        case (Wall) => jsPair("Wall",JsString(""))
        case (Ground) => jsPair("Ground",JsString(""))
        case (Water) => jsPair("Water",JsString(""))
        case (Graveyard) => jsPair("Graveyard",JsString(""))
        case (SorceryNode) => jsPair("SorceryNode",JsString(""))
        case (Teleporter) => jsPair("Teleporter",JsString(""))
        case (t:StartHex) => jsPair("StartHex",startHexFormat.writes(t))
        case (t:Spawner) => jsPair("Spawner",spawnerFormat.writes(t))
        case (Mist) => jsPair("Mist",JsString(""))
        case (Earthquake) => jsPair("Earthquake", JsString(""))
        case (Firestorm) => jsPair("Firestorm", JsString(""))
        case (Flood) => jsPair("Flood", JsString(""))
        case (Whirlwind) => jsPair("Whirlwind", JsString(""))
      }
    }
    Format(reads,writes)
  }

  def mapJsResults[T,U:ClassTag](arr:IndexedSeq[T])(f: T => JsResult[U]): JsResult[Array[U]] = {
    val len = arr.length
    def loop(acc: List[U], idx: Int): JsResult[Array[U]] = {
      if(idx >= len)
        JsSuccess(acc.toArray.reverse)
      else {
        f(arr(idx)) match {
          case (e: JsError) => e
          case (s: JsSuccess[U]) => loop(s.get :: acc, idx+1)
        }
      }
    }
    loop(Nil,0)
  }

  implicit def sideArrayFormat[T:ClassTag](implicit format: Format[T]) : Format[SideArray[T]] = {
    new Format[SideArray[T]] {
      def fail(): JsResult[SideArray[T]] =
        JsError("JSON pair expected when parsing side array")
      def reads(json: JsValue): JsResult[SideArray[T]] = json match {
        case JsArray(arr) =>
          if(arr.length != 2) fail()
          else mapJsResults(arr)(format.reads).map{ (arr:Array[T]) => SideArray.ofArrayInplace(arr) }
        case _ => fail()
      }
      def writes(sa: SideArray[T]): JsValue = {
        val arr = sa.toArrayInplace
        JsArray(Array(format.writes(arr(0)),format.writes(arr(1))))
      }
    }
  }

  implicit def mapFormat[T:ClassTag,U:ClassTag](implicit formatKey: Format[T], formatValue: Format[U]): Format[Map[T,U]] = {
    new Format[Map[T,U]] {
      def reads(json: JsValue): JsResult[Map[T,U]] = {
        Json.fromJson[List[(T,U)]](json).map { list => list.toMap }
      }
      def writes(map: Map[T,U]): JsValue = {
        Json.toJson(map.toList)
      }
    }
  }

  implicit val planeTopologyFormat = {
    val reads: Reads[PlaneTopology] = readsFromString[PlaneTopology]("PlaneTopology")(PlaneTopology.ofString)
    val writes: Writes[PlaneTopology] = writesToString[PlaneTopology](planeTopology => planeTopology.toString)
    Format(reads,writes)
  }

  implicit def planeFormat[T:ClassTag](implicit format: Format[T]) : Format[Plane[T]] = {
    import play.api.libs.json.Reads._
    import play.api.libs.functional.syntax._
    def getValue(obj:scala.collection.Map[String,JsValue], str: String): JsResult[JsValue] = {
      obj.get(str) match {
        case None => JsError("When parsing plane failed to find field: " + str)
        case Some(x) => JsSuccess(x)
      }
    }
    def getArray(obj:scala.collection.Map[String,JsValue], str: String): JsResult[IndexedSeq[JsValue]] = {
      obj.get(str) match {
        case None => JsError("When parsing plane failed to find field: " + str)
        case Some(JsArray(x)) => JsSuccess(x)
        case Some(_) => JsError("When parsing plane field " + str + " was not an array")
      }
    }
    def getInt(obj:scala.collection.Map[String,JsValue], str: String): JsResult[Int] = {
      obj.get(str) match {
        case None => JsError("When parsing plane failed to find field: " + str)
        case Some(JsNumber(x)) => JsSuccess(x.intValue)
        case Some(_) => JsError("When parsing plane field " + str + " was not an integer")
      }
    }
    new Format[Plane[T]] {
      def reads(json: JsValue): JsResult[Plane[T]] = json match {
        case JsObject(obj) =>
          getInt(obj,"xSize").flatMap { xSize =>
            getInt(obj,"ySize").flatMap { ySize =>
              getValue(obj,"topology").flatMap { topology =>
                planeTopologyFormat.reads(topology).flatMap { topology =>
                  getArray(obj,"arr").flatMap { arr =>
                    mapJsResults(arr)(format.reads).map { (arr:Array[T]) =>
                      new Plane(xSize,ySize,topology,arr)
                    }
                  }
                }
              }
            }
          }
        case _ => JsError("JSON object expected when parsing plane")
      }
      def writes(plane: Plane[T]): JsValue = {
        JsObject(Map(
          "xSize" -> JsNumber(plane.xSize),
          "ySize" -> JsNumber(plane.ySize),
          "topology" -> planeTopologyFormat.writes(plane.topology),
          "arr" -> JsArray(plane.getArrayInplace.map(format.writes))
        ))
      }
    }
  }

  //BoardState.scala--------------------------------------------------------------------------------
  implicit val startedTurnWithIDFormat = Json.format[StartedTurnWithID]
  implicit val spawnedThisTurnFormat = Json.format[SpawnedThisTurn]
  implicit val pieceSpecFormat = {
    val reads: Reads[PieceSpec] = readsFromPair[PieceSpec]("PieceSpec",Map(
      "StartedTurnWithID" -> ((json:JsValue) => startedTurnWithIDFormat.reads(json)),
      "SpawnedThisTurn" -> ((json:JsValue) => spawnedThisTurnFormat.reads(json))
    ))
    val writes: Writes[PieceSpec] = new Writes[PieceSpec] {
      def writes(t: PieceSpec): JsValue = t match {
        case (t:StartedTurnWithID) => jsPair("StartedTurnWithID",startedTurnWithIDFormat.writes(t))
        case (t:SpawnedThisTurn) => jsPair("SpawnedThisTurn",spawnedThisTurnFormat.writes(t))
      }
    }
    Format(reads,writes)
  }

  implicit val movementFormat = Json.format[Movement]
  implicit val movementsFormat = Json.format[Movements]
  implicit val attackFormat = Json.format[Attack]
  implicit val spawnFormat = Json.format[Spawn]
  implicit val spellOrAbilityTargetsFormat = Json.format[SpellOrAbilityTargets]
  implicit val spellPlayedInfoFormat = Json.format[SpellPlayedInfo]
  implicit val activateTileFormat = Json.format[ActivateTile]
  implicit val activateAbilityFormat = Json.format[ActivateAbility]
  implicit val blinkFormat = Json.format[Blink]
  implicit val teleportFormat = Json.format[Teleport]
  implicit val playSpellFormat = Json.format[PlaySpell]
  implicit val discardSpellFormat = Json.format[DiscardSpell]
  implicit val playerActionFormat = {
    val reads: Reads[PlayerAction] = readsFromPair[PlayerAction]("PlayerAction",Map(
      "Movements" -> ((json:JsValue) => movementsFormat.reads(json)),
      "Attack" -> ((json:JsValue) => attackFormat.reads(json)),
      "Spawn" -> ((json:JsValue) => spawnFormat.reads(json)),
      "ActivateTile" -> ((json:JsValue) => activateTileFormat.reads(json)),
      "ActivateAbility" -> ((json:JsValue) => activateAbilityFormat.reads(json)),
      "Blink" -> ((json:JsValue) => blinkFormat.reads(json)),
      "Teleport" -> ((json:JsValue) => teleportFormat.reads(json)),
      "PlaySpell" -> ((json:JsValue) => playSpellFormat.reads(json)),
      "DiscardSpell" -> ((json:JsValue) => discardSpellFormat.reads(json))
    ))
    val writes: Writes[PlayerAction] = new Writes[PlayerAction] {
      def writes(t: PlayerAction): JsValue = t match {
        case (t:Movements) => jsPair("Movements",movementsFormat.writes(t))
        case (t:Attack) => jsPair("Attack",attackFormat.writes(t))
        case (t:Spawn) => jsPair("Spawn",spawnFormat.writes(t))
        case (t:ActivateTile) => jsPair("ActivateTile",activateTileFormat.writes(t))
        case (t:ActivateAbility) => jsPair("ActivateAbility",activateAbilityFormat.writes(t))
        case (t:Blink) => jsPair("Blink",blinkFormat.writes(t))
        case (t:Teleport) => jsPair("Teleport",teleportFormat.writes(t))
        case (t:PlaySpell) => jsPair("PlaySpell",playSpellFormat.writes(t))
        case (t:DiscardSpell) => jsPair("DiscardSpell",discardSpellFormat.writes(t))
      }
    }
    Format(reads,writes)
  }

  implicit val buyReinforcementFormat = Json.format[BuyReinforcement]
  implicit val gainSpellFormat = Json.format[GainSpell]
  implicit val generalBoardActionFormat = {
    val reads: Reads[GeneralBoardAction] = readsFromPair[GeneralBoardAction]("GeneralBoardAction",Map(
      "BuyReinforcement" -> ((json:JsValue) => buyReinforcementFormat.reads(json)),
      "GainSpell" -> ((json:JsValue) => gainSpellFormat.reads(json)),
    ))
    val writes: Writes[GeneralBoardAction] = new Writes[GeneralBoardAction] {
      def writes(t: GeneralBoardAction): JsValue = t match {
        case (t:BuyReinforcement) => jsPair("BuyReinforcement",buyReinforcementFormat.writes(t))
        case (t:GainSpell) => jsPair("GainSpell",gainSpellFormat.writes(t))
      }
    }
    Format(reads,writes)
  }

  implicit val pieceModFormat = {
    val reads: Reads[PieceMod] = readsFromString[PieceMod]("PieceMod")(PieceMod.ofString)
    val writes: Writes[PieceMod] = writesToString[PieceMod](pieceMod => pieceMod.toString)
    Format(reads,writes)
  }

  implicit val pieceStatsFormat = {
    new Format[PieceStats] {
      def reads(json: JsValue): JsResult[PieceStats] = json match {
        case JsString(s) =>
          Units.pieceMap.get(s) match {
            case None => JsError("Unknown piece name: " + s)
            case Some(stats) => JsSuccess(stats)
          }
        case _ =>
          JsError("JSON string expected when parsing piece stats")
      }
      def writes(t: PieceStats): JsValue = {
        assert(t.isBaseStats)
        JsString(t.name)
      }
    }
  }

  implicit val movingFormat = Json.format[Moving]
  implicit val attackingFormat = Json.format[Attacking]
  implicit val actStateFormat = {
    val reads: Reads[ActState] = readsFromPair[ActState]("ActState",Map(
      "Moving" -> ((json:JsValue) => movingFormat.reads(json)),
      "Attacking" -> ((json:JsValue) => attackingFormat.reads(json)),
      "Spawning" -> ((_:JsValue) => JsSuccess(Spawning: ActState)),
      "DoneActing" -> ((_:JsValue) => JsSuccess(DoneActing: ActState)),
    ))
    val writes: Writes[ActState] = new Writes[ActState] {
      def writes(t: ActState): JsValue = t match {
        case (t:Moving) => jsPair("Moving",movingFormat.writes(t))
        case (t:Attacking) => jsPair("Attacking",attackingFormat.writes(t))
        case (Spawning) => jsPair("Spawning",JsString(""))
        case (DoneActing) => jsPair("DoneActing",JsString(""))
      }
    }
    Format(reads,writes)
  }

  implicit val pieceModWithDurationFormat = Json.format[PieceModWithDuration]
  implicit val pieceFormat = Json.format[Piece]

  implicit val tileFormat = new Format[Tile] {
    def fail(): JsResult[Tile] =
      JsError("Could not parse JSON for tile")
    def reads(json: JsValue): JsResult[Tile] = json match {
      case JsArray(arr) =>
        if(arr.length != 2) fail()
        else {
          terrainFormat.reads(arr(0)).flatMap { terrain =>
            arr(1).validate[List[PieceModWithDuration]].map { modsWithDuration =>
              Tile(terrain,modsWithDuration)
            }
          }
        }
      case _ => fail()
    }

    def writes(t: Tile): JsValue = {
      JsArray(Array(Json.toJson(t.terrain),Json.toJson(t.modsWithDuration)))
    }
  }
  implicit val boardStateFormat = Json.format[BoardState]

  //Board.scala--------------------------------------------------------------------------------
  implicit val playerActionsFormat = Json.format[PlayerActions]
  implicit val doGeneralBoardActionFormat = Json.format[DoGeneralBoardAction]
  implicit val localPieceUndoFormat = Json.format[LocalPieceUndo]
  implicit val spellUndoFormat = Json.format[SpellUndo]
  implicit val buyReinforcementUndoFormat = Json.format[BuyReinforcementUndo]
  implicit val gainSpellUndoFormat = Json.format[GainSpellUndo]
  implicit val redoFormat = Json.format[Redo]
  implicit val boardActionFormat = {
    val reads: Reads[BoardAction] = readsFromPair[BoardAction]("BoardAction",Map(
      "PlayerActions" -> ((json:JsValue) => playerActionsFormat.reads(json)),
      "DoGeneralBoardAction" -> ((json:JsValue) => doGeneralBoardActionFormat.reads(json)),
      "Redo" -> ((json:JsValue) => redoFormat.reads(json)),
      "LocalPieceUndo" -> ((json:JsValue) => localPieceUndoFormat.reads(json)),
      "SpellUndo" -> ((json:JsValue) => spellUndoFormat.reads(json)),
      "BuyReinforcementUndo" -> ((json:JsValue) => buyReinforcementUndoFormat.reads(json)),
      "GainSpellUndo" -> ((json:JsValue) => gainSpellUndoFormat.reads(json)),
    ))
    val writes: Writes[BoardAction] = new Writes[BoardAction] {
      def writes(t: BoardAction): JsValue = t match {
        case (t:PlayerActions) => jsPair("PlayerActions",playerActionsFormat.writes(t))
        case (t:DoGeneralBoardAction) => jsPair("DoGeneralBoardAction",doGeneralBoardActionFormat.writes(t))
        case (t:Redo) => jsPair("Redo",redoFormat.writes(t))
        case (t:LocalPieceUndo) => jsPair("LocalPieceUndo",localPieceUndoFormat.writes(t))
        case (t:SpellUndo) => jsPair("SpellUndo",spellUndoFormat.writes(t))
        case (t:BuyReinforcementUndo) => jsPair("BuyReinforcementUndo",buyReinforcementUndoFormat.writes(t))
        case (t:GainSpellUndo) => jsPair("GainSpellUndo",gainSpellUndoFormat.writes(t))
      }
    }
    Format(reads,writes)
  }
  implicit val boardSummaryFormat = Json.format[BoardSummary]
  implicit val boardHistoryFormat = Json.format[BoardHistory]
  implicit val boardFormat = Json.format[Board]

  //Game.scala--------------------------------------------------------------------------------
  implicit val techLevelFormat = {
    val reads: Reads[TechLevel] = readsFromString[TechLevel]("TechLevel")(TechLevel.ofString)
    val writes: Writes[TechLevel] = writesToString[TechLevel](techLevel => techLevel.toString)
    Format(reads,writes)
  }

  implicit val pieceTechFormat = Json.format[PieceTech]
  implicit val techFormat = {
    val reads: Reads[Tech] = readsFromPair[Tech]("Tech",Map(
      "PieceTech" -> ((json:JsValue) => pieceTechFormat.reads(json)),
    ))
    val writes: Writes[Tech] = new Writes[Tech] {
      def writes(t: Tech): JsValue = t match {
        case (t:PieceTech) => jsPair("PieceTech",pieceTechFormat.writes(t))
      }
    }
    Format(reads,writes)
  }

  implicit val techStateFormat = Json.format[TechState]

  implicit val payForReinforcementFormat = Json.format[PayForReinforcement]
  implicit val unpayForReinforcementFormat = Json.format[UnpayForReinforcement]
  implicit val chooseSpellFormat = Json.format[ChooseSpell]
  implicit val unchooseSpellFormat = Json.format[UnchooseSpell]
  implicit val buyExtraTechAndSpellFormat = Json.format[BuyExtraTechAndSpell]
  implicit val unbuyExtraTechAndSpellFormat = Json.format[UnbuyExtraTechAndSpell]
  implicit val addWinFormat = Json.format[AddWin]
  implicit val addUpcomingSpellsFormat = Json.format[AddUpcomingSpells]
  implicit val performTechFormat = Json.format[PerformTech]
  implicit val undoTechFormat = Json.format[UndoTech]
  implicit val setBoardDoneFormat = Json.format[SetBoardDone]
  implicit val resignBoardFormat = Json.format[ResignBoard]
  implicit val gameActionFormat = {
    val reads: Reads[GameAction] = readsFromPair[GameAction]("GameAction",Map(
      "PayForReinforcement" -> ((json:JsValue) => payForReinforcementFormat.reads(json)),
      "UnpayForReinforcement" -> ((json:JsValue) => unpayForReinforcementFormat.reads(json)),
      "ChooseSpell" -> ((json:JsValue) => chooseSpellFormat.reads(json)),
      "UnchooseSpell" -> ((json:JsValue) => unchooseSpellFormat.reads(json)),
      "BuyExtraTechAndSpell" -> ((json:JsValue) => buyExtraTechAndSpellFormat.reads(json)),
      "UnbuyExtraTechAndSpell" -> ((json:JsValue) => unbuyExtraTechAndSpellFormat.reads(json)),
      "AddWin" -> ((json:JsValue) => addWinFormat.reads(json)),
      "AddUpcomingSpells" -> ((json:JsValue) => addUpcomingSpellsFormat.reads(json)),
      "PerformTech" -> ((json:JsValue) => performTechFormat.reads(json)),
      "UndoTech" -> ((json:JsValue) => undoTechFormat.reads(json)),
      "SetBoardDone" -> ((json:JsValue) => setBoardDoneFormat.reads(json)),
      "ResignBoard" -> ((json:JsValue) => resignBoardFormat.reads(json)),
    ))
    val writes: Writes[GameAction] = new Writes[GameAction] {
      def writes(t: GameAction): JsValue = t match {
        case (t:PayForReinforcement) => jsPair("PayForReinforcement",payForReinforcementFormat.writes(t))
        case (t:UnpayForReinforcement) => jsPair("UnpayForReinforcement",unpayForReinforcementFormat.writes(t))
        case (t:ChooseSpell) => jsPair("ChooseSpell",chooseSpellFormat.writes(t))
        case (t:UnchooseSpell) => jsPair("UnchooseSpell",unchooseSpellFormat.writes(t))
        case (t:BuyExtraTechAndSpell) => jsPair("BuyExtraTechAndSpell",buyExtraTechAndSpellFormat.writes(t))
        case (t:UnbuyExtraTechAndSpell) => jsPair("UnbuyExtraTechAndSpell",unbuyExtraTechAndSpellFormat.writes(t))
        case (t:AddWin) => jsPair("AddWin",addWinFormat.writes(t))
        case (t:AddUpcomingSpells) => jsPair("AddUpcomingSpells",addUpcomingSpellsFormat.writes(t))
        case (t:PerformTech) => jsPair("PerformTech",performTechFormat.writes(t))
        case (t:UndoTech) => jsPair("UndoTech",undoTechFormat.writes(t))
        case (t:SetBoardDone) => jsPair("SetBoardDone",setBoardDoneFormat.writes(t))
        case (t:ResignBoard) => jsPair("ResignBoard",resignBoardFormat.writes(t))
      }
    }
    Format(reads,writes)
  }

  implicit val gameFormat = Json.format[Game]

  //ExternalInfo.scala
  implicit val externalInfoFormat = Json.format[ExternalInfo]

  //Protocol.scala--------------------------------------------------------------------------------
  implicit val versionFormat = Json.format[Version]
  implicit val queryErrorFormat = Json.format[QueryError]
  implicit val clientHeartbeatRateFormat = Json.format[ClientHeartbeatRate]
  implicit val okHeartbeatFormat = Json.format[OkHeartbeat]
  implicit val messagesFormat = Json.format[Messages]
  implicit val playersFormat = Json.format[Players]
  implicit val initializeFormat = Json.format[Initialize]
  implicit val userJoinedFormat = Json.format[UserJoined]
  implicit val userLeftFormat = Json.format[UserLeft]
  implicit val okBoardActionFormat = Json.format[OkBoardAction]
  implicit val okGameActionFormat = Json.format[OkGameAction]
  implicit val reportBoardHistoryFormat = Json.format[ReportBoardHistory]
  implicit val reportBoardActionFormat = Json.format[ReportBoardAction]
  implicit val reportGameActionFormat = Json.format[ReportGameAction]
  implicit val reportNewTurnFormat = Json.format[ReportNewTurn]
  implicit val reportResetBoardFormat = Json.format[ReportResetBoard]
  implicit val reportRevealSpellsFormat = Json.format[ReportRevealSpells]
  implicit val reportTimeLeftFormat = Json.format[ReportTimeLeft]
  implicit val reportPauseFormat = Json.format[ReportPause]
  implicit val responseFormat = {
    val reads: Reads[Response] = readsFromPair[Response]("Response",Map(
      "Version" -> ((json:JsValue) => versionFormat.reads(json)),
      "QueryError" -> ((json:JsValue) => queryErrorFormat.reads(json)),
      "ClientHeartbeatRate" -> ((json:JsValue) => clientHeartbeatRateFormat.reads(json)),
      "OkHeartbeat" -> ((json:JsValue) => okHeartbeatFormat.reads(json)),
      "Messages" -> ((json:JsValue) => messagesFormat.reads(json)),
      "Players" -> ((json:JsValue) => playersFormat.reads(json)),
      "Initialize" -> ((json:JsValue) => initializeFormat.reads(json)),
      "UserJoined" -> ((json:JsValue) => userJoinedFormat.reads(json)),
      "UserLeft" -> ((json:JsValue) => userLeftFormat.reads(json)),
      "OkBoardAction" -> ((json:JsValue) => okBoardActionFormat.reads(json)),
      "OkGameAction" -> ((json:JsValue) => okGameActionFormat.reads(json)),
      "ReportBoardHistory" -> ((json:JsValue) => reportBoardHistoryFormat.reads(json)),
      "ReportBoardAction" -> ((json:JsValue) => reportBoardActionFormat.reads(json)),
      "ReportGameAction" -> ((json:JsValue) => reportGameActionFormat.reads(json)),
      "ReportNewTurn" -> ((json:JsValue) => reportNewTurnFormat.reads(json)),
      "ReportResetBoard" -> ((json:JsValue) => reportResetBoardFormat.reads(json)),
      "ReportRevealSpells" -> ((json:JsValue) => reportRevealSpellsFormat.reads(json)),
      "ReportTimeLeft" -> ((json:JsValue) => reportTimeLeftFormat.reads(json)),
      "ReportPause" -> ((json:JsValue) => reportPauseFormat.reads(json))
    ))
    val writes: Writes[Response] = new Writes[Response] {
      def writes(t: Response): JsValue = t match {
        case (t:Version) => jsPair("Version",versionFormat.writes(t))
        case (t:QueryError) => jsPair("QueryError",queryErrorFormat.writes(t))
        case (t:ClientHeartbeatRate) => jsPair("ClientHeartbeatRate",clientHeartbeatRateFormat.writes(t))
        case (t:OkHeartbeat) => jsPair("OkHeartbeat",okHeartbeatFormat.writes(t))
        case (t:Messages) => jsPair("Messages",messagesFormat.writes(t))
        case (t:Players) => jsPair("Players",playersFormat.writes(t))
        case (t:Initialize) => jsPair("Initialize",initializeFormat.writes(t))
        case (t:UserJoined) => jsPair("UserJoined",userJoinedFormat.writes(t))
        case (t:UserLeft) => jsPair("UserLeft",userLeftFormat.writes(t))
        case (t:OkBoardAction) => jsPair("OkBoardAction",okBoardActionFormat.writes(t))
        case (t:OkGameAction) => jsPair("OkGameAction",okGameActionFormat.writes(t))
        case (t:ReportBoardHistory) => jsPair("ReportBoardHistory",reportBoardHistoryFormat.writes(t))
        case (t:ReportBoardAction) => jsPair("ReportBoardAction",reportBoardActionFormat.writes(t))
        case (t:ReportGameAction) => jsPair("ReportGameAction",reportGameActionFormat.writes(t))
        case (t:ReportNewTurn) => jsPair("ReportNewTurn",reportNewTurnFormat.writes(t))
        case (t:ReportResetBoard) => jsPair("ReportResetBoard",reportResetBoardFormat.writes(t))
        case (t:ReportRevealSpells) => jsPair("ReportRevealSpells",reportRevealSpellsFormat.writes(t))
        case (t:ReportTimeLeft) => jsPair("ReportTimeLeft",reportTimeLeftFormat.writes(t))
        case (t:ReportPause) => jsPair("ReportPause",reportPauseFormat.writes(t))
      }
    }
    Format(reads,writes)
  }

  implicit val heartbeatFormat = Json.format[Heartbeat]
  implicit val requestBoardHistoryFormat = Json.format[RequestBoardHistory]
  implicit val doBoardActionFormat = Json.format[DoBoardAction]
  implicit val doGameActionFormat = Json.format[DoGameAction]
  implicit val chatFormat = Json.format[Chat]
  implicit val requestPauseFormat = Json.format[RequestPause]
  implicit val queryFormat = {
    val reads: Reads[Query] = readsFromPair[Query]("Query",Map(
      "Heartbeat" -> ((json:JsValue) => heartbeatFormat.reads(json)),
      "RequestBoardHistory" -> ((json:JsValue) => requestBoardHistoryFormat.reads(json)),
      "DoBoardAction" -> ((json:JsValue) => doBoardActionFormat.reads(json)),
      "DoGameAction" -> ((json:JsValue) => doGameActionFormat.reads(json)),
      "Chat" -> ((json:JsValue) => chatFormat.reads(json)),
      "RequestPause" -> ((json:JsValue) => requestPauseFormat.reads(json))
    ))
    val writes: Writes[Query] = new Writes[Query] {
      def writes(t: Query): JsValue = t match {
        case (t:Heartbeat) => jsPair("Heartbeat",heartbeatFormat.writes(t))
        case (t:RequestBoardHistory) => jsPair("RequestBoardHistory",requestBoardHistoryFormat.writes(t))
        case (t:DoBoardAction) => jsPair("DoBoardAction",doBoardActionFormat.writes(t))
        case (t:DoGameAction) => jsPair("DoGameAction",doGameActionFormat.writes(t))
        case (t:Chat) => jsPair("Chat", chatFormat.writes(t))
        case (t:RequestPause) => jsPair("RequestPause", requestPauseFormat.writes(t))
      }
    }
    Format(reads,writes)
  }
}

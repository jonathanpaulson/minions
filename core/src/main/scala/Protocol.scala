package minionsgame.core
import play.api.libs.json._

object Protocol {
  sealed trait Message
  case class Version(version: String) extends Message
  case class BoardAction(action: Action) extends Message

  sealed trait Query
  case object RequestFullState extends Query


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

  //CommonTypes.scala
  implicit val locFormat = Json.format[Loc]
  implicit val vecFormat = Json.format[Vec]

  implicit val sideFormat = {
    val reads: Reads[Side] = readsFromString[Side]("Side")(Side.ofString)
    val writes: Writes[Side] = writesToString[Side](side => side.toString)
    Format(reads,writes)
  }

  //BoardState.scala
  implicit val startedTurnWithIDFormat = Json.format[StartedTurnWithID]
  implicit val spawnedThisTurnFormat = Json.format[SpawnedThisTurn]
  implicit val pieceSpecFormat = {
    val reads: Reads[PieceSpec] = readsFromPair[PieceSpec]("PieceSpec",Map(
      "StartedTurnWithID" -> ((json:JsValue) => startedTurnWithIDFormat.reads(json)),
      "SpawnedThisTurn" -> ((json:JsValue) => spawnedThisTurnFormat.reads(json))
    ))
    val writes: Writes[PieceSpec] = new Writes[PieceSpec] {
      def writes(t: PieceSpec): JsValue = t match {
        case (t:StartedTurnWithID) => startedTurnWithIDFormat.writes(t)
        case (t:SpawnedThisTurn) => spawnedThisTurnFormat.writes(t)
      }
    }
    Format(reads,writes)
  }

  implicit val movementFormat = Json.format[Movement]
  implicit val movementsFormat = Json.format[Movements]
  implicit val attackFormat = Json.format[Attack]
  implicit val spawnFormat = Json.format[Spawn]
  implicit val playedSpellOrAbilityFormat = Json.format[PlayedSpellOrAbility]
  implicit val spellsAndAbilitiesFormat = Json.format[SpellsAndAbilities]
  implicit val playerActionFormat = {
    val reads: Reads[PlayerAction] = readsFromPair[PlayerAction]("PlayerAction",Map(
      "Movements" -> ((json:JsValue) => movementsFormat.reads(json)),
      "Attack" -> ((json:JsValue) => attackFormat.reads(json)),
      "Spawn" -> ((json:JsValue) => spawnFormat.reads(json)),
      "SpellsAndAbilities" -> ((json:JsValue) => spellsAndAbilitiesFormat.reads(json))
    ))
    val writes: Writes[PlayerAction] = new Writes[PlayerAction] {
      def writes(t: PlayerAction): JsValue = t match {
        case (t:Movements) => movementsFormat.writes(t)
        case (t:Attack) => attackFormat.writes(t)
        case (t:Spawn) => spawnFormat.writes(t)
        case (t:SpellsAndAbilities) => spellsAndAbilitiesFormat.writes(t)
      }
    }
    Format(reads,writes)
  }

  implicit val buyReinforcementFormat = Json.format[BuyReinforcement]
  implicit val gainSpellFormat = Json.format[GainSpell]
  implicit val revealSpellFormat = Json.format[RevealSpell]
  implicit val generalActionFormat = {
    val reads: Reads[GeneralAction] = readsFromPair[GeneralAction]("GeneralAction",Map(
      "BuyReinforcement" -> ((json:JsValue) => buyReinforcementFormat.reads(json)),
      "GainSpell" -> ((json:JsValue) => gainSpellFormat.reads(json)),
      "RevealSpell" -> ((json:JsValue) => revealSpellFormat.reads(json))
    ))
    val writes: Writes[GeneralAction] = new Writes[GeneralAction] {
      def writes(t: GeneralAction): JsValue = t match {
        case (t:BuyReinforcement) => buyReinforcementFormat.writes(t)
        case (t:GainSpell) => gainSpellFormat.writes(t)
        case (t:RevealSpell) => revealSpellFormat.writes(t)
      }
    }
    Format(reads,writes)
  }

  //Board.scala
  implicit val playerActionsFormat = Json.format[PlayerActions]
  implicit val doGeneralActionFormat = Json.format[DoGeneralAction]
  implicit val localUndoFormat = Json.format[LocalUndo]
  implicit val actionFormat = {
    val reads: Reads[Action] = readsFromPair[Action]("Action",Map(
      "PlayerActions" -> ((json:JsValue) => playerActionsFormat.reads(json)),
      "DoGeneralAction" -> ((json:JsValue) => doGeneralActionFormat.reads(json)),
      "LocalUndo" -> ((json:JsValue) => localUndoFormat.reads(json))
    ))
    val writes: Writes[Action] = new Writes[Action] {
      def writes(t: Action): JsValue = t match {
        case (t:PlayerActions) => playerActionsFormat.writes(t)
        case (t:DoGeneralAction) => doGeneralActionFormat.writes(t)
        case (t:LocalUndo) => localUndoFormat.writes(t)
      }
    }
    Format(reads,writes)
  }

  //Protocol.scala
  implicit val versionFormat = Json.format[Version]
  implicit val boardActionFormat = Json.format[BoardAction]
  implicit val messageFormat = {
    val reads: Reads[Message] = readsFromPair[Message]("Message",Map(
      "Version" -> ((json:JsValue) => versionFormat.reads(json)),
      "BoardAction" -> ((json:JsValue) => boardActionFormat.reads(json)),
    ))
    val writes: Writes[Message] = new Writes[Message] {
      def writes(t: Message): JsValue = t match {
        case (t:Version) => versionFormat.writes(t)
        case (t:BoardAction) => boardActionFormat.writes(t)
      }
    }
    Format(reads,writes)
  }

}

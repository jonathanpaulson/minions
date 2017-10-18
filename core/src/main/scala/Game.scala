package minionsgame.core

import scala.util.{Try,Success,Failure}
import RichImplicits._

/** TechLevel:
  * How far long the tech tree a side is for a given unit
  */
sealed trait TechLevel {
  override def toString() : String = this match {
    case TechLocked => "Locked"
    case TechUnlocked => "Unlocked"
    case TechAcquired => "Acquired"
  }
  def toUnicodeSymbol() : String = this match {
    case TechLocked => "0"
    case TechUnlocked => "1"
    case TechAcquired => "\u2605" //Unicode star
  }
}
object TechLevel {
  def ofString(s:String): TechLevel = {
    s match {
      case "TechLocked" => TechLocked
      case "TechUnlocked" => TechUnlocked
      case "TechAcquired" => TechAcquired
      case "Locked" => TechLocked
      case "Unlocked" => TechUnlocked
      case "Acquired" => TechAcquired
      case _ => throw new Exception("Could not parse TechLevel: " + s)
    }
  }
}
case object TechLocked extends TechLevel
case object TechUnlocked extends TechLevel
case object TechAcquired extends TechLevel


/** Tech:
  * Element in the tech sequence.
  */
sealed trait Tech {
  def shortDisplayName: String = {
    this match {
      case PieceTech(pieceName) => Units.pieceMap(pieceName).shortDisplayName
    }
  }
  def displayName: String = {
    this match {
      case PieceTech(pieceName) => Units.pieceMap(pieceName).displayName
    }
  }
  def pieceStats : PieceStats = {
    this match {
      case PieceTech(pieceName) => Units.pieceMap(pieceName)
    }
  }
}
case class PieceTech(pieceName:PieceName) extends Tech

/** TechState:
  * State of a single tech.
  */
case class TechState(
  val shortDisplayName: String,
  val displayName: String,
  val tech: Tech,
  val techNumber: Option[Int],
  val level: SideArray[TechLevel],
  val startingLevelThisTurn: SideArray[TechLevel]
)

sealed trait GameAction
case class PerformTech(side: Side, techLineIdx: Int) extends GameAction
case class UndoTech(side: Side, techLineIdx: Int) extends GameAction
case class SetBoardDone(boardIdx: Int, done: Boolean) extends GameAction
case class ResignBoard(boardIdx: Int) extends GameAction
//server->client only
case class PayForReinforcement(side: Side, pieceName: PieceName) extends GameAction
case class UnpayForReinforcement(side: Side, pieceName: PieceName) extends GameAction
case class ChooseSpell(side: Side, spellId: Int) extends GameAction
case class UnchooseSpell(side: Side, spellId: Int) extends GameAction
case class AddWin(side: Side, boardIdx: Int) extends GameAction
case class AddUpcomingSpell(side: Side, spellId: Int) extends GameAction

case object Game {
  def apply(
    numBoards: Int,
    targetNumWins: Int,
    startingSide: Side,
    startingMana: SideArray[Int],
    extraTechCost: Int,
    extraManaPerTurn: Int,
    techsAlwaysAcquired: Array[Tech],
    lockedTechs: Array[(Tech,Int)]
  ) = {
    val techStatesAlwaysAcquired = techsAlwaysAcquired.map { tech =>
      TechState(
        shortDisplayName = tech.shortDisplayName,
        displayName = tech.displayName,
        tech = tech,
        techNumber = None,
        level = SideArray.ofArrayInplace(Array(TechAcquired,TechAcquired)),
        startingLevelThisTurn = SideArray.ofArrayInplace(Array(TechAcquired,TechAcquired))
      )
    }
    val techStatesLocked = lockedTechs.map { case (tech,techNumber) =>
      TechState(
        shortDisplayName = tech.shortDisplayName,
        displayName = tech.displayName,
        tech = tech,
        techNumber = Some(techNumber),
        level = SideArray.ofArrayInplace(Array(TechLocked,TechLocked)),
        startingLevelThisTurn = SideArray.ofArrayInplace(Array(TechLocked,TechLocked))
      )
    }
    val piecesAlwaysAcquired: Map[PieceName,TechState] =
      techStatesAlwaysAcquired.map { techState =>
        techState.tech match { case PieceTech(pieceName) => (pieceName -> techState) }
      }.toMap

    val game = new Game(
      numBoards = numBoards,
      curSide = startingSide,
      turnNumber = 0,
      winner = None,
      mana = startingMana.copy(),
      wins = SideArray.create(0),
      techLine = techStatesAlwaysAcquired ++ techStatesLocked,
      piecesAcquired = SideArray.create(piecesAlwaysAcquired),
      numTechsThisTurn = 0,
      spellsToChoose = Vector(),
      spellsChosen = Set(),
      upcomingSpells = SideArray.create(Vector()),
      targetNumWins = targetNumWins,
      extraTechCost = extraTechCost,
      extraManaPerTurn = extraManaPerTurn,
      isBoardDone = Array.fill(numBoards)(false),
      newTechsThisTurn = Vector()
    )
    game
  }
}

/** Game:
  * The "global" state which isn't local to a board.
  */
case class Game (
  val numBoards: Int,
  var curSide: Side,
  var turnNumber: Int,
  var winner: Option[Side],

  val mana: SideArray[Int],
  val wins: SideArray[Int],

  val techLine: Array[TechState],
  val piecesAcquired: SideArray[Map[PieceName,TechState]],
  var numTechsThisTurn: Int,

  var spellsToChoose: Vector[Int],
  var spellsChosen: Set[Int],
  var upcomingSpells: SideArray[Vector[Int]],

  val targetNumWins: Int,
  val extraTechCost: Int,
  val extraManaPerTurn: Int,

  //Flags set when user indicates that the board is done. Server ends the turn when all boards have this set.
  val isBoardDone: Array[Boolean],

  //Just for display purposes
  var newTechsThisTurn: Vector[(Side,Tech)]
) {

  def addMana(side: Side, amount: Int): Unit = {
    mana(side) = mana(side) + amount
  }

  def tryIsLegal(action: GameAction): Try[Unit] = {
    action match {
      case PayForReinforcement(side,pieceName) => tryCanPayForReinforcement(side,pieceName)
      case UnpayForReinforcement(side,pieceName) => tryCanUnpayForReinforcement(side,pieceName)
      case ChooseSpell(side,spellId) => tryCanChooseSpell(side,spellId)
      case UnchooseSpell(side,spellId) => tryCanUnchooseSpell(side,spellId)
      case AddWin(_,_) => Success(())
      case AddUpcomingSpell(_,_) => Success(())
      case PerformTech(side,techLineIdx) => tryCanPerformTech(side,techLineIdx)
      case UndoTech(side,techLineIdx) => tryCanUndoTech(side,techLineIdx)
      case SetBoardDone(boardIdx,done) => tryCanSetBoardDone(boardIdx,done)
      case ResignBoard(boardIdx) => tryCanResignBoard(boardIdx)
    }
  }

  def doAction(action: GameAction): Try[Unit] = {
    action match {
      case PayForReinforcement(side,pieceName) => payForReinforcement(side,pieceName)
      case UnpayForReinforcement(side,pieceName) => unpayForReinforcement(side,pieceName)
      case ChooseSpell(side,spellId) => chooseSpell(side,spellId)
      case UnchooseSpell(side,spellId) => unchooseSpell(side,spellId)
      case AddWin(side,_) => { doAddWin(side); Success(()) }
      case AddUpcomingSpell(side,spellId) => { doAddUpcomingSpell(side,spellId); Success(()) }
      case PerformTech(side,techLineIdx) => performTech(side,techLineIdx)
      case UndoTech(side,techLineIdx) => undoTech(side,techLineIdx)
      case SetBoardDone(boardIdx,done) => setBoardDone(boardIdx,done)
      case ResignBoard(boardIdx) => resignBoard(boardIdx)
    }
  }

  private def clearAndDrawSpells(): Unit = {
    spellsToChoose = Vector()
    spellsChosen = Set()
    while(upcomingSpells(curSide).nonEmpty && spellsToChoose.length < numBoards+1) {
      val spellId = upcomingSpells(curSide)(0)
      upcomingSpells(curSide) = upcomingSpells(curSide).drop(1)
      spellsToChoose = spellsToChoose :+ spellId
    }
  }


  //A hook for things that also need to happen at the start of the game before any turns for setup
  //rather than just on the ends of turns
  def startGame(): Unit = {
    //Clear and draw spells first turn
    clearAndDrawSpells()
  }

  def endTurn(): Unit = {
    curSide = curSide.opp
    turnNumber += 1
    numTechsThisTurn = 0

    mana(curSide) += extraManaPerTurn

    newTechsThisTurn = Vector()
    techLine.foreach { techState =>
      Side.foreach { side =>
        if(techState.startingLevelThisTurn(side) != TechAcquired && techState.level(side) == TechAcquired) {
          newTechsThisTurn = newTechsThisTurn :+ ((side,techState.tech))
        }
        techState.startingLevelThisTurn(side) = techState.level(side)
      }
    }

    for(i <- 0 until isBoardDone.length)
      isBoardDone(i) = false

    clearAndDrawSpells()
  }

  private def tryCanPayForReinforcement(side: Side, pieceName: PieceName): Try[Unit] = {
    if(side != curSide)
      Failure(new Exception("Currently the other team's turn"))
    else if(!Units.pieceMap.contains(pieceName))
      Failure(new Exception("Trying to pay for reinforcement piece with unknown name: " + pieceName))
    else {
      piecesAcquired(side).get(pieceName) match {
        case None => Failure(new Exception("Piece tech not acquired yet: " + pieceName))
        case Some(techState) =>
          if(techState.startingLevelThisTurn(side) != TechAcquired)
            Failure(new Exception("Cannot buy pieces on the same turn as teching to them"))
          else {
            val stats = Units.pieceMap(pieceName)
            if(mana(side) < stats.cost)
              Failure(new Exception("Not enough souls"))
            else
              Success(())
          }
      }
    }
  }

  private def payForReinforcement(side: Side, pieceName: PieceName): Try[Unit] = {
    tryCanPayForReinforcement(side,pieceName) match {
      case (err : Failure[Unit]) => err
      case (suc : Success[Unit]) =>
        val stats = Units.pieceMap(pieceName)
        mana(side) = mana(side) - stats.cost
        suc
    }
  }

  private def tryCanUnpayForReinforcement(side: Side, pieceName: PieceName): Try[Unit] = {
    if(side != curSide)
      Failure(new Exception("Currently the other team's turn"))
    else if(!Units.pieceMap.contains(pieceName))
      Failure(new Exception("Trying to unpay for reinforcement piece with unknown name: " + pieceName))
    else
      Success(())
  }

  private def unpayForReinforcement(side: Side, pieceName: PieceName): Try[Unit] = {
    tryCanUnpayForReinforcement(side,pieceName) match {
      case (err : Failure[Unit]) => err
      case (suc : Success[Unit]) =>
        val stats = Units.pieceMap(pieceName)
        mana(side) = mana(side) + stats.cost
        suc
    }
  }

  private def tryCanChooseSpell(side: Side, spellId: Int): Try[Unit] = {
    if(side != curSide)
      Failure(new Exception("Currently the other team's turn"))
    else if(!spellsToChoose.contains(spellId))
      Failure(new Exception("Trying to choose unknown spell id"))
    else if(spellsChosen.contains(spellId))
      Failure(new Exception("Spell already chosen by a board"))
    else
      Success(())
  }

  private def chooseSpell(side: Side, spellId: Int): Try[Unit] = {
    tryCanChooseSpell(side,spellId) match {
      case (err : Failure[Unit]) => err
      case (suc : Success[Unit]) =>
        spellsChosen = spellsChosen + spellId
        suc
    }
  }

  private def tryCanUnchooseSpell(side: Side, spellId: Int): Try[Unit] = {
    if(side != curSide)
      Failure(new Exception("Currently the other team's turn"))
    else if(!spellsToChoose.contains(spellId))
      Failure(new Exception("Trying to undo choosing unknown spell id"))
    else if(!spellsChosen.contains(spellId))
      Failure(new Exception("Cannot undo choosing an unchosen spell"))
    else
      Success(())
  }

  private def unchooseSpell(side: Side, spellId: Int): Try[Unit] = {
    tryCanUnchooseSpell(side,spellId) match {
      case (err : Failure[Unit]) => err
      case (suc : Success[Unit]) =>
        spellsChosen = spellsChosen - spellId
        suc
    }
  }

  private def doAddWin(side: Side): Unit = {
    wins(side) = wins(side) + 1
    if(wins(side) >= targetNumWins) {
      winner = Some(side)
    }
  }

  private def doAddUpcomingSpell(side: Side, spellId: Int): Unit = {
    upcomingSpells(side) = upcomingSpells(side) :+ spellId
  }

  private def tryCanPerformTech(side: Side, techLineIdx: Int): Try[Unit] = {
    if(side != curSide)
      Failure(new Exception("Currently the other team's turn"))
    else if(numTechsThisTurn > 0 && mana(side) < extraTechCost)
      Failure(new Exception("Not enough souls to tech more than once this turn"))
    else if(techLineIdx < 0 || techLineIdx >= techLine.length)
      Failure(new Exception("Invalid tech idx"))
    else if(techLineIdx > 0 && techLine(techLineIdx-1).level(side) == TechLocked)
      Failure(new Exception("Must unlock techs in order"))
    else {
      val techState = techLine(techLineIdx)
      (techState.level(side), techState.level(side.opp)) match {
        case (TechAcquired, _) => Failure(new Exception("Already own this tech"))
        case (TechUnlocked, TechAcquired) => Failure(new Exception("Cannot acquire tech owned by the opponent"))
        case (_, _) => Success(())
      }
    }
  }

  private def performTech(side: Side, techLineIdx: Int): Try[Unit] = {
    tryCanPerformTech(side,techLineIdx) match {
      case (err : Failure[Unit]) => err
      case (suc : Success[Unit]) =>
        if(numTechsThisTurn > 0)
          mana(side) = mana(side) - extraTechCost

        numTechsThisTurn += 1
        val techState = techLine(techLineIdx)
        techState.level(side) match {
          case TechAcquired => assertUnreachable()
          case TechLocked =>
            techState.level(side) = TechUnlocked
          case TechUnlocked =>
            techState.level(side) = TechAcquired
            techState.tech match {
              case PieceTech(pieceName) =>
                piecesAcquired(side) = piecesAcquired(side) + (pieceName -> techState)
            }
        }
        suc
    }
  }

  private def tryCanUndoTech(side: Side, techLineIdx: Int): Try[Unit] = {
    if(side != curSide)
      Failure(new Exception("Currently the other team's turn"))
    else if(techLineIdx < 0 || techLineIdx >= techLine.length)
      Failure(new Exception("Invalid tech idx"))
    else if(techLine(techLineIdx).level(side) == techLine(techLineIdx).startingLevelThisTurn(side))
      Failure(new Exception("Cannot undo tech from previous turns"))
    else if(techLineIdx < techLine.length - 1 &&
      techLine(techLineIdx).level(side) == TechUnlocked &&
      techLine(techLineIdx+1).level(side) != techLine(techLineIdx+1).startingLevelThisTurn(side))
      Failure(new Exception("Cannot undo this tech without undoing later techs first"))
    else {
      val techState = techLine(techLineIdx)
      techState.level(side) match {
        case TechLocked => Failure(new Exception("Cannot undo tech never acquired"))
        case TechUnlocked | TechAcquired => Success(())
      }
    }
  }

  private def undoTech(side: Side, techLineIdx: Int): Try[Unit] = {
    tryCanUndoTech(side,techLineIdx) match {
      case (err : Failure[Unit]) => err
      case (suc : Success[Unit]) =>
        if(numTechsThisTurn > 1)
          mana(side) = mana(side) + extraTechCost

        numTechsThisTurn -= 1
        val techState = techLine(techLineIdx)
        techState.level(side) match {
          case TechAcquired =>
            techState.level(side) = TechUnlocked
            techState.tech match {
              case PieceTech(pieceName) =>
                piecesAcquired(side) = piecesAcquired(side) - pieceName
            }
          case TechUnlocked =>
            techState.level(side) = TechLocked
          case TechLocked =>
            assertUnreachable()
        }
        suc
    }
  }

  private def tryCanSetBoardDone(boardIdx: Int, done: Boolean): Try[Unit] = {
    val _ = done
    if(boardIdx < 0 || boardIdx >= isBoardDone.length)
      Failure(new Exception("Invalid board idx"))
    else
      Success(())
  }

  private def setBoardDone(boardIdx: Int, done: Boolean): Try[Unit] = {
    tryCanSetBoardDone(boardIdx,done) match {
      case (err : Failure[Unit]) => err
      case (suc : Success[Unit]) =>
        isBoardDone(boardIdx) = done
        suc
    }
  }

  private def tryCanResignBoard(boardIdx: Int): Try[Unit] = {
    if(boardIdx < 0 || boardIdx >= isBoardDone.length)
      Failure(new Exception("Invalid board idx"))
    else
      Success(())
  }

  private def resignBoard(boardIdx: Int): Try[Unit] = {
    tryCanResignBoard(boardIdx) match {
      case (err : Failure[Unit]) => err
      case (suc : Success[Unit]) =>
        doAddWin(curSide.opp)
        suc
    }
  }

}

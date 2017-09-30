package minionsgame

package object core {
  /** PieceName:
    * An identifier for each kind of piece. Currently a string, like "zombie" or "bat". Used in the communication protocol
    * as well as in recorded files and such.
    */
  type PieceName = String

  /** AbilityName:
    * An identifier for each kind of ability. Currently a string, like "doubleattack". Used in the communication protocol
    * as well as in recorded files and such.
    */
  type AbilityName = String

  /** SpellName:
    * An identifier for each kind of spell. Currently a string, like "fester". Used in the communication protocol
    * as well as in recorded files and such.
    */
  type SpellName = String
}

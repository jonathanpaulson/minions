package minionsgame.core

object BoardMaps {
  private def make(xSize: Int, ySize: Int, s: String): Plane[Terrain] = {
    val map: Array[Terrain] =
      s.toArray.flatMap {
        case '0' => Some(StartHex(S0))
        case '1' => Some(StartHex(S1))
        case '.' => Some(Ground)
        case 'w' => Some(Water)
        case 'g' => Some(Graveyard)
        case _ => None
      }
    new Plane(xSize,ySize,HexTopology,map)
  }

  val basic = make(12,12,"""
w w w w w w w w w w w w
 w . . . . . . . . g w w
  w . . . . . g . . . . w
   w g . 0 . . . . . . . w
    w . . . . . . g w . g w
     w . . . w . . w w . . w
      w . . w w . . w . . . w
       w g . w g . . . . . . w
        w . . . . . . . 1 . g w
         w . . . . g . . . . . w
          w w g . . . . . . . . w
           w w w w w w w w w w w w
""")

}

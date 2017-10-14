package minionsgame.core

object BoardMaps {
  private def make(xSize: Int, ySize: Int, s: String): (() => BoardState) = { () =>
    val map: Array[Terrain] =
      s.toArray.flatMap {
        case '0' => Some(StartHex(S0))
        case '1' => Some(StartHex(S1))
        case '.' => Some(Ground)
        case 'w' => Some(Water)
        case 'g' => Some(Graveyard)
        case 's' => Some(SorceryNode)
        case 't' => Some(Teleporter)
        case 'z' => Some(Spawner(Units.zombie.name))

        //TODO this should be split out into various terrain tiles
        case '?' => Some(Ground)

        case _ => None
      }
    val plane = new Plane(xSize,ySize,HexTopology,map)
    BoardState.create(plane)
  }

  val empty = make(10,10,"""
 . . . . . . . . . .
  . . . . . . . . . .
   . . 0 . . . . . . .
    . . . . . . . . . .
     . . . . . . . . . .
      . . . . . . . . . .
       . . . . . . . . . .
        . . . . . . . 1 . .
         . . . . . . . . . .
          . . . . . . . . . .
""")

  val testMap = make(10,10,"""
 . . . . . . . . g w
  . . . . . g . . . .
   g . 0 . . . . . . .
    . . . . . . g w . g
     . . . w . . w w . .
      . . w w . . w . . .
       g . w g . . . . . .
        . . . . . . . 1 . g
         . . . . g . . . . .
          w g . . . . . . . .
""")

  val sorcerorsLair = make(10,10,"""
 s w . . . g . . w w
  w . . . . w . g . w
   g . 0 . w . . . g .
    . . . . . . . . . .
     w . g . s . . . w g
      . . w . . s . w . .
       . . . . . . . . . .
        g . . . w g . 1 . .
         . w . . . . . . . w
          w . g . . w . g w s
""")

  val chaosDiamond = make(10,10,"""
 . . . w . . . . g .
  . g . . g . . t . w
   w . 0 . . . . . w w
    w . . w . . . w . .
     . . . . g . . . g .
      . g . . . g . . . .
       . . w . . . w . . w
        w w . . . . . 1 . w
         w . t . . g . . g .
          . g . . . . w . . .
""")

  val apocalypse = make(10,10,"""
 . . g . . . . . g .
  w . . . . . . . . .
   w . 0 . g . . . . .
    w . . . . . . z . .
     . . w w . . g w . g
      g . w g . . w w . .
       . . z . . . . . . w
        . . . . . g . 1 . w
         . . . . . . . . . w
          . g . . . . . g . .
""")

  val treacherousPathways = make(10,10,"""
 . g . . . . . g . .
  . . . . . . . ? . .
   . . 0 . g . . ? ? g
    . . . . . . . . . .
     g . . . . . . . . .
      w ? w w ? . . g . .
       . . . . w . . . . .
        . . . . w . . 1 . .
         g ? . . ? . . . . g
          ? g . . w g . . . .
""")

  val midnightLake = make(10,10,"""
 . g . w w . . . . .
  . . . . . . . g . .
   . . 0 . . . . . g .
    g . . g . w w . . .
     w . . . w w w . . .
      . . . . w w . . . w
       . g . . . . g . . w
        . . . . . . . 1 . .
         . . . g . . . . . g
          . . . . . w g . . .
""")

  val eternalBattlefield = make(10,10,"""
 w w . g . . . . . g
  w . . . . . g . . .
   . . 0 . . . . . . .
    . . . . . . . . g .
     . g w . . . . . . .
      w w g . . . . . . .
       w . . . . . . . . g
        . . . . g w . 1 . .
         . g . . w g . . . w
          . . . w w . . . w w
""")

  val forbiddenIsle = make(10,10,"""
 w w w w w w w w w w
  w w . . g . w g . w
   w g 0 . . . . . . w
    w . . . . . . . . w
     w . . g w . . . g w
      w g . . . w g . . w
       w . . . . . . . . w
        w . . . . . . 1 g w
         w . g w . g . . w w
          w w w w w w w w w w
""")

  val puddles = make(10,10,"""
 w . . w . . w . . w
  . g . . w . . g . .
   . . 0 . . w . . w .
    w . . g . . . . . g
     . w . . g . . w . .
      . . w . . g . . w .
       g . . . . . g . . w
        . w . . w . . 1 . .
         . . g . . w . . g .
          w . . w . . w . . w
""")

  val blackenedShores = make(10,10,"""
 . . g . . . g . . .
  w . . . . . . . . .
   w . 0 . . . . . . .
    w . . . . g . . . g
     g w . . . . g . . .
      w . . w w . . . . .
       w g . w w . . . . .
        w . w . . . . 1 . g
         . . . g . w . . . .
          . . w w w g w w w .
""")



  val advancedMaps = Map(
    "Sorceror's Lair" -> sorcerorsLair,
    "Chaos Diamond" -> chaosDiamond,
    "Apocalypse" -> apocalypse,

    //DISABLED since we don't have the tiles implemented for it
    //"Treacherous Pathways" -> treacherousPathways,
  )

  val basicMaps = Map(
    "Midnight Lake" -> midnightLake,
    "Eternal Battlefield" -> eternalBattlefield,
    "Forbidden Isle" -> forbiddenIsle,
    "Puddles" -> puddles,
    "Blackened Shores" -> blackenedShores,
  )

  val groundImage = Map(
    "Sorceror's Lair" -> "img_terrain_grass1",
    "Chaos Diamond" -> "img_terrain_dirt1",
    "Apocalypse" -> "img_terrain_sand0",
    "Midnight Lake" -> "img_terrain_grass1",
    "Eternal Battlefield" -> "img_terrain_grass3",
    "Forbidden Isle" -> "img_terrain_grass0",
    "Puddles" -> "img_terrain_grass2",
    "Blackened Shores" -> "img_terrain_dirt0",
  )

  val waterImage = Map(
    "Sorceror's Lair" -> "img_terrain_water0",
    "Chaos Diamond" -> "img_terrain_water1",
    "Apocalypse" -> "img_terrain_water1",
    "Midnight Lake" -> "img_terrain_water0",
    "Eternal Battlefield" -> "img_terrain_water1",
    "Forbidden Isle" -> "img_terrain_water1",
    "Puddles" -> "img_terrain_water1",
    "Blackened Shores" -> "img_terrain_water0",
  )
}

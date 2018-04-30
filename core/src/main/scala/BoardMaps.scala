package minionsgame.core

object BoardMaps {
  private def make(xSize: Int, ySize: Int, startLocs: SideArray[Loc], s: String): (() => BoardState) = { () =>
    val map: Array[Terrain] =
      s.toArray.flatMap {
        case '.' => Some(Ground)
        case 'w' => Some(Water(false))
        case 'g' => Some(Graveyard)
        case 's' => Some(SorceryNode)
        case 't' => Some(Teleporter)
        case 'z' => Some(Spawner(Units.zombie.name))
        case 'm' => Some(Mist)
        case 'f' => Some(Firestorm(false))
        case 'a' => Some(Whirlwind(false))
        case 'e' => Some(Earthquake(false))

        case 'W' => Some(Water(true))
        case 'F' => Some(Firestorm(true))
        case 'A' => Some(Whirlwind(true))
        case 'E' => Some(Earthquake(true))

        case _ => None
      }
    val plane = new Plane(xSize,ySize,HexTopology,map)
    BoardState.create(plane, startLocs)
  }

  val empty = make(10,10,SideArray.createTwo(Loc(2, 2), Loc(7, 7)), """
 . . . . . . . . . .
  . . . . . . . . . .
   . . . . . . . . . .
    . . . . . . . . . .
     . . . . . . . . . .
      . . . . . . . . . .
       . . . . . . . . . .
        . . . . . . . . . .
         . . . . . . . . . .
          . . . . . . . . . .
""")

  val testMap = make(10,10,SideArray.createTwo(Loc(2, 2), Loc(7, 7)), """
 . . . . . . . . g w
  . . . . . g . . . .
   g . . . . . . . . .
    . . . . . . g w . g
     . . . w . . w w . .
      . . w w . . w . . .
       g . w g . . . . . .
        . . . . . . . . . g
         . . . . g . . . . .
          w g . . . . . . . .
""")

  val apocalypse = make(10,10,SideArray.createTwo(Loc(2, 2), Loc(7, 7)), """
 . . g . . . . . g .
  w . . . . . . . . .
   w . . . g . . . . .
    w . . . w . . z . .
     . . w w . . g w . g
      g . w g . . w w . .
       . . z . . w . . . w
        . . . . . g . . . w
         . . . . . . . . . w
          . g . . . . . g . .
""")

  val blackenedShores = make(10,10,SideArray.createTwo(Loc(2, 2), Loc(7, 7)), """
 . . g . . . g . . .
  w . . . . . . . . .
   w . . . . . . . . .
    w . . . . g . . . g
     g w . . . . g . . .
      w . . w w . . . . .
       w g . . w . . . . .
        w . w . . . . . . g
         . . . g . w . . . .
          . . w w w g w w w .
""")

  val chaosDiamond = make(10,10,SideArray.createTwo(Loc(2, 2), Loc(7, 7)), """
 . . . w . . . . g .
  . g . . g . . t . w
   w . . . . . . . w w
    w . . w . . . w . .
     . . . . g . . . g .
      . g . . . g . . . .
       . . w . . . w . . w
        w w . . . . . . . w
         w . t . . g . . g .
          . g . . . . w . . .
""")

  val eternalBattlefield = make(10,10,SideArray.createTwo(Loc(2, 2), Loc(7, 7)), """
 w w . g . . . . . g
  w . . . . . g . . .
   . . . . . . . . . .
    . . . . . . . . g .
     . g w . . . . . . .
      w w g . . . . . . .
       w . . . . . . . . g
        . . . . g w . . . .
         . g . . w g . . . w
          . . . w w . . . w w
""")

  val forbiddenIsle = make(10,10,SideArray.createTwo(Loc(2, 2), Loc(7, 7)), """
 w w w w w w w w w w
  w w . . g . w g . w
   w g . . . . . . . w
    w . . . . . . . . w
     w . . g w . . . g w
      w g . . . w g . . w
       w . . . . . . . . w
        w . . . . . . . g w
         w . g w . g . . w w
          w w w w w w w w w w
""")

  val megaPuddles = make(10,10,SideArray.createTwo(Loc(2, 2), Loc(7, 7)), """
 w . . w . . w . . w
  . g . . w . . g . .
   . . . . . w . . w .
    w . . g . . . . . g
     . w . . g . . w . .
      . . w . . g . . w .
       g . . . . . g . . w
        . w . . w . . . . .
         . . g . . w . . g .
          w . . w . . w . . w
""")

  val midnightLake = make(10,10,SideArray.createTwo(Loc(2, 2), Loc(7, 7)), """
 . g . w w . . . g .
  . . . . . . . . . g
   . . . . . . . . . .
    g . . g . w w . . .
     w . . . w w w . . .
      . . . . w w . . . w
       . g . . . . g . . w
        . . . . . . . . . .
         . . . g . . . . . g
          . . . . . w g . . .
""")

  val riverStyx = make(10,10,SideArray.createTwo(Loc(2, 2), Loc(7, 7)), """
 . . . . w w . . g .
  . g . . w . . . . .
   . . . g . . . . . .
    . . . . . . . w . .
     . . w w . . g w g .
      . g w g . . w w . .
       . . w . . . . . . .
        . . . . . . g . . .
         . . . . . w . . g .
          . g . . w w . . . .

    """)

  val sorcerorsLair = make(10,10,SideArray.createTwo(Loc(2, 2), Loc(7, 7)), """
 s w . . . g . . w w
  w . . . . w . g . w
   g . . . w . . . g .
    . . . . . . . . . .
     w . g . s . . . w g
      . . w . . s . w . .
       . . . . . . . . . .
        g . . . w g . . . .
         . w . . . . . . . w
          w . g . . w . g w s
""")

  val treacherousPathways = make(10,10,SideArray.createTwo(Loc(2, 2), Loc(7, 7)), """
 . g . . . . . g . .
  . . . . . . . a . .
   . . f . g . . W a g
    . . . . . . . . . .
     g . . . . . . . . .
      w e w w F . . g . .
       . . . . w . . . . .
        . . . . w . . f . .
         g A . . e . . . . g
          E g . . w g . . . .
""")

  val treacherousPathwaysMist = make(10,10,SideArray.createTwo(Loc(2, 2), Loc(7, 7)), """
 . g . . . . . g . .
  . m . . . . . m . .
   . . . . g . . w m g
    . . . . . . . . . .
     g . . . . . . . . .
      w m w w m . . g . .
       . . . . w . . . . .
        . . . . w . . . . .
         g w . . m . . . m g
          . g . . w g . . . .
""")

  val advancedMaps = Map(
    "Sorceror's Lair" -> sorcerorsLair,
    "Chaos Diamond" -> chaosDiamond,
    "Apocalypse" -> apocalypse,
    "Treacherous Pathways" -> treacherousPathways,
  )

  val basicMaps = Map(
    "Midnight Lake" -> midnightLake,
    "Eternal Battlefield" -> eternalBattlefield,
    "Forbidden Isle" -> forbiddenIsle,
    "MegaPuddles" -> megaPuddles,
    "Blackened Shores" -> blackenedShores,
    "River Styx" -> riverStyx
  )

  val groundImage = Map(
    "Sorceror's Lair" -> "img_terrain_grass1",
    "Chaos Diamond" -> "img_terrain_dirt1",
    "Apocalypse" -> "img_terrain_sand0",
    "Midnight Lake" -> "img_terrain_grass1",
    "Eternal Battlefield" -> "img_terrain_grass3",
    "Forbidden Isle" -> "img_terrain_grass0",
    "MegaPuddles" -> "img_terrain_grass2",
    "Blackened Shores" -> "img_terrain_dirt0",
    "Treacherous Pathways" -> "img_terrain_grass0",
    "River Styx" -> "img_terrain_dirt0",
  )

  val waterImage = Map(
    "Sorceror's Lair" -> "img_terrain_water0",
    "Chaos Diamond" -> "img_terrain_water1",
    "Apocalypse" -> "img_terrain_water1",
    "Midnight Lake" -> "img_terrain_water0",
    "Eternal Battlefield" -> "img_terrain_water1",
    "Forbidden Isle" -> "img_terrain_water1",
    "MegaPuddles" -> "img_terrain_water1",
    "Blackened Shores" -> "img_terrain_water0",
    "Treacherous Pathways" -> "img_terrain_water1",
    "River Styx" -> "img_terrain_water0",
  )
}

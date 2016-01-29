
object Board {
  object Import {
    val ADJVECS : List[Vec] = List(Vec(-1,-1), Vec(0,-1), Vec(-1,0), Vec(1,0), Vec(0,1), Vec(1,1))
  }
}

class Board(
  val width: Int,
  val height: Int
) {
  val tiles: Plane[Tile] = new Plane[Tile](width,height,Wall)
  val pieces: Plane[Piece] = new Plane[Piece](width,height,null) //TODO replace null
  val pieceCounts: Plane[Int] = new Plane[Int](width,height,0)


}

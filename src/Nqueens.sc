val positionsList: List[(String, Int, Int)] = List(
  ("Q", 0,0), ("K", 3, 4))

val posList: List[(String, Int, Int)] = List()

val figureList: List[String] = List ("Queen", "Queen", "King", "King",
  "Bishop", "Bishop", "Knight")


def placeFigure (pos: (String, Int, Int), positions: List[(String, Int, Int)]):List[(String, Int, Int)]  = {
  pos :: positions
}


def isOccupyPosition (pos : (String, Int, Int), positions: List[(String, Int, Int)]): Boolean = {
  if (positions.isEmpty) false
  else positions exists (p => p._2 == pos._2 && p._3 == pos._3 )
}
placeFigure(("Kn", 5, 6), positionsList)


isOccupyPosition(("K", 2, 4), positionsList)



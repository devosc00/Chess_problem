/**
 * Created by rafa on 03.12.14.
 */
class ChessSolution {

  def findSolution (figureList: List[(String, Pos)]) = figureList.head._1 match {
    case None => List(Nil)
    case "Queen" => ???
    case "King" => ???
    case "Bishop" => ???
    case "Knight" => ???
  }

}

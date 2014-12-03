/**
 * Created by rafa on 03.12.14.
 */
class ChessSolution {

 // var positionList: List[(String, Int, Int)]
//  val solutionList: List[List[(String, Int, Int)]]


  def findSolution (figureList: List[String], positionList: List[(String, Int, Int)]): Boolean =

    figureList match {
      case List() => Nil false
      case x :: xs => x match {
        case "Queen" => for {
          row <- 1 to figureList.size
          column <- 1 to 7
          pos = ("Q", row, column)
          if isEmptyPosition(pos, positionList) && isNotAttackted(pos, positionList)
          placeFigure(pos, positionList)
/*          if (figureList.isEmpty)
          addSolution(positionList)
          else findSolution(figureList.tail)*/
        } false

        case "King" => for {
          row <- 1 to figureList.size
          column <- 1 to 7
          pos = ("K", row, column)
          if isEmptyPosition(pos, positionList) && isNotAttackted(pos, positionList)
          placeFigure(pos, positionList)
        } false

        case "Bishop" => for {
          row <- 1 to figureList.size
          column <- 1 to 7
          pos = ("B", row, column)
          if isEmptyPosition(pos, positionList) && isNotAttackted(pos, positionList)
          placeFigure(pos, positionList)
        } false

        case "Knight" => for {
          row <- 1 to figureList.size
          column <- 1 to 7
          pos = ("Kn", row, column)
          if isEmptyPosition(pos, positionList) && isNotAttackted(pos, positionList)
          placeFigure(pos, positionList)
        } false
      }
        findSolution(xs, positionList)
    }



  def isEmptyPosition (pos : (String, Int, Int), positions: List[(String, Int, Int)]) = {
    if (positions.isEmpty) true
      else positions forall ( p => p._2 == pos._2 && p._3 == pos._3 )
   }


  def isNotAttackted (pos : (String, Int, Int), positions: List[(String, Int, Int)]) = {
    if (positions.isEmpty) true
    else positions forall (p => p._1 match {
      case "Q" => Figure.isQueenAttack((p._2, p._3), (pos._2, pos._3)) false
      case "K" => Figure.isKingAttack((p._2, p._3), (pos._2, pos._3)) false
      case "B" => Figure.isBishopAttack((p._2, p._3), (pos._2, pos._3)) false
      case "Kn" => Figure.isBishopAttack((p._2, p._3), (pos._2, pos._3)) false
      case _ => print("Bad figure name") false
      }
    )
  }

  def placeFigure (pos: (String, Int, Int), positions: List[(String, Int, Int)]): Boolean = {
    pos :: positions
    true
  }

  def addSolution(positionList: List[(String, Int, Int)], solutionList: List[List[(String, Int, Int)]]): Unit = {
    positionList :: solutionList
  }

}

/**
 * Created by rafa on 03.12.14.
 */
class ChessSolution {

  val positionList: List[(String, Int, Int)]
  val solusionList: List[List[(String, Int, Int)]]


  def findSolution (figureList: List[String]) =
    figureList.head match {
      case "" => false
      case "Queen" => for {
        row <- 1 to figureList.size
        column <- 1 to 7
        pos = ("Q", row, column)
        if isEmptyPosition(pos, positionList) && isNotAttackted(pos, positionList)
        placeFigure(pos, positionList)
          if (figureList.isEmpty)
            addSolution(positionList)
         // else findSolution(figureList.tail)
      } false

      case "King" => for {
        position <- positionList
        if (isEmpty(position, positionList) && (isNotAttackted(position, positionList))) {
          placeKing(position, positionList)
          if (findSolution(figureList.tail)) true else addSolution(positionList)
        }
      } false

      case "Bishop" => for {
        position <- positionList
        if (isEmpty(position, positionList) && (isNotAttackted(position, positionList))) {
          placeBishop(position, positionList)
          if (findSolution(figureList.tail)) true else addSolution(positionList)
        }
      } false

      case "Knight" => for {
        position <- positionList
        if (isEmpty(position, positionList) && (isNotAttackted(position, positionList))) {
          placeKnight(position, positionList)
          if (findSolution(figureList.tail)) true else addSolution(positionList)
        }
      } false
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

  def addSolution(positionList: List[(String, Int, Int)]): Unit = {
    positionList :: solusionList
  }

}

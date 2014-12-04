package chess_problem

/**
 * Created by rafa on 03.12.14.
 */
class ChessSolution {

 // var positionList: List[(String, Int, Int)]
//  val solutionList: List[List[(String, Int, Int)]]


  def findSolution (figureList: List[String], positionList: List[(String, Int, Int)]): List[(String, Int, Int)] =

    figureList match {
      case List() => Nil
      case last :: Nil => last match {
        case "Queen" => for {
          row <- 1 to figureList.size -1
          column <- 1 to 7
          pos = ("Q", row, column)
          if !isOccupyPosition(pos, positionList) && !isAttackted(pos, positionList) //end if are less then two queens
          updatedList = placeFigure(pos, positionList)
        } updatedList

        case "King" => for {
          row <- 1 to figureList.size - 1
          column <- 1 to 7
          pos = ("K", row, column)
          if !isOccupyPosition(pos, positionList) && !isAttackted(pos, positionList)
          updatedList = placeFigure(pos, positionList)
        } updatedList

        case "Bishop" => for {
          row <- 1 to figureList.size -1
          column <- 1 to 7
          pos = ("B", row, column)
          if !isOccupyPosition(pos, positionList) && !isAttackted(pos, positionList)
          updatedList = placeFigure(pos, positionList)
        } updatedList

        case "Knight" => for {
          row <- 1 to figureList.size
          column <- 1 to 7
          pos = ("Kn", row, column)
          if !isOccupyPosition(pos, positionList) && !isAttackted(pos, positionList)
          updatedList = placeFigure(pos, positionList)
        } updatedList

        case _ => println("Lost last element")

      }
        positionList


      case head :: tail => head match {
        case "Queen" => for {
          row <- 1 to figureList.size -1
          column <- 1 to 7
          pos = ("Q", row, column)
          if !isOccupyPosition(pos, positionList) && !isAttackted(pos, positionList)
          updatedList = placeFigure(pos, positionList)
        } findSolution(figureList.tail, updatedList)


        case "King" => for {
          row <- 1 to figureList.size
          column <- 1 to 7
          pos = ("K", row, column)
          if !isOccupyPosition(pos, positionList) && !isAttackted(pos, positionList)
          updatedList = placeFigure(pos, positionList)
        } findSolution(figureList.tail, updatedList)

        case "Bishop" => for {
          row <- 1 to figureList.size
          column <- 1 to 7
          pos = ("B", row, column)
          if !isOccupyPosition(pos, positionList) && !isAttackted(pos, positionList)
          updatedList = placeFigure(pos, positionList)
        } findSolution(figureList.tail, updatedList)

        case "Knight" => for {
          row <- 1 to figureList.size -1
          column <- 1 to 7
          pos = ("Kn", row, column)
          if !isOccupyPosition(pos, positionList) && !isAttackted(pos, positionList)
          updatedList = placeFigure(pos, positionList)
        } findSolution(figureList.tail, updatedList)

        case _ => println("Lost last element")
      }
        findSolution(figureList.tail, positionList)
    }



  def isOccupyPosition (pos : (String, Int, Int), positions: List[(String, Int, Int)]): Boolean = {
    if (positions.isEmpty) false
    else positions exists (p => p._2 == pos._2 && p._3 == pos._3 )
  }


  def isAttackted (pos : (String, Int, Int), positions: List[(String, Int, Int)]) = {
    if (positions.isEmpty) false
    else positions forall (p => p._1 match {
      case "Q" => Figure.isQueenAttack((p._2, p._3), (pos._2, pos._3))
      case "K" => Figure.isKingAttack((p._2, p._3), (pos._2, pos._3))
      case "B" => Figure.isBishopAttack((p._2, p._3), (pos._2, pos._3))
      case "Kn" => Figure.isKnightAttack((p._2, p._3), (pos._2, pos._3))
      case _ => print("Bad figure name")
        true
      }
    )
  }

  def placeFigure (pos: (String, Int, Int), positions: List[(String, Int, Int)]): List[(String, Int, Int)] = {
    pos :: positions
  }

  def addSolution(positionList: List[(String, Int, Int)], solutionList: List[List[(String, Int, Int)]]): Unit = {
    positionList :: solutionList
  }

/*  def tryPlaceFigure (figure: String, positions: List[(String, Int, Int)]): List[(String, Int, Int)] = {
    for {
      row <- 1 to 7
      column <- 1 to 7
      pos = (figure, row, column)
      if !isOccupyPosition(pos, positions) && !isAttackted(pos, positions)
      updatedList = placeFigure(pos, positions)
    }
    updatedList
  }*/

}

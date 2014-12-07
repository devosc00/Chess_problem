package chess_problem

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
 * Created by rafa on 03.12.14.
 */

class ChessSolution {

  def run(figures: List[String]): List[List[(String, Int, Int)]] = {

    // @tailrec
    def findSolution(figureList: List[String], size: Int): List[List[(String, Int, Int)]] =

      figureList match {
        case List() => Nil

        case head :: tail => head match {

/*

          case "Queen" => x //return List[IndexedSeq[List[(...)]]]
          lazy val x = findSolution(figureList, size - 1) map (x => for {col <- 1 to size
                                                                         pos = ("Q", size, col)
                                                                         if !isOccupyPosition(pos, x) && !isAttackted(pos, x)
            } yield pos :: x
              )

          case "King" => x
            lazy val x = findSolution(figureList, size - 1) map (x => for {col <- 1 to size
                                                                           pos = ("K", size, col)
                                                                           if !isOccupyPosition(pos, x) && !isAttackted(pos, x)
            } yield pos :: x
              )

          case "Bishop" => x
            lazy val x = findSolution(figureList, size - 1) map (x => for {col <- 1 to size
                                                                           pos = ("B", size, col)
                                                                           if !isOccupyPosition(pos, x) && !isAttackted(pos, x)
            } yield pos :: x
              )

          case "Knight" => x
            lazy val x = findSolution(figureList, size - 1) map (x => for {col <- 1 to size
                                                                           pos = ("Kn", size, col)
                                                                           if !isOccupyPosition(pos, x) && !isAttackted(pos, x)
            } yield pos :: x
              )

*/


          case "Queen" => x  //return List[List(Strint, Int, Int)]]

            lazy val x = for {
                              figures <- findSolution(figureList, size -1)
                               col <- 1 to size
                               pos = ("Q", size, col)
                               if !isOccupyPosition(pos, figures) && !isAttackted(pos, figures)
                            } yield pos :: figures

          case "King" => x

            lazy val x = for {
                              figures <- findSolution(figureList, size - 1 )
                              column <- 1 to size
                              pos = ("K", size, column)
                               if !isOccupyPosition(pos, figures) && !isAttackted(pos, figures)
                            } yield pos :: figures

          case "Bishop" => x

            lazy val x = for {
                              figures <- findSolution(figureList, size - 1)
                              column <- 1 to size
                              pos = ("B", size, column)
                                if !isOccupyPosition(pos, figures) && !isAttackted(pos, figures)
                            } yield pos :: figures

          case "Knight" => x

            lazy val x = for {
                              figures <- findSolution(figureList, size - 1)
                              column <- 1 to size
                              pos = ("Kn", size, column)
                                if !isOccupyPosition(pos, figures) && !isAttackted(pos, figures)
                            } yield pos :: figures

        }

            findSolution(figureList.tail, figureList.tail.size)
        }

          findSolution(figures, figures.size)

      }



    def isOccupyPosition(pos: (String, Int, Int), positions: List[(String, Int, Int)]): Boolean = {
      if (positions.isEmpty) {
        println("is occupy pos")
        false
      }
      else positions exists (p => p._2 == pos._2 && p._3 == pos._3)
    }


    def isAttackted(pos: (String, Int, Int), positions: List[(String, Int, Int)]) = {
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

/*
    def placeFigure(pos: (String, Int, Int), positions: List[(String, Int, Int)]): List[(String, Int, Int)] = {
      pos :: positions
    }

    def addSolution(positionList: List[(String, Int, Int)], solutionList: List[List[(String, Int, Int)]]): Unit = {
      positionList :: solutionList
    }
*/

    /*
  def tryPlaceFigure (figures: String): List[List[(String, Int, Int)]] = {
    for {
      figures <- tryPlaceFigure()
      column <- 1 to 7
      pos = (figure, row, column)
      if !isOccupyPosition(pos, positions) && !isAttackted(pos, positions)
    } yield


  }
*/

}

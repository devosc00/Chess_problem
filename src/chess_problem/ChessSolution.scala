package chess_problem

import scala.annotation.tailrec
import scala.collection.immutable
import scala.collection.mutable.ListBuffer

/**
 * Created by rafa on 03.12.14.
 */

class ChessSolution {

  var acc = List[(String, Int, Int)]()

  var queens = List[(String, Int, Int)]()

  var kings = List[(String, Int, Int)]()

  var bishops = List[(String, Int, Int)]()

  var knights = List[(String, Int, Int)]()


  def findSolution(figures: List[String], size: Int) = {

    @tailrec
    def accFunction (figures: List[String], acc: List[(String, Int, Int)]): List[(String, Int, Int)] = figures match {

      case Nil => acc
      case head :: tail => (1 to size).flatMap(x => (1 to size).map(y => accFunction(figures.tail,(head, x, y)).map(_ :: acc)))
      // acc foreach (x => println(x._1 + " " + x._2 + " " + x._3))
    }
    accFunction(figures, Nil)
  }


  def buildUniqueLists(figuresPos: List[(String, Int, Int)]) = {

    for (figure <- figuresPos)
      figure match {

        case ("Queen", _, _) => queens ::= figure
        case ("King", _, _) => kings ::= figure
        case ("Bishop", _, _) => bishops ::= figure
        case ("Knight", _, _) => knights ::= figure
      }

  }


  val queensComb = queens.combinations(2).toList
  val kingsComb = kings.combinations(2).toList
  val bishopsComb = bishops.combinations(2).toList

  println(queensComb, kingsComb, bishopsComb)


  def isProperTimesOnList (figure: (String, Int, Int), figures: List[(String, Int, Int)]): Boolean = { figure._1 match {
    case "Knight" => Figure.isOnList(figure, figures, 0)
    case _ => Figure.isOnList(figure, figures, 1)
  }

  }


  def generalFilter (figure: (String, Int, Int), positions: List[(String, Int, Int)]): Boolean = {
    !isOccupyPosition(figure, positions) && isProperTimesOnList(figure, positions) && !isAttackted(figure, positions)
  }



  def isOccupyPosition(pos: (String, Int, Int), positions: List[(String, Int, Int)]): Boolean = {
    if (positions.isEmpty)
      false
    else positions exists (p => p._2 == pos._2 && p._3 == pos._3)
  }


  def isAttackted(pos: (String, Int, Int), positions: List[(String, Int, Int)]): Boolean =
    pos._1 match {
      case "Queen" => positions forall (p => Figure.isQueenAttack((p._2, p._3), (pos._2, pos._3)))
      case "King" => positions forall (p => Figure.isKingAttack((p._2, p._3), (pos._2, pos._3)))
      case "Bishop" => positions forall  (p => Figure.isBishopAttack((p._2, p._3), (pos._2, pos._3)))
      case "Knight" => positions forall  (p => Figure.isKnightAttack((p._2, p._3), (pos._2, pos._3)))
      case _ => print("Bad figure name")
        true
    }
}

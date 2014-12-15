package chess_problem



/**
 * Created by rafa on 03.12.14.
 */

class ChessSolution {

  var acc = List[(String, Int, Int)]()

  var queens = List[(String, Int, Int)]()

  var kings = List[(String, Int, Int)]()

  var bishops = List[(String, Int, Int)]()

  var knights = List[(String, Int, Int)]()



  def findSolution(figures: List[String], size: Int): List[(String, Int, Int)] = figures match {

      case Nil => acc
      case head :: tail => for {
        x <- 1 to size
        y <- 1 to size
        pos = (head, x, y)
      } acc ::= pos

    findSolution(figures.tail, size)
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


def twoSameFiguresComb: List[List[(String, Int, Int)]] =  {
  val queensComb = queens.combinations(2).map(fig => fig.dropWhile(f => generalFilter(f, fig))).toList.filterNot(x => x.isEmpty)
  val kingsComb = kings.combinations(2).map(fig => fig.dropWhile(f => generalFilter(f, fig))).toList.filterNot(x => x.isEmpty || x.size == 1)
  val bishopsComb = bishops.combinations(2).map(fig => fig.dropWhile(f => generalFilter(f, fig))).toList.filterNot(x => x.isEmpty)


 val treeFiguresList = queensComb ::: kingsComb ::: bishopsComb

  treeFiguresList
}


  def withStream(figures: List[(String, Int, Int)]): List[List[(String, Int, Int)]]  = {

    lazy val countAll = { acc.reverse.combinations(7).toStream.map (x => x.dropWhile(x => generalFilter(x, acc))) }
    val tr = countAll.toList
    for (t <- tr) println(t)
    tr
  }


  def generalFilter (figure: (String, Int, Int), positions: List[(String, Int, Int)]): Boolean = {
    isOccupyPosition(figure, positions) && isAttackted(figure, positions) && isProperTimesOnList(figure, positions)
  }


  def isProperTimesOnList (figure: (String, Int, Int), figures: List[(String, Int, Int)]): Boolean = { figure._1 match {
    case "Knight" => Figure.isOnList(figure, figures, 0)
    case _ => Figure.isOnList(figure, figures, 1)
  }

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

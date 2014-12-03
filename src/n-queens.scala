

/**
 * Created by rafa on 02.12.14.
 */

object Nqueen {

  type Solutions = List[List[(Int, Int)]]

  val row = 4

  val column = 4

  type chessboard = List[List[(Int, Int)]]


/*
  def placeFigures(figures: Int): Unit ={

    if (figures == 0) List(Nil)
    else

  }*/

  def placeQueens(n: Int): Solutions = n match {
    case 0 => List(Nil)
    case _ => for {
      queens <- placeQueens(n - 1)
      y <- 1 to row
      queen = (n, y)
      if (isSafeQueen(queen, queens))
    } yield queen :: queens
  }


  def printSolution(solutions: Solutions) {

    println(solutions.size + " solutions found")
    // print the board of the first solution
    for (queen <- solutions.head; x <- 1 to row) {
      if (queen._2 == x) print("Q ") else print(". ")
      if (x == row) println()
    }
  }


  def isSafeQueen(queen: (Int, Int), others: List[(Int, Int)]) =
    others forall (!isAttacked(queen, _))

  def isAttacked(q1: (Int, Int), q2: (Int, Int)) =
    q1._1 == q2._1 ||
    q1._2 == q2._2 ||
      (q2._1 - q1._1).abs == (q2._2 - q1._2).abs


  def main(args: Array[String]) {

    println("Start")
    printSolution(placeQueens(4))
  }
}


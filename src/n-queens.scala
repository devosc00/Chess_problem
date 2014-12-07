import scala.annotation.tailrec

/**
 * Created by rafa on 02.12.14.
 */

object Nqueen {

  type Solutions = List[List[(Int, Int)]]



  def queens(n: Int): List[List[(Int, Int)]] = {
    //@tailrec
    def placeQueens(k: Int): List[List[(Int, Int)]] =
      if (k == 0)
        List(List())
      else
        for {
          queens <- placeQueens(k - 1)
          column <- 1 to n
          queen = (k, column)
          if isSafeQueen(queen, queens)
        } yield queen :: queens

    placeQueens(n)
  }


  def factorialTR(n: Int): Int = {
    @tailrec
    def loop(n: Int, acum: Int): Int =
      if (n == 0) acum else loop(n - 1, acum * n)
    loop(n, 1)
  }


/*
  def printSolution(solutions: Solutions) {

    println(solutions.size + " solutions found")
    // print the board of the first solution
    for (queen <- solutions.head; x <- 1 to n) {
      if (queen._2 == x) print("Q ") else print(". ")
      if (x == n) println()
    }
  }
*/

  def isSafe(col: Int, queens: List[Int], delta: Int): Boolean =
    queens match {
      case Nil => true
      case List() => true
      case c :: rest =>
        c != col &&
          math.abs(c - col) != delta &&
          isSafe(col, rest, delta + 1)
    }


  def isSafeQueen(queen: (Int, Int), others: List[(Int, Int)]) =
    others forall (!isAttacked(queen, _))

  def isAttacked(q1: (Int, Int), q2: (Int, Int)) =
    q1._1 == q2._1 ||
    q1._2 == q2._2 ||
      (q2._1 - q1._1).abs == (q2._2 - q1._2).abs


/*  def main(args: Array[String]) {

    queens(2)
    println("Start")

    //printSolution(placeQueens(4))
  }*/
}


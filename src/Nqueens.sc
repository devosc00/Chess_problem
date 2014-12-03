object Nqueens {
  type Queen = (Int, Int)
  type Solutions = List[List[Queen]]

  def run (n : Int) {
    val size: Int = n match {
      case None => sys.error("Provide a board size")
      case Some(size) => n


    }


    def placeQueens(n: Int): Solutions = n match {
      case 0 => List(Nil)
      case _ => for {
        queens <- placeQueens(n - 1)
        y <- 1 to size
        queen = (n, y)
        if (isSafe(queen, queens))
      } yield queen :: queens
    }

    val solutions = placeQueens(size)
    println(solutions.size + " solutions found")
    // print the board of the first solution
    for (queen <- solutions.head; x <- 1 to size) {
      if (queen._2 == x) print("Q ") else print(". ")
      if (x == size) println()
    }
  }

  def isSafe(queen: Queen, others: List[Queen]) =
    others forall (!isAttacked(queen, _))

  def isAttacked(q1: Queen, q2: Queen) =
    q1._1 == q2._1 ||
      q1._2 == q2._2 ||
      (q2._1-q1._1).abs == (q2._2-q1._2).abs

  run (20)

}






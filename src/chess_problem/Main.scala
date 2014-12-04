package chess_problem

/**
 * Created by rafa on 04.12.14.
 */
object Main {



  def main(args: Array[String]): Unit = {

   val positions:List[(String, Int, Int)] = List(("Q", 0,0 ))

    val chess = new ChessSolution
    chess.findSolution(List("Queen", "King", "Bishop", "Knight"), positions)
  }

}

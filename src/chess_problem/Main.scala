package chess_problem

/**
 * Created by rafa on 04.12.14.
 */
object Main {



  def main(args: Array[String]): Unit = {

    val fullList = List("Queen", "Queen", "King", "King", "Bishop", "Bishop", "Knight")

    val partList = List("Queen", "King", "Bishop", "Knight")

    val chess = new ChessSolution

    val solution = chess.run(fullList)

    println(solution.size)
  }

}

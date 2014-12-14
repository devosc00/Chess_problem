package chess_problem

/**
 * Created by rafa on 04.12.14.
 */
object Main {



  def main(args: Array[String]): Unit = {


    val partList = List("Queen", "King", "Bishop", "Knight")


    val chess = new ChessSolution

    val solution = chess.buildUniqueLists(chess.findSolution(partList, 7))

    println(solution)
  }

}

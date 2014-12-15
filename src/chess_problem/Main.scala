package chess_problem

/**
 * Created by rafa on 04.12.14.
 */
object Main {



  def main(args: Array[String]): Unit = {


    val partList = List("Queen", "King", "Bishop", "Knight")


    val chess = new ChessSolution

    println(chess.findSolution(partList, 7).size)

    val solution = chess.buildUniqueLists(chess.findSolution(partList, 7))

    val semilist = chess.twoSameFiguresComb

//    val stream = chess.withStream(chess.findSolution(partList, 7))

//    val print = for (sem <- semilist) println(sem)
//    print
//    println(semilist.size)

  }

}

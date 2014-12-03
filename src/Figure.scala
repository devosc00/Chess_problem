/**
 * Created by rafa on 02.12.14.
 */
object Figure {

/*  case class Queen(pos: (Int, Int)) extends Figure

  case class King(pos: (Int, Int)) extends Figure

  case class Bishop(pos: (Int, Int)) extends Figure

  case class Knight(pos: (Int, Int)) extends Figure*/


  def sameRow(p1: (Int, Int), p2: (Int, Int)) = p1._1 == p2._1

  def sameColumn(p1: (Int, Int), p2: (Int, Int)) = p1._2 == p2._2

  def sameDiag(p1: (Int, Int), p2: (Int, Int)) = (p1._1 - p2._1).abs == (p1._2 - p2._2).abs

  def sameL(p1: (Int, Int), p2: (Int, Int)) = (
    ((p1._1 + 2, p1._2 + 1) ==(p2._1 + 2, p2._2 + 1))
      || ((p1._1 - 2, p1._2 - 1) ==(p2._1 - 2, p2._2 - 1))
      || ((p1._1 - 2, p1._2 + 1) ==(p2._1 - 2, p2._2 + 1))
      || ((p1._1 + 2, p1._2 - 1) ==(p2._1 + 2, p2._2 - 1))
      || ((p1._1 - 1, p1._2 - 2) ==(p2._1 - 1, p2._2 - 2))
      || ((p1._1 - 1, p1._2 + 2) ==(p2._1 - 1, p2._2 + 2))
      || ((p1._1 + 1, p1._2 - 2) ==(p2._1 + 1, p2._2 - 2))
      || ((p1._1 + 1, p1._2 + 2) ==(p2._1 + 1, p2._2 + 2))
    )

  def kingMoves(p1: (Int, Int), p2: (Int, Int)) = (
    ((p1._1 - 1, p1._2 - 1) ==(p2._1 - 1, p2._2 - 1))
      || ((p1._1 - 1, p1._2 + 1) ==(p2._1 - 1, p2._2 + 1))
      || ((p1._1 + 1, p1._2 - 1) ==(p2._1 + 1, p2._2 - 1))
      || ((p1._1 + 1, p1._2 + 1) ==(p2._1 + 1, p2._2 + 1))
      || ((p1._1 + 1) == (p2._1 + 1))
      || ((p1._1 - 1) == (p2._1 - 1))
      || ((p1._2 - 1) == (p2._2 - 1))
      || ((p1._2 + 1) == (p2._2 + 1))
    )

  def isQueenAttack(p1: (Int, Int), p2: (Int, Int)) = (
    sameRow(p1: (Int, Int), p2: (Int, Int))
      || sameColumn(p1: (Int, Int), p2: (Int, Int))
      || sameDiag(p1: (Int, Int), p2: (Int, Int))
    )

  def isBishopAttack(p1: (Int, Int), p2: (Int, Int)) = sameDiag(p1: (Int, Int), p2: (Int, Int))

  def isKingAttack(p1: (Int, Int), p2: (Int, Int)) = kingMoves(p1: (Int, Int), p2: (Int, Int))

  def isKnightAttack(p1: (Int, Int), p2: (Int, Int)) = sameL(p1: (Int, Int), p2: (Int, Int))

}
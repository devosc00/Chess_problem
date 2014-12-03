/**
 * Created by rafa on 02.12.14.
 */
trait Figure {

  case class Queen(pos: Pos) extends Figure
  case class King(pos: Pos) extends Figure
  case class Bishop(pos: Pos) extends Figure
  case class Knight(pos: Pos) extends Figure


}

case class Pos(row: Int, column: Int) {
  def sameRow(p: Pos) = row == p.row
  def sameColumn(p: Pos) = column == p.column
  def sameDiag(p: Pos) = (p.column - column).abs == (p.row - row).abs
  def sameL(p: Pos) = (
    ((p.column + 2, p.row +1) == (column + 2, row + 1))
    || ((p.column - 2, p.row - 1) == (column - 2, row -1))
    || ((p.column -2, p.row + 1) == (column - 2, row + 1))
    || ((p.column + 2, p.row -1) == (column + 2, row - 1))
    || ((p.column +1, p.row +2) == (column + 1, row +2))
    || ((p.column + 1, p.row -2) == (column + 1, row -2))
    || ((p.column -1, p.row + 2) == (column -1, row +2))
    || ((p.column -1, p.row -2) == (column -1, row -2))
    )
  def kingMoves(p: Pos) = (
    ((p.column + 1, p.row +1) == (column + 1, row + 1))
      || ((p.column - 1, p.row - 1) == (column - 1, row -1))
      || ((p.column -1, p.row + 1) == (column - 1, row + 1))
      || ((p.column + 1, p.row -1) == (column + 1, row - 1))
      || ((p.column +1) == (column + 1))
      || ((p.row -1) == (row -1))
      || ((p.column -1) == (column -1))
      || ((p.row +1) == (row + 1))
    )

  def illegal(p: Pos) = sameRow(p) || sameColumn(p) || sameDiag(p)
  def legal(p: Pos) = !illegal(p)
}
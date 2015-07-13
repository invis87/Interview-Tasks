package chess_horse

import utils.Extensions._

object ChessTable {

  val size: Int = 8

  private val horseMoves: List[(Int, Int)] = List(
    (2, 1),
    (2, -1),
    (-2, 1),
    (-2, -1)
  )

  def isInside(pos: ChessmanPosition): Boolean = {
    pos.x.between(1, size) && pos.y.between(1, size)
  }

  def horseMoves(pos: ChessmanPosition): List[ChessmanPosition] = {
    horseMoves.flatMap(step => List(pos.move(step), pos.move(step._2, step._1))).filter(pos => isInside(pos))
  }
}

case class ChessmanPosition(x: Int, y: Int){
  def move(stepX: Int, stepY: Int): ChessmanPosition = ChessmanPosition(x + stepX, y + stepY)
  def move(step: (Int, Int)): ChessmanPosition = move(step._1, step._2)
  override def toString = "(%s, %s)".format(x, y)
}

object ChessmanPosition {
  def apply(pos: (Int, Int)): ChessmanPosition = ChessmanPosition(pos._1, pos._2)
}

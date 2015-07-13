package chess_horse

object HorsePaths {

  class Path(history: List[ChessmanPosition], val endPos: ChessmanPosition) {
    def size: Int = history.length
    def extend(move: ChessmanPosition) = new Path(history :+ move, move)
    override def toString = history mkString " -> "
  }

  def from(paths: Set[Path], explored: Set[ChessmanPosition]): Stream[Set[Path]] = {
    if(paths.isEmpty) Stream.empty
    else {
      val more = paths.flatMap(path => ChessTable.horseMoves(path.endPos) map path.extend)
        .filter(path => !explored.contains(path.endPos))

      paths #:: from(more, explored ++ more.map(_.endPos))
    }
  }

  def apply(initPos: ChessmanPosition, target: ChessmanPosition): Stream[Path] = {
    val initialPath = new Path(List(initPos), initPos)
    val pathSets = from(Set(initialPath), Set(initPos))

    pathSets.flatMap(pathSet => pathSet.filter(path => path.endPos == target))
  }
}

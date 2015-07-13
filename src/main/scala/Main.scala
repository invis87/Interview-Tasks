import chess_horse.HorsePaths.Path
import chess_horse.{ChessmanPosition, HorsePaths}
import find_words._
import letter_combinations._

object Main {

  def main(args: Array[String]) {
    println("======= All possible words for number \"567\"")
    println(letterCombinationsSoluiton("567") mkString ", ")
    println()

    println("======= 8 longest horse movements")
    printSomeLongHorsePath()
    println()

    println("======= All words from 350k words dictionary that contains symbols \"cqrd\" (in order)")
    val wordsTree = WordsTreeFactory.wordsTree("src/main/resources/dictionary.txt")
    wordsTree.findWords("cqrd").foreach(println)
    println()

    println("======= Print words tree for words (array, listarray, list, linkedlist, leaf, lemonade, lemon")
    val tree = RootNode.
      addWord("array").
      addWord("listarray").
      addWord("list").
      addWord("linkedlist").
      addWord("leaf").
      addWord("lemonade").
      addWord("lemon")
    println(tree)
    println()
  }

  def printSomeLongHorsePath() = {
    def helper(i: Int, power: Int): Int = {
      (i / power) % 8 + 1
    }

    val index = 0 to 4095
    index.
      map(i => chessHorsePath((helper(i, 64), helper(i, 16)), (helper(i, 8), helper(i, 1)))).
      sortBy(p => -p.size).
      take(8).
      foreach(println)
  }

  def chessHorsePath(from: (Int, Int), to: (Int, Int)): Path = {
    HorsePaths(ChessmanPosition(from), ChessmanPosition(to)).head
  }

  def letterCombinationsSoluiton(digits: String): Iterator[String] = {
    SlotMachineFactory.create(digits).iterator
  }
}
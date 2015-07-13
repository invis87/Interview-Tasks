package find_words

import scala.io.Source._
import scala.collection.mutable

trait WordsTree {
  val letter: Char
  val endOfTheWord: Boolean

  def lastCharInWord: WordsTree

  def children: List[WordsTree]

  def contains(char: Char): Boolean

  val leafsCount: Int

  def depth(level: Int = 0): Int

  override def toString: String = {
    val treeMap: List[mutable.HashMap[Int, CharPointer]] = List.fill(depth() + 1)(new mutable.HashMap[Int, CharPointer])

    buildCharsMap(treeMap, 0)

    def printMap(bld: StringBuilder, map: mutable.HashMap[Int, CharPointer]) = {
      val maxKey = map.keySet.max
      for (key <- 0 to maxKey) {
        if (map.contains(key)) {
          bld.append("[%s]".format(map(key)))
        }
        if (key != maxKey) {
          bld.append('\t')
        }
      }
      bld.append('\n')
    }

    val bld = new StringBuilder()
    treeMap.foreach(printMap(bld, _))
    bld.toString()
  }

  def buildCharsMap(treeMap: List[mutable.HashMap[Int, CharPointer]], offset: Int): Int

  def leftLeafs: Int

  def addWord(str: Seq[Char]): WordsTree

  def allWords(path: Array[Char] = Array.empty): List[String] = {
    if (endOfTheWord) {
      new String(path :+ letter) :: children.flatMap(ch => ch.allWords(path :+ letter))
    } else {
      children.flatMap(ch => ch.allWords(path :+ letter))
    }
  }

  def findWords(chars: Seq[Char], path: Array[Char] = Array.empty): List[String] = {
    if (chars.size == 1 && chars.head == letter) {
      return allWords(path)
    }

    if (chars.head == letter) {
      children.flatMap(ch => ch.findWords(chars.tail, path :+ letter))
    } else {
      children.flatMap(ch => ch.findWords(chars, path :+ letter))
    }
  }

  protected def fromChars(str: Seq[Char]): WordsTree = {
    require(str.nonEmpty)
    str.reverse.tail.foldLeft[WordsTree](Leaf(str.last))((tree, ch) => Node(ch, List(tree)))
  }

}

object RootNode extends Leaf('.') {
  override val endOfTheWord = false
}

case class Node(letter: Char, nodes: List[WordsTree], endOfTheWord: Boolean = false) extends WordsTree {
  require(nodes.size > 0, "The number of nodes must be positive.")

  def lastCharInWord: WordsTree = Node(letter, children, endOfTheWord = true)

  val leafsCount: Int = nodes.map(_.leafsCount).sum

  override def leftLeafs: Int = leafsCount / 2

  override def children: List[WordsTree] = nodes

  override def depth(level: Int): Int = 1 + children.map(_.depth(level + 1)).max

  def addWord(str: Seq[Char]): WordsTree = {
    require(str.nonEmpty)
    val childOpt = children.find(ch => ch.letter == str.head)
    if (childOpt.nonEmpty) {
      val child = childOpt.get
      val index = children.indexOf(child)
      if (str.size == 1) {
        Node(letter, children.updated(index, child.lastCharInWord))
      } else {
        Node(letter, children.updated(index, child.addWord(str.tail)), endOfTheWord)
      }
    } else {
      val newTree = fromChars(str)
      Node(letter, newTree :: children, endOfTheWord)
    }
  }

  override def contains(char: Char): Boolean = {
    if (letter == char) {
      true
    } else {
      nodes.exists(_.contains(char))
    }
  }

  override def buildCharsMap(treeMap: List[mutable.HashMap[Int, CharPointer]], offset: Int): Int = {
    val leftOffset = leftLeafs + offset
    treeMap.head(leftOffset) = CharPointer(letter, endOfTheWord)

    children.foldLeft(offset)((offset, node) => node.buildCharsMap(treeMap.tail, offset))
  }
}

case class Leaf(letter: Char) extends WordsTree {
  val leafsCount: Int = 1
  val endOfTheWord = true

  override def leftLeafs: Int = 0

  def lastCharInWord: WordsTree = this

  override def children: List[WordsTree] = List.empty

  override def depth(level: Int): Int = 0

  def addWord(str: Seq[Char]): WordsTree = {
    require(str.nonEmpty)
    val newTree = fromChars(str)
    if (str.head == letter)
      newTree
    else
      Node(letter, List(newTree), endOfTheWord)
  }

  override def contains(char: Char): Boolean = letter == char

  override def buildCharsMap(treeMap: List[mutable.HashMap[Int, CharPointer]], offset: Int): Int = {
    treeMap.head(offset) = CharPointer(letter, endOfTheWord)
    offset + 1
  }
}

case class CharPointer(ch: Char, endOfTheWord: Boolean) {
  override def toString = {
    if (endOfTheWord) ch.toUpper.toString
    else
      ch.toString
  }
}

object WordsTreeFactory {
  def wordsTree(filePath: String): WordsTree = {
    var tree: WordsTree = RootNode
    val lines = fromFile(filePath).getLines()
    while(lines.hasNext){
      tree = tree.addWord(lines.next().toLowerCase)
    }
    tree
  }
}
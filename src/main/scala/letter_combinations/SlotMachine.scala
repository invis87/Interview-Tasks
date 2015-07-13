package letter_combinations

import scala.annotation.tailrec
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

private class Slot(val chars: List[Char], parent: Option[Slot] = None) extends Iterable[Char] {
  val charsSize: Int = chars.size
  private var state: Int = 0
  private var isFinished: Boolean = false

  override def iterator: Iterator[Char] = new Iterator[Char] {
    def hasNext: Boolean = !isFinished

    def next(): Char = {
      val index = state % charsSize
      increment()
      chars(index)
    }
  }

  @tailrec
  private def increment(): Unit = {
    state += 1
    if (state == charsSize) {
      state = 0
      if (parent.isEmpty) {
        isFinished = true
      } else {
        parent.get.increment()
      }
    }
  }

  def value: Char = chars(state % charsSize)
}

class SlotMachine(slots: List[Slot]) extends Iterable[String] {
  override def iterator: Iterator[String] = new Iterator[String] {
    def hasNext = slots.head.iterator.hasNext

    def next(): String = {
      val str = new String(slots.map(s => s.value).toArray)
      slots.last.iterator.next()
      str
    }
  }
}

object SlotMachineFactory {
  private val lettersMap: HashMap[Char, List[Char]] = HashMap(
    ('2', List('a', 'b', 'c')),
    ('3', List('d', 'e', 'f')),
    ('4', List('g', 'h', 'i')),
    ('5', List('j', 'k', 'l')),
    ('6', List('m', 'n', 'o')),
    ('7', List('p', 'q', 'r', 's')),
    ('8', List('t', 'u', 'v')),
    ('9', List('w', 'x', 'y', 'z'))
  )

  def create(number: Seq[Char]): SlotMachine = {
    val result: ListBuffer[Slot] = new ListBuffer[Slot]
    var prevSlot: Option[Slot] = None
    for (char <- number) {
      val numberChars = lettersMap.getOrElse(char, List.empty)
      if (numberChars.nonEmpty) {
        val slot = new Slot(numberChars, prevSlot)
        prevSlot = Option(slot)
        result.append(slot)
      }
    }

    new SlotMachine(result.toList)
  }
}

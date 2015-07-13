package utils

object Extensions {

  implicit class RichInt(val i: Int) extends AnyVal {
    def square = i * i
    def between(lowerBound: Int, upperBound: Int): Boolean = i >= lowerBound && i <= upperBound
  }

}

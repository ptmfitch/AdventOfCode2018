import scala.io.Source

object Day2 extends App {
  val ids = Source.fromFile("day2").getLines.toList

  def containsNRepeatedLetters(s: String, n: Int): Boolean = s.exists(c => s.count(_ == c) == n)
  println(ids.count(containsNRepeatedLetters(_ , 2)) * ids.count(containsNRepeatedLetters(_, 3)))

  def isOffByOne(s1: String, s2: String): Boolean = {
    s1.zip(s2).count(t => t._1 == t._2) == s1.length - 1
  }
  ids.foreach(s1 => {
    val offByOne = ids.find(s2 => isOffByOne(s1, s2))
    if (offByOne.isDefined) {
      println(s1.zip(offByOne.get).filter(t => t._1 == t._2).map(_._1).mkString(""))
    }
  })
}

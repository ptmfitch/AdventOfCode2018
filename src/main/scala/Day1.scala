import scala.io.Source

object Day1 extends App {
  val frequencies = Source.fromFile("day1").getLines.toList.map(_.toInt)
  def loop(is: List[Int], seen: Set[Int] = Set(), c: Int = 0): Int = is match {
    case Nil => loop(frequencies, seen, c)
    case h :: t =>
      val f = c + h
      if (seen.contains(f)) f
      else loop(t, seen ++ Set(f), c + h)
  }
  println(loop(frequencies))
}

import scala.io.Source

object Day5 extends App {
  val polymer = Source.fromFile("day5").getLines.mkString
  def opposites(a: Char, b: Char) = a != b && a.toLower == b.toLower
  val res = polymer.toList.foldLeft(List.empty[Char])({
    case (h :: t, next) if opposites(h, next) => t
    case (l, next) => next :: l
  }).reverse.mkString("")
  println(('a' to 'z').map(c => {
    val filtered = res.filterNot(d => d == c || d == c.toUpper)
    filtered.toList.foldLeft(List.empty[Char])({
      case (h :: t, next) if opposites(h, next) => t
      case (l, next) => next :: l
    }).reverse.mkString("").length
  }).min)
}

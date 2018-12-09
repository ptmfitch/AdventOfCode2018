import scala.io.Source

object Day3 extends App {

  case class Vector2(x: Int, y: Int) {
    def equals(that: Vector2): Boolean = {
      this.x == that.x && this.y == that.y
    }
  }

  case class Claim(in: String) {
    private def parseInput(sep: Char): Vector2 = {
      s"\\d+$sep\\d+".r.findFirstIn(in).get.split(sep) match {
        case Array(a, b) => Vector2(a.toInt, b.toInt)
      }
    }
    val start: Vector2 = parseInput(',')
    val size: Vector2 = parseInput('x')
    val xRange = Range(start.x, start.x + size.x)
    val yRange = Range(start.y, start.y + size.y)
    val claimed: Seq[Vector2] = xRange.flatMap(x => yRange.map(y => Vector2(x, y)))
  }

  val claims = Source.fromFile("day3").getLines.toList.map(Claim)
  val easyFilter = claims.filter(c => !claims.exists(d => c != d && c.start == d.start))
  val downToOne = easyFilter.par.find(c => {
    println(c.in)
    !easyFilter.par.exists(d => c.in != d.in && c.claimed.exists(d.claimed.contains))
  })
  println(downToOne.get.in)

}

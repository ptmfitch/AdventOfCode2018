import scala.io.Source

object Day4 extends App {

  val messages = Source.fromFile("day4").getLines.toList.map(Message(_)).sortWith(Message.sortDate)
  val groups = groupMessages(messages)
  val guards = groups.groupBy(_.head.guardNo).map(g => g._1 -> g._2.flatMap(_.drop(1)).sliding(2, 2).map {
    case List(a, b) => (a.min, b.min)
  }.toSeq)
  val sleepiestGuard = guards.maxBy(g => repeatedMinuteAsleep(g._2))
  println(minuteMostAsleep(sleepiestGuard._2) * sleepiestGuard._1)

  case class Message(in: String) {
    val datetimeString: String = in.slice(6, 17)
    val messageString: String = in.drop(19)
    val month: Int = datetimeString.take(2).toInt
    val day: Int = datetimeString.slice(3, 5).toInt
    val hour: Int = datetimeString.slice(6, 8).toInt
    val min: Int = datetimeString.takeRight(2).toInt
    val guardNo: Int = messageString.filter(_.isDigit) match {
      case "" => -1
      case ds => ds.toInt
    }
  }
  object Message {
    def sortDate(dateA: Message, dateB: Message): Boolean = {
      if (dateA.month == dateB.month) {
        if (dateA.day == dateB.day) {
          if (dateA.hour == dateB.hour) {
            dateA.min < dateB.min
          } else dateA.hour < dateB.hour
        } else dateA.day < dateB.day
      } else dateA.month < dateB.month
    }
  }
  def groupMessages(ms: List[Message]): List[List[Message]] = {
    if (ms.isEmpty) Nil
    else {
      val group = ms.takeWhile(m => m == ms.head || "Guard".r.findFirstIn(m.messageString).isEmpty)
      List(group) ++ groupMessages(ms.drop(group.length))
    }
  }

  def repeatedMinuteAsleep(pairs: Seq[(Int, Int)]): Int = {
    val minutesAsleep = pairs.flatMap(p => Range(p._1, p._2))
    if (minutesAsleep.nonEmpty) {
      val minuteMostAsleep = minutesAsleep.maxBy(m => minutesAsleep.count(_ == m))
      minutesAsleep.count(_ == minuteMostAsleep)
    } else -1
  }

  def totalMinutesAsleep(pairs: Seq[(Int, Int)]): Int = {
    pairs.map(p => p._2 - p._1).sum
  }

  def minuteMostAsleep(pairs: Seq[(Int, Int)]): Int = {
    val minutesAsleep = pairs.flatMap(p => Range(p._1, p._2))
    minutesAsleep.maxBy(m => minutesAsleep.count(_ == m))
  }
}

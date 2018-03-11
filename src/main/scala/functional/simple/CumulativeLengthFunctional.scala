package functional.simple

import scala.collection.immutable.Queue

/** Reads lines and prints cumulative length of all lines so far along with most recent line itself. */
object CumulativeLengthFunctional extends App {

  val lines = scala.io.Source.stdin.getLines

  val results = Iterator.iterate(Option(("dummy", 0))) {
    case Some((_, n)) =>
      if (lines.hasNext) {
        val line = lines.next
        Option((line, n + line.length))
      } else {
        None
      }
  } drop (1) takeWhile (x => x.isDefined) map (x => x.get)

  results.foreach { r => println(r) }

}

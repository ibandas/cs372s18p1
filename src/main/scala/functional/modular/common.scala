package functional.modular

/** Defines a dependency (plug-in contract) on a run method that processes an input stream. */
trait Task[Result] {
  def run(input: Iterator[Int], windowSize: Array[Int]): Iterator[Int]
}

/**
 * Provides a reusable main task tied to stdin and stdout.
 * Depends on a suitable run method.
 */
trait Main[Result] extends Task[Result] {
  def main(args: Array[String]): Unit = {
    val lines = scala.io.Source.stdin.getLines
    val words = lines.flatMap(_.split("\\W+")).map(_.toInt)
    val result = run(words, args.map(_.toInt))
    result.foreach(println)
  }
}
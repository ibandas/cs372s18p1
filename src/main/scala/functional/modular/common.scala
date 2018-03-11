package functional.modular

/** Defines a dependency (plug-in contract) on a run method that processes an input stream. */
trait Task[Result] {
  def run(input: Iterator[String]): Iterator[Result]
}

/**
 * Provides a reusable main task tied to stdin and stdout.
 * Depends on a suitable run method.
 */
trait Main[Result] extends Task[Result] {
  def main(args: Array[String]): Unit = {
    val lines = scala.io.Source.stdin.getLines
    val result = run(lines)
    result.foreach(println)
  }
}
package imperative.modular

/** Defines a dependency (plug-in contract) on an output method (Observer). */
trait Output[Result] {
  def doOutput(result: Result): Unit
}

/** Provides a reusable output observer tied to println/stdout. */
trait OutputToStdOut[Result] extends Output[Result] {
  override def doOutput(result: Result) = println(result)
}

/** Defines a dependency (plug-in contract) on a run method that processes an input stream. */
trait Task {
  def run(input: Iterator[Int], windowSize: Array[Int]): Unit
}

/**
 * Provides a reusable main task tied to stdin and stdout.
 * Depends on a suitable run method.
 */
trait Main[Result] extends Task with OutputToStdOut[Result] {
  def main(args: Array[Int], windowSize: Array[Int]): Unit = {
    val lines = scala.io.Source.stdin.getLines
    val windows = scala.io.Source.stdin.getLines
    run(lines, windows)
  }
}

https://ibandas@bitbucket.org/psenkow/cs372s18p1.git
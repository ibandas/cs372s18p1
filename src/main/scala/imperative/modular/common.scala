package imperative.modular

import scala.collection.mutable.ListBuffer

/** Defines a dependency (plug-in contract) on an output method (Observer). */
trait Output {
  def doOutput(result: (Int, Int, ListBuffer[Option[(Double, Double, Double)]])): Unit
}

/** Provides a reusable output observer tied to println/stdout. */
trait OutputToStdOut extends Output {
  override def doOutput(result: (Int, Int, ListBuffer[Option[(Double, Double, Double)]])) = println(result)
}

/** Defines a dependency (plug-in contract) on a run method that processes an input stream. */
trait Task {
  def run(input: Iterator[Int], windowSize: Array[Int]): Unit
}

/**
 * Provides a reusable main task tied to stdin and stdout.
 * Depends on a suitable run method.
 */
trait Main extends Task with OutputToStdOut {
  def main(args: Array[String]): Unit = {
    val lines = scala.io.Source.stdin.getLines()
    val words = lines.flatMap(_.split("\\W+")).map(_.toInt)
    run(words, args.map(_.toInt))
  }
}

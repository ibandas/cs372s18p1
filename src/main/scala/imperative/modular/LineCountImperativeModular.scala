package imperative.modular

/**
 * Provides a main method for reading lines and printing line count along with line itself.
 * Depends on a suitable Output provider.
 */
trait CountLines extends Task with Output[(Int, Int)] {

  def run(input: Iterator[Int], windowSizes: Array[Int]) = {
    var count = 0
    for (line <- input) {
      count += 1
      doOutput((count, line))
    }
  }
}

/** A concrete main application object. */
object LineCountImperativeModular extends Main[(Int, Int)] with CountLines
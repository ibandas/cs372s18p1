package imperative.modular
import scala.collection.mutable.Queue

/**
 * Provides a main method for reading lines and printing the cumulative length
 * of all lines so far along with the most recent line itself.
 * Depends on a suitable Output provider.
 */
trait AccumulateLength extends Task with Output[(Int, Int)] {

  def run(input: Iterator[Int], windowSizes: Array[Int]) = {
    var counter = 0
    var windowQueue = Queue[Int]()
    val windowSizeMax = windowSizes.max

    for (line <- input) { //Loops through each number stream
      windowQueue.enqueue(line)
      if (windowQueue.length > windowSizeMax) { //If the queue is at max length, dequeue so the new value will be enqueued
        windowQueue.dequeue()
      }
      counter = counter + 1
      print(line + ", " + counter + ", ") //First two numbers are always the input number and counter
      for (windowSize <- windowSizes) { //Uses the queue for each window to get stats
        //If there is enough numbers in queue, do stats
        if (windowQueue.length >= windowSize) {
          val stats = movingAverage(windowQueue, windowSize)
          print(stats._1 + ", " + stats._2 + ", " + stats._3 + ", ")
        } else { //If there is not enough numbers in queue, print question marks
          print("?, ?, ? | ")
        }
      }
      println(" ")
      //length += line.length
      //doOutput((line, length))
    }
  }

  def movingAverage(inputQueue: Queue[Int], divisible: Int) = {
    val windowArray = inputQueue.takeRight(divisible)
    val sum = windowArray.sum
    val min = windowArray.min
    val max = windowArray.max
    val avg = sum / divisible
    (min, avg, max)
  }
}

/** A concrete main application object. */
object CumulativeLengthImperativeModular extends Main[(Int, Int)] with AccumulateLength
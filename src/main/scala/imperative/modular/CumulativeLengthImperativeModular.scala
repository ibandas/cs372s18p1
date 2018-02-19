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
    var average = 0
    var windowQueue = Queue[Int]()
    var windowSizeMax = 0
    var windowSize = 0

    for (window <- windowSizes) { //Loop to find the window with the largest size
      if (window > windowSizeMax) {
        windowSizeMax = window
      } else {
        println("")
      }
    }

    for (line <- input) { //Loops through each number stream
      if (windowQueue.length == windowSizeMax) { //If the queue is at max length, dequeue so the new value will be enqueued
        windowQueue.dequeue()
      }
      if (input.hasNext) { //If there is another incoming input number from stream, increase counter
        counter = counter + 1
      }
      windowQueue.enqueue(line)
      print(line + ", " + counter + ", ") //First two numbers are always the input number and counter
      for (windowLine <- windowSizes) { //Uses the queue for each window to get stats
        if (windowQueue.take(windowLine) == true) { //If there is enough numbers in queue, do stats
          average = movingAverage(windowQueue, windowSize)
          print(windowQueue.min + ", " + average + ", " + windowQueue.max + ", ")
        } else { //If there is not enough numbers in queue, print question marks
          print("?, ?, ?")
        }
        println(" ")
      }
      //length += line.length
      //doOutput((line, length))
    }
  }

  def movingAverage(inputQueue: Queue[Int], divisible: Int): Int = {
    var sum = 0
    var avg = 0
    var windowArray = inputQueue.take(divisible)
    for (value <- windowArray) {
      sum = sum + value
    }
    avg = sum / divisible
    return avg
  }
}

/** A concrete main application object. */
object CumulativeLengthImperativeModular extends Main[(Int, Int)] with AccumulateLength
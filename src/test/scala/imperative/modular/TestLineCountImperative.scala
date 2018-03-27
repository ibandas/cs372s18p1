package imperative.modular

import org.scalatest.WordSpec

import scala.collection.mutable.{ Buffer, ListBuffer }

/** Provides an output observer that accumulates the results in a buffer one can inspect later. */
trait OutputToBuffer extends Output {

  private val buffer = Buffer.empty[(Int, Int, ListBuffer[Option[(Double, Double, Double)]])]

  def getResults: Seq[(Int, Int, ListBuffer[Option[(Double, Double, Double)]])] = buffer.toSeq

  override def doOutput(result: (Int, Int, ListBuffer[Option[(Double, Double, Double)]])) = { buffer += result }
}

class TestLineCountImperative extends WordSpec {

  /** Creates a (mutable!) SUT instance. */
  def createSUT() = new AccumulateLength with OutputToBuffer

  "An imperative LineCounter" when {
    "given an empty iterator" should {
      "produce an empty output" in {
        // create SUT instance for this test case
        val sut = createSUT()
        // exercise SUT
        sut.run(Iterator.empty, Array(1, 5, 10))
        // check effect on output observer
        assert(sut.getResults.isEmpty)
      }
    }

    "given a nonempty iterator" should {
      "produce the correct nonempty output" in {
        // input data for this test case
        val data = Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
        // create SUT instance for this test case
        val sut = createSUT()
        // exercise SUT
        sut.run(data.iterator, Array(1, 10))
        // check effect on output observer
        assert(sut.getResults === (1 to data.length).zip(data))
      }
    }
  }
  //Test Empty
  //For three window sizes, test if first has values and next two dont
}
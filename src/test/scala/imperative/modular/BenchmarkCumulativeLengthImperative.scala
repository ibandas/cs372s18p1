package imperative.modular

import org.scalameter.api._

object BenchmarkCumulativeLengthImperative extends Bench.LocalTime {

  val sut = new AccumulateLength {
    var length = 0
    override def doOutput(result: (Int, Int)): Unit = { length += 1 }
  }

  val sizes = Gen.exponential("size")(1000, 10000000, 10)
  val windows = Gen.exponential("windows")(1000, 10000000, 10)
  var windowArray = Array[Int](1, 5, 10)

  performance of "CumulativeLengthImperative" in {
    measure method "run" in {
      using(sizes) in {
        size => sut.run(Iterator.fill(size)(1), windowArray)
      }
    }
  }
}

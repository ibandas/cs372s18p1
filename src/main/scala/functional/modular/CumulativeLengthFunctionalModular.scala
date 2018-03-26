package functional.modular

import scala.collection.AbstractIterator
import scala.collection.immutable.Queue

/** Reads lines and prints cumulative length of all lines so far along with most recent line itself. */
object CumulativeLengthFunctionalModular extends Main[(String, Int)] {

  // def iterate[T](start: T)(f: (T) ⇒ T): Iterator[T]
  // def scanLeft[B](z: B)(op: (B, A) ⇒ B): Iterator[B]
  // https://github.com/scala/scala/blob/v2.12.4/src/library/scala/collection/Iterator.scala#L595

  /**
   * Simple implementation of `scanLeft` using `iterate` and `takeWhile`.
   * For `takeWhile` to work, this requires lifting the resulting iterator to `Option`.
   * Alternatively, one could use `null`, but that would be less stylish.
   */
  def scanLeftUsingIterate[T, U](it: Iterator[T])(z: U)(op: (U, T) => U): Iterator[U] =
    Iterator.iterate(Option(z)) {
      case Some(r) =>
        if (it.hasNext) Option(op(r, it.next())) else None
    } takeWhile (_.isDefined) map (_.get)

  /**
   * From-scratch implementation of `scanLeft` as an `AbstractIterator` by essentially
   * fusing `++` for the initial value, `iterate`, and `takeWhile`.
   * This fusion results in a slightly complicated state space, but it works efficiently.
   */
  def scanLeftAsIterator[T, U](it: Iterator[T])(z: U)(op: (U, T) => U): Iterator[U] = new AbstractIterator[U] {
    private var hd: T = _
    private var hdDefined = false
    private var tail = it
    private var initial = true
    private var elem = z

    def hasNext = initial || hdDefined || tail.hasNext && {
      hd = tail.next()
      hdDefined = true
      hdDefined
    }

    def next() =
      if (hasNext) {
        hdDefined = false
        if (initial) initial = false
        else elem = op(elem, hd)
        elem
      } else
        Iterator.empty.next()
  }

  def accumulateCount(acc: (String, Int), line: String): (String, Int) = (line, acc._2 + line.length)
  //Make function like above with MovingAverage

  def run(lines: Iterator[String]): Iterator[(String, Int)] =
    lines.scanLeft("dummy", 0)(accumulateCount).drop(1)
  // the following fails when running interactively: delayed by one line
  // lines.scanLeft(("dummy", 0))(accumulateCount).drop(1)
  // see https://github.com/scala/bug/issues/10709
}

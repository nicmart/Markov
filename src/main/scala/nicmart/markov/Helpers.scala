/*
 * (c) 2014 Nicolò Martini
 *
 * http://nicolo.martini.io
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

package nicmart.markov

/**
 * Object Description
 */
object Helpers {

  /**
   * Implement a stream-compatible sliding feature
   */
  implicit class ImprovedStream[T](stream: Stream[T]) {
    def slidingStream(size: Int, step: Int = 1): Stream[Stream[T]] = {
      val window = stream.take(size)
      if (window.length < size) Stream()
      else stream.take(size) #:: stream.drop(step).slidingStream(size)
    }

    def slidingPaddedStream(size: Int, position: Int = 0): Stream[Stream[Option[T]]] = {
      val prefix: Stream[Option[T]] = fixedLengthAndValueStream[Option[T]](None, position)
      val suffix: Stream[Option[T]] = fixedLengthAndValueStream[Option[T]](None, size - position - 1)

      val optionStream = stream.map(Some(_))

      (prefix #::: optionStream #::: suffix).slidingStream(size)
    }

    def takeUntil(predicate: T => Boolean, times: Int = 1) = {
      val countStream = stream.scanLeft(1) { (count, element) =>
        if (predicate(element)) count + 1
        else count
      }
      stream.zip(countStream)
        .takeWhile{ case (element, count) => count <= times }
        .map{ case(element, count) => element }
    }

    private def fixedLengthAndValueStream[U](value: U, length: Int): Stream[U] = {
      if (length == 0) Stream()
      else value #:: fixedLengthAndValueStream(value, length - 1)
    }
  }
}

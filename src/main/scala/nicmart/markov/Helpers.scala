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
  }
}

/*
 * (c) 2014 Nicolò Martini
 *
 * http://nicolo.martini.io
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

package nicmart.markov

import IndexType._

/**
 * Class Description
 */
case class IndexType(keyLength: Int, valueLength: Int, direction: Direction) {
  /**
   * Given a window, returns a pair whose first element is the keys sequence, and the second one
   * is the values sequence
   */
  def keysAndValues[T](window: Seq[T]): (Seq[T], Seq[T]) = {
    if (direction == Forward) window.splitAt(keyLength) else window.splitAt(valueLength).swap
  }
}

object IndexType {
  sealed abstract class Direction

  case object Forward extends Direction
  case object Backward extends Direction
}
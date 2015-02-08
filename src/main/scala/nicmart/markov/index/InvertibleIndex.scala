/*
 * (c) 2015 Nicolò Martini
 *
 * http://nicolo.martini.io
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

package nicmart.markov.index

/**
 * Trait Description
 */
trait InvertibleIndex[T] extends Index[T] {

  def indexExist(index: Int): Boolean

  def invert(index: Int): T
}

object InvertibleIndex {
  def apply[T] = new MapInvertibleIndex[T]
}

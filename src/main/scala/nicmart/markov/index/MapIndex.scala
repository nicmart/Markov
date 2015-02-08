/*
 * (c) 2015 Nicolò Martini
 *
 * http://nicolo.martini.io
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

package nicmart.markov.index

import scala.collection.mutable

/**
 * A Map-based index
 */
class MapIndex[T] extends Index[T] {

  private[this] val map: mutable.Map[T, Int] = mutable.Map()

  def isIndexed(term: T): Boolean = map.isDefinedAt(term)

  def apply(term: T): Int = map(term)

  def add(term: T): Index[T] = {
    if (!isIndexed(term)) {
      map(term) = map.size + 1
    }

    this
  }

  def size: Int = map.size
}

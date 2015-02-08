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
 * Class Description
 */
class MapInvertibleIndex[T] extends InvertibleIndex[T] {

  private[this] val directIndex = Index[T]
  private[this] val inverseIndex = mutable.Map[Int, T]()

  def indexExist(index: Int): Boolean = inverseIndex.isDefinedAt(index)

  def invert(index: Int): T = inverseIndex(index)

  def isIndexed(term: T): Boolean = directIndex.isIndexed(term)

  def apply(term: T): Int = directIndex(term)

  def add(term: T): Index[T] = {

    val index: Int = directIndex.indexAndGet(term)
    inverseIndex(index) = term

    this
  }

  def size: Int = directIndex.size
}

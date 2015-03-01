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
trait MapIndex[From, To] extends Index[From, To]{

  private[this] val directMap: mutable.Map[From, To] = mutable.Map()
  private[this] val inverseMap: mutable.Map[To, From] = mutable.Map()

  def isIndexed(term: From): Boolean = directMap.isDefinedAt(term)

  def apply(term: From): To = directMap(term)

  def add(term: From): Index[From, To] = {
    if (!isIndexed(term)) {
      val index = nextAvailableIndex
      directMap(term) = index
      inverseMap(index) = term
    }

    this
  }

  override def size: Int = directMap.size

  def indexExists(index: To): Boolean = inverseMap.isDefinedAt(index)

  def invert(index: To): From = inverseMap(index)

  protected def nextAvailableIndex: To
}

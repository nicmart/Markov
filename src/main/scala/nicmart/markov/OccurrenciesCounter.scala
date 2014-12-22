/*
 * (c) 2014 Nicolò Martini
 * 
 * http://nicolo.martini.io
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

package nicmart.markov

import scala.collection
import scala.collection.mutable
import nicmart._

/**
 * Class Description
 */
class OccurrenciesCounter[T, S] {

  private val index: mutable.Map[T, mutable.Map[S, Int]] = mutable.Map()

  def addElement(key: T, element: S): this.type = {

    if (!index.isDefinedAt(key)) index(key) = mutable.Map()

    val map = index(key)

    if (!map.isDefinedAt(element)) map(element) = 1
    else map(element) = map(element) + 1

    this
  }

  def getCount(key: T, element: S) = {

    index.get(key).map(_.getOrElse(element, 0)).getOrElse(0)
  }

  def getIndexes = index

  def getDistributionMap: collection.Map[T, WeightedRandomDistribution[S]] = {

    index.mapValues(map => {
      val values = map.map(pair => WeightedValue(pair._1, pair._2))
      new WeightedRandomDistribution(values)
    })
  }
}

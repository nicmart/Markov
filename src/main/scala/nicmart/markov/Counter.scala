/*
 * (c) 2014 Nicolò Martini
 *
 * http://nicolo.martini.io
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

package nicmart.markov

trait Counter {
  type CountMap[T] = Map[T, Int]
  type PrefixedCountMap[T, S] = Map[T, Map[S, Int]]

  def countCollection[T](xs: Traversable[T]): CountMap[T]
  def countMap[T, S](map: Map[T, S]): PrefixedCountMap[T, S]
}

object Counter extends Counter {
  def countCollection[T](xs: Traversable[T]): CountMap[T] = {
    val emptyMap = Map[T, Int]().withDefault(_ => 0)
    xs.foldLeft(emptyMap){(map, element) => map.updated(element, map(element) + 1)}
  }

  def countMap[T, S](map: Map[T, S]): PrefixedCountMap[T, S] = {
    countPairs(map.toTraversable)
  }

  def countPairs[T, S](pairs: Traversable[(T, S)]): PrefixedCountMap[T, S] = {
    val emptyMap = Map[T, Map[S, Int]]().withDefault(_ => Map[S, Int]().withDefault(_ => 0))
    def updateCountMap(countMap: Map[T, Map[S, Int]], el1: T, el2: S) = {
      val subCountMap = countMap(el1)
      countMap.updated(el1, subCountMap.updated(el2, subCountMap(el2) + 1))
    }
    pairs.foldLeft(emptyMap){case (countMap, (el1, el2)) => updateCountMap(countMap, el1, el2)}
  }
}
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
 * An integer index of objects of type T
 */
trait Index[T] {

  def isIndexed(term: T): Boolean

  def apply(term: T): Int

  def add(term: T): Index[T]

  def indexAndGet(term: T): Int = add(term).apply(term)
}

object Index {

  def apply[T]: Index[T] = new MapIndex[T]
}
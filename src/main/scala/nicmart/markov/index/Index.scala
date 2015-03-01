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
trait Index[From, To] {

  def isIndexed(term: From): Boolean

  def apply(term: From): To

  def add(term: From): Index[From, To]

  def indexAndGet(term: From): To = add(term).apply(term)

  def size: Int

  def indexExists(index: To): Boolean

  def invert(index: To): From
}

object Index {

  def apply[From, To <: Int](implicit evidence: To =:= Int) = new MapIndex[From, Int] {
    protected def nextAvailableIndex: Int = size
  }

  def apply[From, To <: Short](implicit evidence: To =:= Short, a: DummyImplicit) = new MapIndex[From, Short] {
    protected def nextAvailableIndex: Short = size.toShort
  }
}
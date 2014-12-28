/*
 * (c) 2014 Nicolò Martini
 *
 * http://nicolo.martini.io
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

package nicmart.markov

import scala.collection.generic.FilterMonadic

/**
 * A Markov Chain that output something between transitions
 */
trait MarkovChain[State, T, CollectionType[_] <: Iterable[_]] {
  def next(from: State): (State, CollectionType[T])

  def stream(from: State): Stream[(State, CollectionType[T])] = {
    val (state, output) = next(from)
    (state, output) #:: stream(state)
  }

  //wrong type...
  def t(c: CollectionType[T]) = c.toStream

  def outputStream(from: State): Stream[CollectionType[T]] = stream(from).map(_._2)

  //def flattenOutputStream(from: State): Stream[T] = stream(from).map(_._2.toStream).flatten
}

class Test extends MarkovChain[String, String, Seq] {
  override def next(from: String): (String, Seq[String]) = (from, List())
}
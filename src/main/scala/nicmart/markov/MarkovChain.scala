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
trait MarkovChain[State, T, TCollection <: Iterable[T]] {
  def next(from: State): Option[(State, TCollection)]

  def stream(from: State): Stream[(State, TCollection)] = next(from) match {
    case None => Stream()
    case Some((state, output)) => (state, output) #:: stream(state)
  }

  def outputStream(from: State): Stream[TCollection] = stream(from).map(_._2)

  def flattenOutputStream(from: State): Stream[T] = stream(from).flatMap(_._2)
}
/*
 * (c) 2014 Nicolò Martini
 *
 * http://nicolo.martini.io
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

package nicmart.markov

import scala.annotation.tailrec

/**
 * A (possibily infinite) state automaton
 */
trait StateAutomaton[State, Input] {

  def transition(from: State, symbol: Input): State
  
  def transition(from: State, symbols: Traversable[Input]): State = {
    if (symbols.isEmpty) {
      from
    } else {
      transition(transition(from, symbols.head), symbols.drop(1))
    }
  }

  final def statesStream(from: State, symbols: Traversable[Input]): Stream[State] = {
    if (symbols.isEmpty) {
      Stream(from)
    } else {
      from #:: statesStream(transition(from, symbols.head), symbols.drop(1))
    }
  }

  final def statesAndInputStream(from: State, distribution: PartialFunction[State, Input]): Stream[(Input, State)] = {
    if (!distribution.isDefinedAt(from)) {
      Stream()
    } else {
      val input = distribution(from)
      val nextState = transition(from, input)
      (input, nextState) #:: statesAndInputStream(nextState, distribution)
    }
  }
}

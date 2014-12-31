/*
 * (c) 2014 Nicolò Martini
 *
 * http://nicolo.martini.io
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

package nicmart.markov

/**
 * A symbol-window based automaton
 */
class SymbolStringAutomaton[T](keyLength: Int) extends StateAutomaton[Traversable[T], Traversable[T]] {
  type Input = Traversable[T]
  type State = Traversable[T]

  def transition(from: State, symbol: Input): State = (from ++ symbol).toSeq.takeRight(keyLength)
}

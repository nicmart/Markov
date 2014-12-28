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
 * Trait Description
 */
trait InputDistribution[State, Input] extends PartialFunction[State, Input] {
  def apply(from: State): Input
}

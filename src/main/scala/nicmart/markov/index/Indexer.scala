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
 * Transform collections to indexes
 */
trait Indexer[From, To] extends (Traversable[From] => Index[From, To]) {

  def apply(collection: Traversable[From]): Index[From, To] = index(collection)

  def index(collection: Traversable[From]): Index[From, To]

  def indexAndMap(collection: Traversable[From]): (Index[From, To], Traversable[To])
}

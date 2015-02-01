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
trait Indexer[T] extends (Traversable[T] => Index[T]) {

  def apply(collection: Traversable[T]): Index[T] = index(collection)

  def index(collection: Traversable[T]): Index[T]

  def indexAndMap(collection: Traversable[T]): (Index[T], Traversable[Int])
}

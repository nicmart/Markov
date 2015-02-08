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
 * A basic implementation of an indexer
 */
class BasicIndexer[T](indexFactory: => Index[T] = new MapIndex[T]) extends Indexer[T] {

  def index(collection: Traversable[T]): Index[T] = {

    val index = indexFactory

    for (term <- collection) {
      index.add(term)
    }

    index
  }

  def indexAndMap(collection: Traversable[T]): (Index[T], Traversable[Int]) = {

    val index = indexFactory

    val transformedCollection = collection map { index.indexAndGet(_) }

    (index, transformedCollection)
  }
}

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
class BasicIndexer[From, To](indexFactory: => Index[From, To] = Index[From, Int]) extends Indexer[From, To] {

  def index(collection: Traversable[From]): Index[From, To] = {

    val index = indexFactory

    for (term <- collection) {
      index.add(term)
    }

    index
  }

  def indexAndMap(collection: Traversable[From]): (Index[From, To], Traversable[To]) = {

    val index = indexFactory

    val transformedCollection = collection map { index.indexAndGet(_) }

    (index, transformedCollection)
  }
}

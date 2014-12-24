/*
 * (c) 2014 Nicolò Martini
 *
 * http://nicolo.martini.io
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

package nicmart.markov

import nicmart.{WeightedRandomDistribution, WeightedValue}
import nicmart.markov.Helpers._

import scala.collection.Map
import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.util.Random

/**
 * Class Description
 */
class MarkovEngine[SourceType, TokenType]
    (source: SourceType, windowSize: Int)
    (implicit tokenExtractor: TokenExtractor[SourceType, TokenType], keyBuilder: KeyBuilder[TokenType, String]) {

  type MarkovMap = Map[String, WeightedRandomDistribution[TokenType]]
  type SmallerIndexes = mutable.Map[Int, mutable.Map[String, IndexedSeq[String]]]

  private val tokens: Seq[TokenType] = tokenExtractor(source)
  private val markovMap: MarkovMap = markovMap(counter(tokens))

  def stream(prefix: Traversable[TokenType]): Stream[TokenType] = {
    val prefixStream = prefix.toStream

    lazy val stream: Stream[TokenType] = prefixStream #::: stream.slidingStream(prefixStream.length).map { window =>
      val key = keyBuilder(window.toSeq)
      if (markovMap.isDefinedAt(key)) markovMap(key)()
      else markovMap(keyBuilder(prefixStream))()
    }

    stream
  }

  def stream(prefix: SourceType): Stream[TokenType] = {
    stream(randomPrefixFromString(prefix))
  }

  def stream: Stream[TokenType] = stream(randomStartKeys)

  private def randomPrefixFromString(prefix: SourceType) = {
    randomPrefixFromTokens(tokenExtractor(prefix))
  }

  private def isPrefix[T](a: Seq[T], b: Seq[T]) = {
    a.zip(b) forall { case (x,y) => x == y }
  }

  private def randomPrefixFromTokens(prefix: Traversable[TokenType]) = {
    val candidates: Seq[Seq[TokenType]] = tokens.sliding(windowSize - 1).filter(isPrefix(prefix.toSeq, _)).toSeq
    val length = candidates.length

    if (length == 0) randomStartKeys else {
      candidates(Random.nextInt(length))
    }
  }

  private def counter(tokens: Seq[TokenType]): OccurrenciesCounter[String, TokenType] = {
    val letterCounter = new OccurrenciesCounter[String, TokenType]

    for (window <- tokens.sliding(windowSize)) {
      //println(window.take(windowSize - 1).mkString(" "))
      val keys: Seq[TokenType] = window.take(windowSize - 1)
      letterCounter.addElement(keyBuilder(keys), window.last)
    }

    letterCounter
  }

  private def markovMap(counter: OccurrenciesCounter[String, TokenType]): Map[String, WeightedRandomDistribution[TokenType]] = {
    counter.getIndexes.mapValues(map => {
      val values = map.map(pair => WeightedValue(pair._1, pair._2))
      new WeightedRandomDistribution(values)
    })
  }

  private def randomStartKeys = {
    val index = Random.nextInt(tokens.length - windowSize)
    tokens.slice(index, index + windowSize - 1)
  }
}

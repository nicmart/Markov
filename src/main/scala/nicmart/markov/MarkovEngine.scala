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
import scala.collection.mutable
import scala.util.Random

/**
 * Class Description
 */
class MarkovEngine[SourceType, TokenType]
    (source: SourceType, windowSize: Int, indexTypes: Seq[IndexType])
    (implicit tokenExtractor: TokenExtractor[SourceType, TokenType], keyBuilder: KeyBuilder[TokenType, String]) {

  type MarkovMap = Map[String, WeightedRandomDistribution[Seq[TokenType]]]
  type SmallerIndexes = mutable.Map[Int, mutable.Map[String, IndexedSeq[String]]]

  private val tokens: Seq[TokenType] = tokenExtractor(source)
  private val markovMaps: Map[IndexType, MarkovMap] = indicize
  private val defaultIndexType = indexTypes(0)


  /**
   * Build the stream given a prefix
   */
  def stream(prefix: Traversable[TokenType], indexType: IndexType) = {
    val markovMap: MarkovMap = markovMaps.getOrElse(indexType, throw new NoSuchElementException)
    val prefixStream = prefix.toStream
    val slidingStep = indexType.valueLength

    lazy val stream: Stream[TokenType] = prefixStream #::: stream.slidingStream(prefixStream.length, slidingStep).flatMap { window =>
      val key = keyBuilder(window.toSeq)

      val next = {
        if (markovMap.isDefinedAt(key)) markovMap(key)()
        else {
          println(s"---missed key---: ${key}")
          markovMap(keyBuilder(prefixStream))()
        }
      }
      println(s"${key} --> ${next.mkString}")
      next
    }

    stream
  }

  /**
   * Build a stream from a prefix of the same type of the source
   */
  def stream(prefix: SourceType, indexType: IndexType): Stream[TokenType] = {
    stream(generateStartSequence(prefix, indexType), indexType)
  }

  def generateStartSequence(prefix: SourceType, indexType: IndexType): Seq[TokenType] = {
    generateStartSequence(tokenExtractor(prefix), indexType)
  }

  def generateStartSequence(prefix: Traversable[TokenType], indexType: IndexType): Seq[TokenType] = {
    val candidates: Seq[Seq[TokenType]] =
      tokens.sliding(indexType.keyLength).filter(isPrefix(prefix.toSeq, _)).toSeq
    val length = candidates.length

    if (length == 0) generateStartSequence(indexType) else {
      candidates(Random.nextInt(length))
    }
  }

  def generateStartSequence(indexType: IndexType): Seq[TokenType] = {
    val index = Random.nextInt(tokens.length - windowSize)
    tokens.slice(index, index + indexType.keyLength)
  }

  private def isPrefix(a: Seq[TokenType], b: Seq[TokenType]) = {
    keyBuilder(a) == keyBuilder(b.take(a.length))
  }

  /**
   * Build the index of markov maps
   */
  private def indicize: Map[IndexType, MarkovMap] = {
    val counters = mutable.Map[IndexType, OccurrenciesCounter[String, Seq[TokenType]]]()

    indexTypes foreach {
      indexType => counters(indexType) = new OccurrenciesCounter[String, Seq[TokenType]]
    }

    // Build counters
    for (
        window <- tokens.sliding(windowSize);
        (indexType, counter) <- counters;
        (keys, values) = indexType.keysAndValues(window)
    ) {
      counter.addElement(keyBuilder(keys), values)
    }

    // Build distributions
    counters mapValues (markovMapFromCounter(_))
  }

  private def markovMapFromCounter(
      counter: OccurrenciesCounter[String, Seq[TokenType]]
    ): Map[String, WeightedRandomDistribution[Seq[TokenType]]] = {
    counter.getIndexes.mapValues(map => {
      val values = map.map{case (value, weight) => WeightedValue(value, weight)}
      new WeightedRandomDistribution(values)
    })
  }

  private def getKeyForCounter(keys: Seq[TokenType], counter: OccurrenciesCounter[String, Seq[TokenType]]) = {
    val stringKey = keyBuilder(keys)
  }
}

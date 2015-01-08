/*
 * (c) 2014 Nicolò Martini
 *
 * http://nicolo.martini.io
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

package nicmart.markov

import nicmart.markov.IndexType.Forward
import nicmart.{WeightedRandomDistribution, WeightedValue}
import nicmart.markov.Helpers._

import scala.util.Random

/**
 * Class Description
 *
 * @todo
 *       1. Pass only the MAX indexType size to the engine
 *       2. Index for all the types from 1 to the MAX
 *       3. Define only ONE automaton with keyLength = MAX
 *       4. The state has so to be intended as a window of MAX length
 *       5. The dist function passed to the chain has to select the index type for which the given input
 *          has the closest entropy to the given one (0.1 should be a good fit)
 */
class MarkovEngine[SourceType, TokenType]
    (source: SourceType, maxStateSize: Int, inputSize: Int)
    (implicit tokenExtractor: TokenExtractor[SourceType, TokenType], keyBuilder: KeyBuilder[TokenType, String]) {

  type Distribution = (IndexType, State) => Option[Input]
  type State = Traversable[TokenType]
  type Input = Traversable[TokenType]
  type IndexedState = String

  /**
   * Index Types: forward and backward from 1 to the maxStateSize
   */
  val indexTypes: Seq[IndexType] = (1 to maxStateSize)
    .map(IndexType(_, inputSize, Forward))
    .flatMap(indexType => List(indexType, indexType.opposite))

  /**
   * For each IndexType, define the state automaton
   */
  private val automatons: IndexType => StateAutomaton[State, State] = indexTypes.map{
    indexType => (
      indexType,
      new SymbolStringAutomaton[TokenType](indexType.keyLength)
    )
  }.toMap


  private val maxWindowSize = maxStateSize + inputSize
  private val tokens: Seq[TokenType] = tokenExtractor(source)
  private val distributionMapsWithEntropy = distributionsMap
  private val distributions: Distribution = distributionFromMap(distributionMapsWithEntropy)

  private val chains: IndexType => DefaultStateAutomatonChain[State, Input] = indexTypes.map{
    indexType => (
      indexType,
      DefaultStateAutomatonChain[State, Input](
        automatons(indexType),
        (s: State) => distributions(indexType, s)
      )
    )
  }.toMap


  private val defaultIndexType = indexTypes(0)

  /**
   * The Markov chain stream
   */
  def stream(prefix: Traversable[TokenType], indexType: IndexType): Stream[TokenType] = {
    val from: State = prefix.take(indexType.keyLength)
    val outputStream: Stream[Input] = chains(indexType).flattenOutputStream(from)
    prefix.toStream ++ outputStream.flatten
  }

  /**
   * Build the stream of sentences
   */
  def sentenceStream(
    startSequence: Input,
    indexType: IndexType,
    separator: TokenType
  ): Stream[Stream[TokenType]] = {

    val forwardStream: Stream[TokenType] = stream(startSequence, indexType)
    val backwardStream: Stream[TokenType] = stream(startSequence.toSeq.reverse, indexType.opposite)

    val prefixStream = backwardStream.takeUntil(separator, 1, false).take(10000).dropRight(1)
    val markovStream = prefixStream.reverse #::: forwardStream.drop(startSequence.length)

    markovStream.sentenceStream(separator)
  }

  def startSequenceGenerator(
    prefix: Traversable[TokenType],
    indexType: IndexType
  ): () => Seq[TokenType] = {
    val candidates: Seq[Seq[TokenType]] =
      tokens.sliding(indexType.keyLength).filter(isPrefix(prefix.toSeq, _)).toSeq
    val length = candidates.length

    () => {
      if (length == 0) {
        val index = Random.nextInt(tokens.length - maxWindowSize)
        tokens.slice(index, index + indexType.keyLength)
      } else {
        candidates(Random.nextInt(length))
      }
    }
  }

  def startSequenceGenerator(prefix: SourceType, indexType: IndexType): () => Seq[TokenType] = {
    startSequenceGenerator(tokenExtractor(prefix), indexType)
  }

  private def isPrefix(a: Seq[TokenType], b: Seq[TokenType]) = {
    keyBuilder(a) == keyBuilder(b.take(a.length))
  }

  /**
   * Build the index of markov maps
   */
  private def distributionsMap: Map[(IndexType, IndexedState), (Double, WeightedRandomDistribution[Input])] = {
    // Build counter maps
    val elementsToCount = for (
      window <- tokens.sliding(maxWindowSize);
      indexType <- indexTypes;
      (keys, values) = indexType.keysAndValues(window)
    ) yield ((indexType, keyBuilder(keys)), values)

    val countMaps = Counter.countPairs[(IndexType, String), Input](elementsToCount.toTraversable)

    println("-" * 40)
    println("Entropy Stats")
    countMaps.reindexBy(_._1).foreach{
      case (indexType, map) => {
        map.reindexBy(_._2.matches(".*\\p{P}.*")).foreach{
          case (withPunctuation, innerMap) => {
            val dotString = if (withPunctuation) "with punct." else "without punct."
            val (mean, deviation) = (new ChainStats(innerMap)).entropyMeanAndDeviation
            println(s"${indexType.toString} (${dotString}). mean: ${mean}, deviation: ${deviation}")
          }
        }
      }
    }

    val (mean, deviation) = (new ChainStats(countMaps)).entropyMeanAndDeviation

    println("Global Entropy: " + mean + "/" + deviation)
    println("-" * 40)

    countMaps.mapValues{ countMap: Map[Input, Int] => (
      countMap.entropy,
      new WeightedRandomDistribution(countMap.map{case (value, weight) => WeightedValue(value, weight)})
    )}
  }

  private def distributionFromMap(
      randomDistribs: Map[(IndexType, IndexedState), (Double, WeightedRandomDistribution[Input])]
    ): Distribution = {
    (indexType: IndexType, from: State) => {
      val key = keyBuilder(from)
      if (randomDistribs.isDefinedAt((indexType, key))) Some(randomDistribs((indexType, key))._2()) else None
    }
  }
}

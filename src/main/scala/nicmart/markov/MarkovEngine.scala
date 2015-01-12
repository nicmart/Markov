/*
 * (c) 2014 Nicolò Martini
 *
 * http://nicolo.martini.io
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

package nicmart.markov

import nicmart.markov.IndexType.{Backward, Direction, Forward}
import nicmart.{WeightedRandomDistribution, WeightedValue}
import nicmart.markov.Helpers._

import scala.util.Random

/**
 * Class Description
 *
 * @todo
 *       1. Pass only the MAX indexType size to the engine DONE
 *       2. Index for all the types from 1 to the MAX DONE
 *       3. Define only ONE automaton with keyLength = MAX DONE
 *       4. The state has so to be intended as a window of MAX length DONE
 *       5. The dist function passed to the chain has to select the index type for which the given input
 *          has the closest entropy to the given one (0.1 should be a good fit)
 */
class MarkovEngine[SourceType, TokenType]
    (source: SourceType, windowSize: Int, exponentialEntropy: Double)
    (implicit tokenExtractor: TokenExtractor[SourceType, TokenType], keyBuilder: KeyBuilder[TokenType, String]) {

  type Distribution = (IndexType, State) => Option[Input]
  type State = Traversable[TokenType]
  type Input = Traversable[TokenType]
  type IndexedState = String

  /**
   * Index Types: forward and backward from 1 to the maxStateSize
   */
  val indexTypes: Seq[IndexType] = IndexType(windowSize, 1, Forward) +:
    (1 to windowSize - 1)
      .map(keySize => IndexType(keySize, windowSize - keySize, Forward))
      .flatMap(indexType => List(indexType, indexType.opposite))

  private val stateSize = windowSize
  private val automaton: StateAutomaton[State, State] = new SymbolStringAutomaton[TokenType](stateSize)
  private val tokens: Seq[TokenType] = tokenExtractor(source)
  private val distributionMapsWithEntropy: Map[
      (IndexType, IndexedState), (Double, WeightedRandomDistribution[Input])
    ] = distributionsMap

  private val chains: Direction => DefaultStateAutomatonChain[State, Input] = Map(
    Forward -> DefaultStateAutomatonChain[State, Input](automaton, distribution(_, Forward)),
    Backward -> DefaultStateAutomatonChain[State, Input](automaton, distribution(_, Backward))
  )

  private val defaultIndexType = indexTypes(0)

  /**
   * The Markov chain stream
   */
  def stream(prefix: Traversable[TokenType], direction: Direction): Stream[TokenType] = {
    val outputStream: Stream[Input] = chains(direction).flattenOutputStream(prefix)
    prefix.toStream ++ outputStream.flatten
  }

  /**
   * Build the stream of sentences
   */
  def sentenceStream(
    startSequence: Input,
    direction: Direction,
    separator: TokenType
  ): Stream[Stream[TokenType]] = {

    val forwardStream: Stream[TokenType] = stream(startSequence, direction)
    val backwardStream: Stream[TokenType] = stream(startSequence.toSeq.reverse, direction.opposite)

    val prefixStream = backwardStream.takeUntil(separator, 1, false).take(10000).dropRight(1)
    val markovStream = prefixStream.reverse  #::: forwardStream.drop(startSequence.size)

    markovStream.sentenceStream(separator)
  }

  def startSequenceGenerator(prefix: Traversable[TokenType]): () => Seq[TokenType] = {
    val candidates: Seq[Seq[TokenType]] =
      tokens.sliding(windowSize).filter(isPrefix(prefix.toSeq, _)).toSeq
    val length = candidates.length

    () => {
      if (length == 0) {
        val index = Random.nextInt(tokens.length - windowSize + 1)
        tokens.slice(index, index + windowSize)
      } else {
        candidates(Random.nextInt(length))
      }
    }
  }

  def startSequenceGenerator(prefix: SourceType): () => Seq[TokenType] = {
    startSequenceGenerator(tokenExtractor(prefix))
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
      window <- tokens.sliding(windowSize);
      indexType <- indexTypes;
      (keys, values) = indexType.keysAndValues(window)
    ) yield {
      //println(s"$indexType ${keyBuilder(keys)} --> ${values.mkString(" ")}")
      ((indexType, keyBuilder(keys)), values)
    }

    val countMaps = Counter.countPairs[(IndexType, String), Input](elementsToCount.toTraversable)

    println("-" * 80)
    println("Entropy Stats")
    countMaps.reindexBy(_._1).filter{case (IndexType(_,_, Forward), _) => true; case _ => false}.foreach{
      case (indexType, map) => {
        map.reindexBy(_._2.matches(".*\\p{P}.*")).foreach{
          case (withPunctuation, innerMap) => {
            val dotString = if (withPunctuation) "with punct." else "without punct."
            val (mean, deviation, entropies) = (new ChainStats(innerMap)).entropyMeanAndDeviation
            println(s"${indexType.toString} (${dotString}). mean: ${mean}, deviation: ${deviation}")
            println("Entropies (Top 10): ")
            val total = entropies.size
            val stats = entropies.groupBy(x => x)
              .map{case (entropy, list) => (entropy, list.size, math.pow(2, entropy))}
              .toList
              .sortBy{ case (_, count, _) => -count}
              .take(10)
            stats.foreach{case (entropy, count, exp) =>
              val ratio = (count.toDouble / total) * 100
              val percentage = "%.2f".format(ratio)
              println(s"${"%.3f".format(entropy)} (${percentage}% - ${count}) - exp: ${"%.3f".format(exp)}")
            }
            println("-" * 80)
          }
        }
      }
    }

    val (mean, deviation, _) = (new ChainStats(countMaps)).entropyMeanAndDeviation

    println("Global Entropy: " + mean + "/" + deviation)
    println("-" * 40)

    countMaps.mapValues{ countMap: Map[Input, Int] => (
      countMap.entropy,
      new WeightedRandomDistribution(countMap.map{case (value, weight) => WeightedValue(value, weight)})
    )}
  }

  /**
   * The distribution function
   * Still a bit a mess in order to print stats and debug info
   */
  private def distribution(state: State, direction: Direction): Option[Input] = {
    val distancesAndDists: Iterable[(IndexType, Double, WeightedRandomDistribution[Input])] = for (
      indexType <- indexTypes if indexType.direction == direction;
      key = keyBuilder(indexType.keys(state));
      (entropy, distMap) = distributionMapsWithEntropy((indexType, key))
    ) yield {
      val dist = math.abs(math.pow(2, entropy) - exponentialEntropy)
      //println(s"${indexType} --> Entropy: ${math.pow(2, entropy)}, expEntropy: $exponentialEntropy, dist: $dist")
      (indexType, dist, distMap)
    }

    //val min = distancesAndDists.minBy{ case (indexType, distance, _) => (distance, -indexType.keyLength) }

    def weightFormula(d: Double, k: Int = 1) = 100000 / (d + k)

    val weightedDists = distancesAndDists
      .map{case (indexType, distance, distMap) => WeightedValue((indexType, distMap), weightFormula(distance))}
    val randomDistOfDist = new WeightedRandomDistribution(weightedDists)
    val dist = randomDistOfDist()

    //Some(min._3())
    //println(dist._1)
    Some(dist._2())
  }
}

package nicmart.markov

import scala.collection.SeqView
import scala.util.Random

object Markov {
  type TokenType = String

  def main(args: Array[String]): Unit = {
    val limit = if (args.length > 0) args(0).toInt else 1000
    val windowSize = if (args.length > 1) args(1).toInt else 3
    val file = if (args.length > 2) args(2) else "mostrasprint"
    val counter = new OccurrenciesCounter[String, TokenType]

    val sourceString: String = scala.io.Source.fromFile(file)
      .getLines().mkString(" ")

    val tokens = getTokens[TokenType](sourceString)

    val randomMap = getDistribution(tokens, windowSize)

    val firstElements = startKeys(tokens, windowSize)

    val stream = getMarkovStream(tokens, firstElements, windowSize)

    val punctuation = Set(".", "!", "?")

    for (token <- stream.take(limit)) {
        print(token)
        print(if (punctuation.contains(token)) "\r\n>>> " else " ")
    }
    println("")
    println("---------------------")
  }

  def getDistribution[T](tokens: Seq[T], windowSize: Int)(implicit keyBuilder: KeyBuilder[T, String]) = {
    val letterCounter = new OccurrenciesCounter[String, T]

    for (window <- tokens.sliding(windowSize)) {
      //println(window.take(windowSize - 1).mkString(" "))
      letterCounter.addElement(keyBuilder(window.take(windowSize - 1)), window.last)
    }

    println("")
    println("---------------------")
    for ((i, v) <- letterCounter.getIndexes.take(10)) {
      println(i.mkString(" "))
      println(v)
    }

    letterCounter.getDistributionMap
  }

  def getTokens[T](source: String)(implicit extractor: TokenExtractor[String, T]) = {
    extractor(source)
  }

  def startKeys[T](tokens: Seq[T], windowSize: Int) = {
    val index = Random.nextInt(tokens.length - windowSize)
    tokens.slice(index, index + windowSize - 1)
  }

  def getMarkovStream[T](tokens: Seq[T], start: Seq[T], windowSize: Int)(implicit keyBuilder: KeyBuilder[T, String]) = {
    val distribution = getDistribution(tokens, windowSize)
    val prefix = start.toStream #::: Stream(distribution(keyBuilder(start))())
    lazy val stream: Stream[T] = start.toStream #::: getSlidingStream(stream, start.length).map { window =>
      distribution(keyBuilder(window.toSeq))()
    }
    stream
  }

  def getSlidingStream[T](source: Stream[T], size: Int): Stream[Stream[T]] = {
    val window = source.take(size)
    if (window.length < size) Stream()
    else source.take(size) #:: getSlidingStream(source.drop(1), size)
  }
}

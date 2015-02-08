package nicmart.markov

import nicmart.markov.index.{InvertibleIndex, BasicIndexer}
import nicmart.markov.request._
import Helpers._
import IndexType._
import com.gravity.goose.Goose

object Markov {
  type TokenType = String

  val tokenExtractor = TokenExtractor.stringTokenExtractor

  def main(args: Array[String]): Unit = {
    val request = Request(args)

    val sourceString = request.source.split("\\|").map(getSource(_)).mkString("\n\n")

    val tokens: Seq[String] = tokenExtractor(sourceString)
    val indexer = new BasicIndexer[TokenType](InvertibleIndex[TokenType])
    val (intIndex, intTokens) = indexer.indexAndMap(tokens)

    println(intTokens.take(100))
    println(s"Number of Tokens: ${intIndex.size}")

    val indexType = IndexType(request.windowSize - 1, 1, Forward)

    val engine = new MarkovEngine[TokenType](tokens, request.windowSize, request.exponentialEntropy)
    val startSequenceGenerator = engine.startSequenceGenerator(tokenExtractor(request.prefix))

    val renderer = (new PunctuationWordStreamRenderer[TokenType])
      .andThen(CapitalizeAfterDot)
      .andThen(NewLineDecorator)

    val sentencesStream: Stream[Stream[String]] = (if (request.samePrefixPerSentence) Helpers.inifiniteStream {
      val prefix = startSequenceGenerator()
      //println("Prefix: " + prefix)
      engine.sentenceStream(prefix, Forward, ".").take(1)
    } else {
      engine.sentenceStream(startSequenceGenerator(), Forward, ".")
    }).map(renderer(_))

    // Input stream. I add an element on the head to always print the first sentence
    val linesStream = "" #:: io.Source.stdin.getLines.takeWhile(_ != "quit").toStream

    val sentencesAndInput: Stream[(Stream[String], String)] = sentencesStream.zip(linesStream)

    for ((sentence, _) <- sentencesAndInput) {
      println(sentence.mkString)
    }

    val mb = 1024*1024
    val runtime = Runtime.getRuntime
    println("** Used Memory:  " + (runtime.totalMemory - runtime.freeMemory) / mb)
    println("** Free Memory:  " + runtime.freeMemory / mb)
    println("** Total Memory: " + runtime.totalMemory / mb)
    println("** Max Memory:   " + runtime.maxMemory / mb)
  }

  def config = {
    val conf = new com.gravity.goose.Configuration
    conf.setEnableImageFetching(false)
    conf
  }

  def getPageText(uri: String) = {
    val goose = new Goose(config)
    val article = goose.extractContent(uri)
    article.cleanedArticleText
  }

  def getSource(argument: String): String =
    if (argument.startsWith("http://")) {
      getPageText(argument)
    } else {
      scala.io.Source.fromFile(argument)
        .getLines().mkString(" ")
    }
}

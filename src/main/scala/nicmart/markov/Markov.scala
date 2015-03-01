package nicmart.markov

import nicmart.markov.index.{Index, BasicIndexer}
import nicmart.markov.request._
import Helpers._
import IndexType._
import com.gravity.goose.Goose

object Markov {
  type Token = String
  type EncodedToken = Short

  val tokenExtractor = TokenExtractor.stringTokenExtractor

  def main(args: Array[String]): Unit = {
    val request = Request(args)

    val sourceString = request.source.split("\\|").map(getSource(_)).mkString("\n\n")

    val tokens: Seq[String] = tokenExtractor(sourceString)
    val indexer = new BasicIndexer[Token, EncodedToken](Index[Token, EncodedToken])
    val (intIndex, intTokens) = indexer.indexAndMap(tokens)
    val intTokenExtractor = tokenExtractor.map(intIndex(_))
    val intDot = intIndex(".")

    println(intTokens.take(100))
    println(s"Number of Tokens: ${intIndex.size}")

    val indexType = IndexType(request.windowSize - 1, 1, Forward)

    val engine = new MarkovEngine[EncodedToken](intTokens.toSeq, request.windowSize, request.exponentialEntropy)
    val startSequenceGenerator = engine.startSequenceGenerator(intTokenExtractor(request.prefix))

    val renderer = IndexBasedRenderer(intIndex)
      .andThen(new PunctuationWordStreamRenderer[Token])
      .andThen(CapitalizeAfterDot)
      .andThen(NewLineDecorator)

    val sentencesStream: Stream[Stream[String]] = (if (request.samePrefixPerSentence) Helpers.inifiniteStream {
      val prefix = startSequenceGenerator()
      //println("Prefix: " + prefix)
      engine.sentenceStream(prefix, Forward, intDot).take(1)
    } else {
      engine.sentenceStream(startSequenceGenerator(), Forward, intDot)
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

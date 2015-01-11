package nicmart.markov

import Helpers._
import IndexType._
import com.gravity.goose.Goose

object Markov {
  type TokenType = String

  def main(args: Array[String]): Unit = {
    val arguments = getArg(args) _
    val windowSize = arguments(0, "3").toInt
    val exponentialEntropy = arguments(1, "1.4").toDouble
    val samePrefixPerSentence = arguments(2, "true").toBoolean
    val source = arguments(3, "mostrasprint")
    val prefix = arguments(4, "")

    val sourceString = source.split("\\|").map(getSource(_)).mkString("\n\n")
    val indexType = IndexType(windowSize - 1, 1, Forward)

    val engine = new MarkovEngine[String, TokenType](sourceString, windowSize, exponentialEntropy)
    val startSequenceGenerator = engine.startSequenceGenerator(prefix, indexType)

    val renderer = (new PunctuationWordStreamRenderer[TokenType])
      .andThen(CapitalizeAfterDot)
      .andThen(NewLineDecorator)

    val sentencesStream: Stream[Stream[String]] = (if (samePrefixPerSentence) Helpers.inifiniteStream {
      engine.sentenceStream(startSequenceGenerator(), indexType, ".").take(1)
    } else {
      engine.sentenceStream(startSequenceGenerator(), indexType, ".")
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

  def getArg(args: Array[String])(position: Int, default: String) = {
    if (args.isDefinedAt(position)) args(position) else default
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

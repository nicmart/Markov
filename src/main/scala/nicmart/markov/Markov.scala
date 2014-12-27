package nicmart.markov

import Helpers._
import IndexType._
import com.gravity.goose.Goose

object Markov {
  type TokenType = String

  def main(args: Array[String]): Unit = {
    val arguments = getArg(args) _
    val leftWindowSize = arguments(0, "2").toInt
    val rightWindowSize = arguments(1, "1").toInt
    val limit = arguments(2, "1000").toInt
    val source = arguments(3, "mostrasprint")
    val prefix = arguments(4, "")

    val sourceString = source.split("\\|").map(getSource(_)).mkString("\n\n")

    val indexType = IndexType(leftWindowSize, rightWindowSize, Forward)
    val indexTypes = List(indexType, indexType.opposite)

    val engine = new MarkovEngine[String, TokenType](sourceString, leftWindowSize + rightWindowSize, indexTypes)

    val startSequence = if (prefix == "") {
      engine.generateStartSequence(indexType)
    } else engine.generateStartSequence(prefix, indexType)

    println("StartSequence:")
    println(startSequence.mkString(" "))
    println("-" * 40)

    val markovStream = engine.stream(startSequence, indexType)
    val reverseMarkovStream = engine.stream(startSequence.reverse, indexType.opposite)

    val renderer = (new PunctuationWordStreamRenderer[TokenType])
      .andThen(CapitalizeAfterDot)
      .andThen(NewLineDecorator)

    val reversedStream = reverseMarkovStream.takeUntil(_ == ".", 1, false).take(10000).dropRight(1)
    val truncatedStream = markovStream.takeUntil(_ == ".", limit).take(10000)

    val finalStream = reversedStream.reverse #::: truncatedStream.drop(startSequence.length)

    println("-" * 40)
    val tokens = renderer(finalStream).map(_.toString).force

    println("-" * 40)
    println("Final String:")
    println(tokens.mkString)
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
    //Ok("hi")
  }

  def getSource(argument: String): String =
    if (argument.startsWith("http://")) {
      getPageText(argument)
    } else {
      scala.io.Source.fromFile(argument)
        .getLines().mkString(" ")
    }
}

package com.osinka.scalaby8

import util.parsing.combinator.RegexParsers

object TrivialParser extends RegexParsers {
  override def skipWhitespace = false

  val fontOpen = """\[font(=([a-z]+))?\]""".r
  val fontClose = """\[/font\]""".r

  lazy val nodes = rep(font | text)

  lazy val text =
    rep1(not(fontOpen|fontClose) ~> "(?s).".r) ^^ {
      chars => Text(chars.mkString)
    }

  lazy val font: Parser[Node] = {
    fontOpen ~ nodes <~ fontClose ^^ {
      case fontOpen(_, arg) ~ subnodes => Font(Option(arg), subnodes)
    }
  }

  def parse(in: String) =
    parseAll(nodes, in) match {
      case Success(tree, _) => Right(tree)
      case e: NoSuccess => Left(e.msg)
    }
}

class parserSpec extends CommonSpecs(TrivialParser) {
  import parser.parse

  describe("error markup") {
    it("results in error") {
      parse("t[/font]") must be('left)
      parse("[font]t") must be('left)
    }
  }
}

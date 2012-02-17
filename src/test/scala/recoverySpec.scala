package com.osinka.scalaby8

import util.parsing.combinator.RegexParsers

// extra node for AST
case class FailNode(reason: String, markup: String) extends Node

object RecoveringParser extends RegexParsers {
  override def skipWhitespace = false

  val allowedFontArgs = Set("bold", "italic")

  val fontOpen = """\[font(=([a-z]+))?\]""".r
  val fontClose = """\[/font\]""".r

  protected def recover(p: => Parser[Node]): Parser[Node] =
    Parser { in =>
      val r = p(in)
      lazy val markup = in.source.subSequence(in.offset, r.next.offset).toString
      r match {
        case Success(node: FailNode, next) =>
          Success(node.copy(markup = markup), next)
        case other =>
          other
      }
    }

  protected def failed(reason: String) = FailNode(reason, "")

  lazy val nodes = rep(node | missingOpen)
  lazy val node = font | text | missingClose

  lazy val text =
    rep1(not(fontOpen|fontClose) ~> "(?s).".r) ^^ {
      texts => Text(texts.mkString)
    }

  lazy val font: Parser[Node] = recover {
    fontOpen ~ rep(node) <~ fontClose ^^ {
      case fontOpen(_, arg) ~ subnodes =>
        if (arg == null || allowedFontArgs.contains(arg)) Font(Option(arg), subnodes)
        else failed("arg incorrect")
    }
  }

  def missingOpen = recover {
    fontClose ^^^ { failed("missing open") }
  }

  def missingClose = recover {
    fontOpen ^^^ { failed("missing close") }
  }

  def parse(in: String) =
    parseAll(nodes, in) match {
      case Success(tree, _) => Right(tree)
      case e: NoSuccess => Left(e.msg)
    }
}

class recoverySpec extends CommonSpecs(RecoveringParser) {
  import parser.parse

  describe("recovery") {
    it("reports incorrect arg") {
      parse("[font=b]t[/font]") must equal(Right(
        FailNode("arg incorrect", "[font=b]t[/font]") :: Nil
      ))
    }
    it("recovers extra ending tag") {
      parse("t[/font]") must equal(Right(
        Text("t") :: FailNode("missing open", "[/font]") :: Nil
      ))
    }
    it("recovers extra starting tag") {
      parse("[font]t") must equal(Right(
        FailNode("missing close", "[font]") :: Text("t") :: Nil
      ))
    }
    it("recovers extra starting tag in a longer sequence") {
      parse("[font][font]t[/font]") must equal(Right(
        FailNode("missing close", "[font]") :: Font(None, Text("t") :: Nil) :: Nil
      ))
    }
    it("recovers extra ending tag in a longer sequence") {
      parse("[font]t[/font][/font]") must equal(Right(
        Font(None, Text("t") :: Nil) :: FailNode("missing open", "[/font]") :: Nil
      ))
    }
  }
}

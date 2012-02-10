package com.osinka.scalaby8

import util.parsing.combinator.RegexParsers

// extra node for AST
case class FailNode(reason: String, markup: String) extends Node

object RecoveringParser extends RegexParsers {
  override def skipWhitespace = false

  val allowedFontArgs = Set("bold", "italic")

  val fontOpen = """\[font(=([a-z]+))?\]""".r
  val fontClose = """\[/font\]""".r

  protected def recover[T <: Node](p: => Parser[T]) =
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
      chars => Text(chars.mkString)
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

class recoverySpec extends CommonSpecs(RecoveringParser)
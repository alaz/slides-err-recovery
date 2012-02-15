package com.osinka.scalaby8

import org.scalatest.FunSpec
import org.scalatest.matchers.MustMatchers

abstract class CommonSpecs(val parser: { def parse(in:String): Either[String,List[Node]] }) extends FunSpec with MustMatchers {
  import parser.parse

  describe("parser") {
    it("keeps spaces") {
      parse("  ") must equal(Right(Text("  ") :: Nil))
      parse(" \n ") must equal(Right(Text(" \n ") :: Nil))
    }
    it("parses text") {
      parse("plain text") must equal(Right(Text("plain text") :: Nil))
    }
    it("parses bbcode-like text") {
      parse("plain [tag] [fonttext") must equal(Right(Text("plain [tag] [fonttext") :: Nil))
    }
    it("parses font w/o arg") {
      parse("[font]text[/font]") must equal(Right(Font(None, Text("text") :: Nil) :: Nil))
    }
    it("parses font w/ arg") {
      parse("[font=bold]text[/font]") must equal(Right(Font(Some("bold"), Text("text") :: Nil) :: Nil))
    }
    it("parses consequtive") {
      parse("t [font]t2[/font][font=bold]t3[/font]") must equal(Right(
        Text("t ") :: Font(None, Text("t2") :: Nil) :: Font(Some("bold"), Text("t3") :: Nil) :: Nil
      ))
    }
    it("parses nested") {
      parse("[font][font=bold]t[/font][/font]") must equal(Right(
        Font(None, Font(Some("bold"), Text("t") :: Nil) :: Nil) :: Nil
      ))
    }
  }
}

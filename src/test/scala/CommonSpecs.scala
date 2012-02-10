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
  }
}

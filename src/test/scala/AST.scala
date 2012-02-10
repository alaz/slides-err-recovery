package com.osinka.scalaby8

// NOT sealed - against the rules - intentionally!
trait Node

case class Text(text: String) extends Node
case class Font(arg: Option[String], subnodes: List[Node]) extends Node

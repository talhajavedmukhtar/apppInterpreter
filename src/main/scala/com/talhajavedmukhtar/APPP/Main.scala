package com.talhajavedmukhtar.APPP

import com.talhajavedmukhtar.APPP.apppLexer.tokenize
import com.talhajavedmukhtar.APPP.apppParser

object Main extends App{
  val myTokens = tokenize("var x: bool = tt; if x then print \"true\" else print \"false\" ")
  println(myTokens)

  val x = new id("var","int","x",null)
  val y = new id("const","int","y",2)
  val idMap = Map("x"->x,"y"->y)
  val myParser = new apppParser(idMap)
  val astTree = myParser.program(myTokens)
  println("AST_TREE:",astTree)
}

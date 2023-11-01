
package com.example
package ast

import com.example.CymbolParser.FileContext
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import org.antlr.v4.runtime.TokenStreamRewriter
import org.antlr.v4.runtime.CharStream


object ParserUtil:

  def parse(source: String) =
    doParse(CharStreams.fromString(source))

  def parseFile(filename: String) = 
    doParse(CharStreams.fromFileName(filename))

  def doParse(input: CharStream) =  
    val lexer = CymbolLexer(input)
    val tokens = new CommonTokenStream(lexer)
    val parser = CymbolParser(tokens)
    val tree = parser.file()
    (tree, tokens)
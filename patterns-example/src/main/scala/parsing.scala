
package com.example
package ast

import com.example.CymbolParser.FileContext
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream


object ParserUtil:
  def parse(filename: String): FileContext = 
    val input = CharStreams.fromFileName(filename);
    val lexer = CymbolLexer(input)
    val tokens = new CommonTokenStream(lexer)
    val parser = CymbolParser(tokens)
    val tree = parser.file()
    tree

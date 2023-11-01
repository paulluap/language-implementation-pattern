package com.example
package callgraph

import org.antlr.v4.runtime.misc.{OrderedHashSet, MultiMap}
import scala.jdk.CollectionConverters.*
import scala.collection.convert.AsScalaExtensions
import com.example.ast.ParserUtil
import com.example.CymbolParser.FunctionDeclContext
import com.example.CymbolParser.CallContext
import org.antlr.v4.runtime.tree.ParseTreeWalker

@main
def callgraph: Unit =
  val (tree, tokens) = ParserUtil.parse("src/main/resources/t.cymbol") 
  val walker = new ParseTreeWalker()
  val collector = new FunctionListener
  walker.walk(collector, tree)
  println(collector.graph.toDOT())


class Graph:
  val nodes = new OrderedHashSet[String]
  val edges = new MultiMap[String, String]
  def edge(source: String, target: String): Unit =
    edges.map(source, target)
  def toDOT(): String =
    s"""
    |digraph G {
    |  ranksep=.25;
    |  edge [arrowsize=.5]
    |  node [shape=circle, fontname="ArialNarrow",
    |        fontsize=12, fixedsize=true, height=.45];
    |  ${nodes.asScala.mkString("", ";", ";")}
    |  ${(edges.asScala.map: (k, v) =>
         v.asScala.map(str =>
           s"    ${k} -> ${str}"
         ).mkString(";\n")
        ).mkString(";\n")}
    |}
    """.stripMargin
  // nodes: Set[String] = Set.empty

class FunctionListener extends CymbolBaseListener:
  val graph = new Graph()
  var currentFunctionName : String  = null 
  override def enterFunctionDecl(x: FunctionDeclContext): Unit = 
    currentFunctionName = x.ID().getText()
    graph.nodes.add(currentFunctionName)
  override def exitCall(x: CallContext): Unit = 
    val funcName = x.ID().getText()
    graph.edge(currentFunctionName, funcName)

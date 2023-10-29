package com.example
package symbol.symboltable

import java.{util as ju}
import com.example.CymbolParser.FileContext
import org.antlr.v4.runtime.tree.ParseTreeProperty
import com.example.CymbolParser.FunctionDeclContext
import org.antlr.v4.runtime.ParserRuleContext
import com.example.CymbolParser.BlockContext
import com.example.CymbolParser.FormalParameterContext
import com.example.CymbolParser.VarDeclContext
import org.antlr.v4.runtime.tree.TerminalNode
import org.antlr.v4.runtime.Token
import com.example.CymbolParser.VarContext
import com.example.CymbolParser.CallContext
import com.example.ast.ParserUtil
import org.w3c.dom.traversal.TreeWalker
import org.antlr.v4.runtime.tree.ParseTreeWalker

trait Type:
  def getName(): String

sealed abstract class Symbol(name: String, `type`: Type):
  def this(name:String) = this(name, null)
  def getName(): String = this.name
  override def toString(): String = 
    s"<${name}> : ${`type`}"

trait Scope:
  def getScopeName(): String
  def getEnclosingScope(): Scope
  def define(sym: Symbol): Unit
  def resolve(name: String): Symbol


//use subclasses for symbol categories 
case class VariableSymbol(name: String, `type`: Type) extends Symbol(name, `type`)
case class BultinTypeSymbol(name: String) extends Symbol(name) with Type
object BultinTypeSymbol:
  val INT = BultinTypeSymbol("int")
  val FLOAT = BultinTypeSymbol("float")
  val CHAR = BultinTypeSymbol("char")
  val BOOL = BultinTypeSymbol("bool")
  val VOID = BultinTypeSymbol("void")

abstract class BaseScope(enclosingScope: Scope) extends Scope:
  val symbols = ju.LinkedHashMap[String,Symbol]()
  override def resolve(name: String): Symbol = 
    val s = symbols.get(name) 
    if s != null then 
      s
    else if enclosingScope != null then 
      enclosingScope.resolve(name)
    else 
      null
  override def define(sym: Symbol): Unit = 
    symbols.put(sym.getName(), sym)
  override def getEnclosingScope(): Scope = 
    enclosingScope
  override def toString(): String = 
    s"${getScopeName()} : ${symbols.keySet()}"


class GlobalScope(enclosingScope: Scope) extends BaseScope(enclosingScope):
  override def getScopeName(): String = "globals"

class LocalScope(parent: Scope) extends BaseScope(parent):
  override def getScopeName(): String = "locals"


class FunctionSymbol(name: String, retType: Type, enclosingScope: Scope) extends Symbol(name, retType) with Scope:
  val arguments = ju.LinkedHashMap[String, Symbol]()
  override def define(sym: Symbol): Unit = 
    arguments.put(sym.getName(), sym)
    //TODO sym.scope = this
  override def getEnclosingScope(): Scope = enclosingScope
  override def getScopeName(): String = name
  override def toString(): String = 
    s"function ${super.toString()} : ${arguments.values()}"
  override def resolve(name: String): Symbol = 
    val s = arguments.get(name)
    if s != null then 
      s
    else if enclosingScope != null then 
      enclosingScope.resolve(name)
    else 
      null


class SymbolTable extends Scope:
  val symbols = new ju.HashMap[String, Symbol]()
  define(BultinTypeSymbol("int"))
  define(BultinTypeSymbol("flat"))
  override def getScopeName(): String = "global"
  override def getEnclosingScope(): Scope = null
  override def define(sym: Symbol): Unit = 
    symbols.put(sym.getName(), sym)
  override def resolve(name: String): Symbol = 
    symbols.get(name)


//test our symbol table
class DefPhase extends CymbolBaseListener:
  /**
    * record scope at the followig site
    *   - function declaration
    *   - block
    * 
    * top level scope is stored in globals
    * for ref phase, we need to use the scope at the stored site (func decl and block)
    */
  val scopes = ParseTreeProperty[Scope]
  var currentScope: Scope = null
  var globals: Scope = null;
  override def enterFile(x: FileContext): Unit = 
    globals = GlobalScope(null)
    currentScope = globals
  override def exitFile(x: FileContext): Unit = 
    ;
  private def getType(tokenType: Int): Type =
    tokenType match
      case CymbolParser.K_FLOAT => BultinTypeSymbol.FLOAT
      case CymbolParser.K_INT => BultinTypeSymbol.INT
      case CymbolParser.K_VOID => BultinTypeSymbol.VOID
      case _ => sys.error(s"unknown type ${tokenType}")

  private def saveScope(x: ParserRuleContext, s: Scope): Unit = 
    // println(s"=> save scope ${s}")
    scopes.put(x, s)

  override def enterFunctionDecl(x: FunctionDeclContext): Unit = 
    val name = x.ID().getText()
    val tokenType = x.`type`().start.getType()
    val retType = getType(tokenType)
    val function = FunctionSymbol(name, retType, currentScope) //symbol as well as scope
    currentScope.define(function)
    saveScope(x, function)
    currentScope = function

  override def exitFunctionDecl(x: FunctionDeclContext): Unit = 
    currentScope = currentScope.getEnclosingScope()

  override def enterBlock(x: BlockContext): Unit = 
    currentScope = LocalScope(currentScope)
    saveScope(x, currentScope)

  //exit* methods are just like post order traversal of tree after alternative is visited
  override def exitBlock(x: BlockContext): Unit = 
    currentScope = currentScope.getEnclosingScope()

  override def exitFormalParameter(x: FormalParameterContext): Unit = 
    defineVar(x.`type`(), x.ID().getSymbol())

  override def exitVarDecl(x: VarDeclContext): Unit = 
    defineVar(x.`type`(), x.ID().getSymbol());

  private def defineVar( tpeCtx: CymbolParser.TypeContext , nameToken: Token): Unit = 
    val tpe = getType(tpeCtx.start.getType())
    val variableSymbol = VariableSymbol(nameToken.getText(), tpe)
    currentScope.define(variableSymbol)

    
class RefPhase(scopes: ParseTreeProperty[Scope], globals: Scope) extends CymbolBaseListener:
  var currentScope: Scope = null
  //use correct scope
  override def enterFile(x: FileContext): Unit = 
    currentScope = globals
  override def enterFunctionDecl(x: FunctionDeclContext): Unit = 
    currentScope = scopes.get(x)
  override def exitFunctionDecl(x: FunctionDeclContext): Unit = 
    currentScope = currentScope.getEnclosingScope()
  override def enterBlock(x: BlockContext): Unit = 
    currentScope = scopes.get(x)
  override def exitBlock(x: BlockContext): Unit = 
    currentScope = currentScope.getEnclosingScope()
  //resolve symbols 
  override def exitVar(x: VarContext): Unit = 
    val name = x.ID().getText()
    val varSymbol = currentScope.resolve(name)
    if varSymbol == null then
      println(s"err! no such variable : ${name}")
    if varSymbol.isInstanceOf[FunctionSymbol] then
      println(s"err! ${name} is not a variable but a function")
  override def exitCall(x: CallContext): Unit = 
    val name = x.ID().getText()
    val funSymbol = currentScope.resolve(name)
    if funSymbol == null then 
      println(s"no such funtion ${name}")
    if funSymbol.isInstanceOf[VariableSymbol] then
      println(s"err! ${name} is not a function but a variable")
  
@main
def testSymbolTable: Unit = 
  val parseTree = ParserUtil.parse("./src/main/resources/t.cymbol")
  val defPhase = DefPhase()
  val walker = ParseTreeWalker()
  walker.walk(defPhase, parseTree)
  val refPhase = RefPhase(defPhase.scopes, defPhase.globals)
  walker.walk(refPhase, parseTree)
  println("effect")

//TODO true, false literal, 
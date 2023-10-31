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
import com.example.CymbolParser.StructDeclContext
import java.{util => ju}
import scala.jdk.CollectionConverters.*
import com.example.CymbolParser.MemberAccessContext
import com.example.CymbolParser.TypeContext
import com.example.CymbolParser.StructMemberContext
import dotty.tools.dotc.semanticdb.SymbolInformation.Property
import com.example.CymbolParser.FieldDeclContext
import scala.runtime.stdLibPatches.language.deprecated.symbolLiterals
import com.google.protobuf.Struct
import dotty.tools.dotc.core.StdNames.str
import org.antlr.v4.runtime.tree.ParseTree
import com.example.CymbolParser.AddSubContext

trait Type:
  def getName(): String

// trait ParseTreeExtension:
//   def symbol: Symbol
//   def scope: Scope

sealed abstract class Symbol(name: String):
  //TODO why track scope in symbol ?
  var scope: Scope = null
  var `type`: Type = null
  def getName(): String = this.name
  override def toString(): String = 
    s"<${name}:${`type`}>"

trait Scope:
  def getScopeName(): String
  def getEnclosingScope(): Scope
  def define(sym: Symbol): Unit
  def resolve(name: String): Symbol


//use subclasses for symbol categories 
case class VariableSymbol(name: String) extends Symbol(name)
case class BultinTypeSymbol(name: String) extends Symbol(name) with Type:
  override def toString(): String = s"[$name]"
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

abstract class ScopedSymbol(name: String, enclosingScope: Scope) 
  extends Symbol(name) with Scope:

    override def resolve(name: String): Symbol = 
      val s = getMembers().get(name)
      if s != null then 
        s
      else if enclosingScope != null then
        enclosingScope.resolve(name)
      else
        null

    override def define(sym: Symbol): Unit = 
      getMembers().put(sym.getName(), sym)
      sym.scope = this

    override def getScopeName(): String = name
    override def getEnclosingScope(): Scope = enclosingScope

    def getMembers(): ju.Map[String, Symbol]



//TODO both structSymbol and FunctionSymbol should extend ScopedSymbol
class StructSymbol(name: String, parent: Scope) extends ScopedSymbol(name, parent) with Type:
  val fields = ju.LinkedHashMap[String, Symbol]
  override def getMembers(): ju.Map[String, Symbol] = fields
  /** for a.b, only look in fields to resolve b, not up scope tree */
  def resolveMember(name: String) : Symbol =
    fields.get(name)
  override def toString(): String = 
    s"struct ${name} :{ ${fields.keySet().asScala.mkString(",")} }"

class FunctionSymbol(name: String, enclosingScope: Scope) extends Symbol(name) with Scope:
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


class SymbolTable:
  val globals = GlobalScope(null)
  val builtInTypes = Array(
    BultinTypeSymbol.BOOL,
    BultinTypeSymbol.CHAR,
    BultinTypeSymbol.INT,
    BultinTypeSymbol.FLOAT,
    BultinTypeSymbol.VOID
  )
  initTypeSystem
  def initTypeSystem = 
    builtInTypes.foreach: t =>
      globals.define(t)

object SymbolTable:
  val _boolean = BultinTypeSymbol("booelean")

extension(x: ParseTree)(using scopes: ParseTreeProperty[Scope], symbols:  ParseTreeProperty[Symbol])
  def scope: Scope = scopes.get(x)
  def scope_=(s: Scope): Unit = scopes.put(x, s)
  def symbol: Symbol = symbols.get(x)
  def symbol_=(s: Symbol): Unit = symbols.put(x, s)


//test our symbol table
class DefPhase(val globals: GlobalScope) extends CymbolBaseListener:

  given scopes : ParseTreeProperty[Scope] = ParseTreeProperty[Scope] //also for to be resolved references in AST
  given symbols : ParseTreeProperty[Symbol] = ParseTreeProperty[Symbol] //store type at reference site, referenced symbol


  var currentScope: Scope = null
  /**
    * push a GlobalScope. def BuiltinTypeSymbol objects for int, float, void, ...(already done in SymbolTable)
    *
    */
  override def enterFile(x: FileContext): Unit = 
    currentScope = globals


  /**
    *  Set x's scope field to the current scope (the resolution phase needs it)
    *
    */
  override def enterVar(x: VarContext): Unit = 
    x.scope = currentScope
  
  /**
    * def C as a class Symbol object, sym, in the current scope.
    * and push it as the current scope .
    * 
    * Set sym.def to the class name's ID AST node (omitted)
    * 
    * Set that ID node's symbol to sym
    * 
    * set the scope field of C's super class AST node in the current scope (omitted)
    *
    */
  override def enterStructDecl(x: StructDeclContext): Unit = 
    // println(s"line ${x.ID().getSymbol().getLine()} : def struct ${x.ID().getText()}")
    val sym = StructSymbol(x.ID().getText(), currentScope)
    currentScope.define(sym)
    x.ID().symbol = sym
    x.scope = sym //push scope 1
    currentScope = sym //push scope 2

  override def exitStructDecl(x: StructDeclContext): Unit = 
    currentScope = currentScope.getEnclosingScope()


  /**
    * def x as a VariableSymbol object, sym, in the current scope.
    * this works for globals, class fields, parameters, and lcoals.
    * 
    * Set sym.def to x's ID AST node. (omitted)
    * 
    * Set that ID node's symbol to sym.
    * Set the scope field of x's type AST node to the current scope (handled in exitType)
    *
    */
  private def defVar(id: ParseTree, tpe: TypeContext): Unit = 
    val sym = VariableSymbol(id.getText()) //what's the difference from x.ID().getText()
    currentScope.define(sym)
    id.symbol = sym
    tpe.scope = currentScope

  override def exitVarDecl(x: VarDeclContext): Unit = defVar(x.ID(), x.`type`())
  override def exitFormalParameter(x: FormalParameterContext): Unit = defVar(x.ID(), x.`type`())
  override def exitFieldDecl(x: FieldDeclContext): Unit = defVar(x.ID(), x.`type`())

  /**
    * def f as a FunctionSymbol object, sym, in the current scope.
    * 
    * push it as the current scope
    * 
    * Set sym.def to the function name's ID AST node (omitted)
    * 
    * Set that ID node's symbol to sym
    * 
    * Set the scope field of f's return type AST node to the current scope 
    *
    */
  override def enterFunctionDecl(x: FunctionDeclContext): Unit = 
    val sym = FunctionSymbol(x.ID().getText(), currentScope) //symbol as well as scope
    currentScope.define(sym)
    x.scope = sym  //push scope 1
    currentScope = sym //push scope 2
    x.ID().symbol =  sym
    x.`type`().scope = currentScope

  override def exitFunctionDecl(x: FunctionDeclContext): Unit = 
    currentScope = currentScope.getEnclosingScope()

  /**
    * push a LocalScope as the new current scope
    *
    */
  override def enterBlock(x: BlockContext): Unit = 
    currentScope = LocalScope(currentScope) //push scope 1
    x.scope = currentScope //push scope 2

  //exit* methods are just like post order traversal of tree after alternative is visited
  override def exitBlock(x: BlockContext): Unit = 
    currentScope = currentScope.getEnclosingScope()


class RefPhase(globals: GlobalScope)(using scopes: ParseTreeProperty[Scope], symbols: ParseTreeProperty[Symbol]) extends CymbolBaseListener:

  //resovled symbols: ast node -> symbol, also use the passed in symbols

  var currentScope: Scope = null
  //use correct scope

  /**
    * scope stack management 
    * in the def phase, we push/pop scope for 
    *   - struc decl
    *   - block
    *   - function decl
    */ 
  override def enterFile(x: FileContext): Unit = 
    currentScope = globals
  override def enterFunctionDecl(x: FunctionDeclContext): Unit = 
    currentScope = scopes.get(x)

  /**
    * let t be the ID node for f's return type,
    * ref t, yielding sym
    *
    * set t.symbol to sym
    * set the type field of the MethodSymbol for f to sym
    * 
    * @param x
    */
  override def exitFunctionDecl(x: FunctionDeclContext): Unit = 
    val t = x.`type`()
    val sym = t.scope.resolve(x.`type`().getText())
    t.symbol = sym
    x.ID().symbol.`type` = sym.asInstanceOf[Type]
    currentScope = currentScope.getEnclosingScope()
  override def enterBlock(x: BlockContext): Unit = 
    currentScope = scopes.get(x)
  override def exitBlock(x: BlockContext): Unit = 
    currentScope = currentScope.getEnclosingScope()
  override def enterStructDecl(x: StructDeclContext): Unit = 
    currentScope = scopes.get(x)
  override def exitStructDecl(ctx: StructDeclContext): Unit = 
    currentScope = currentScope.getEnclosingScope()


  //resolve symbols, need post order ?

  //set type of the declar symbol to the type ref's resolved symbol (also must be type), 
  //does antlr3 use type cast ?
  /**
    * Variable declaration, 
    * 
    * Let t be the ID node for x's type, 
    * ref t, yielding sym
    * 
    * Set t.symbol to sym. 
    * 
    * Set x.symbol.type to sym; 
    * in other words, jump to the VariableSymbol for x via the AST node's symbol field and then set its type field to sym
    * 
    */

  private def resolveVarDecl(t: TypeContext, id: ParseTree): Unit = 
    val sym = t.scope.resolve(t.getText())
    if sym == null then
      println(s"ERROR! cannot resolve typeref ${t.ID().getText()} in context ${t.getParent().getText()}")
    t.symbol = sym
    id.symbol.`type` = sym.asInstanceOf[Type] //type cast ?

  override def exitVarDecl(x: VarDeclContext): Unit = resolveVarDecl(x.`type`(), x.ID())

  override def exitFieldDecl(x: FieldDeclContext): Unit = resolveVarDecl(x.`type`(), x.ID())

  override def exitFormalParameter(x: FormalParameterContext): Unit =  resolveVarDecl(x.`type`(), x.ID())



  /**
    * variable reference, ref x, yielding sym
    * set x.symbol to sym, 
    *
    * @param x
    */
  override def exitVar(x: VarContext): Unit = 
    val name = x.ID().getText()
    val sym = currentScope.resolve(name)
    x.symbol = sym
    // println(s"ref ${name} : ${varSymbol}")
    if sym == null then
      println(s"err! no such variable : ${name}")
    if sym.isInstanceOf[FunctionSymbol] then
      println(s"err! ${name} is not a variable but a function")

  override def exitCall(x: CallContext): Unit = 
    val name = x.ID().getText()
    val funSymbol = currentScope.resolve(name)
    if funSymbol == null then 
      println(s"no such funtion ${name}")
    if funSymbol.isInstanceOf[VariableSymbol] then
      println(s"err! ${name} is not a function but a variable")

  //a.b.c
  override def exitMemberAccess(x: MemberAccessContext): Unit = 
    //can we break the member access segments into subrules, so one enter* function handling one segment a time
    val qualifiers = x.ID().asScala.map(_.getText())
    val q1Def = currentScope.resolve(qualifiers.head)
    //from second on, reolsve Member in struct
    println("=> context: " + qualifiers.mkString("."))
    val finalSymbol = qualifiers.tail.foldLeft(q1Def): (prevDef, name) =>  
      val struct = prevDef.`type`.asInstanceOf[StructSymbol]
      val res = struct.resolveMember(name)
      println(s"${name} resolved to ${res} of ${res.`type`}")
      res

  
@main
def testSymbolTable: Unit = 
  val parseTree = ParserUtil.parse("./src/main/resources/tt.cymbol")
  val symtab = SymbolTable()
  val defPhase = DefPhase(symtab.globals)
  val walker = ParseTreeWalker()
  walker.walk(defPhase, parseTree)
  val refPhase = RefPhase(defPhase.globals)(using defPhase.scopes, defPhase.symbols)
  walker.walk(refPhase, parseTree)

//TODO method symbol type resolution
//TODO true, false literal, 
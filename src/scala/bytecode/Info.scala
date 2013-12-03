package scala.bytecode

trait Info[Node, AST <: ast.AST] {
  def cxt: Cxt
  def node: Node

  def tree: AST

  def modifiers: List[Symbol]

  def name: String
  def desc: String
  def verbose: String

  def is(modifier: Symbol): Boolean = modifiers contains modifier

  override def toString = verbose
}

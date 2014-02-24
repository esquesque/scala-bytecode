package scala.bytecode.test

import scala.bytecode.ast.MethodDecl

trait ASTCase extends Case[MethodDecl] {
  lazy val tree: scala.bytecode.ast.MethodDecl = method.tree
  def apply: Boolean = test(tree)
}

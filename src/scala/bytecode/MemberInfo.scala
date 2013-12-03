package scala.bytecode

trait MemberInfo[Node, AST <: ast.AST] extends Info[Node, AST] {
  def owner: ClassInfo
}

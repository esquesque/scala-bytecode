package scala.bytecode.test

package object loops {
  val cases: List[ASTCase] =
    while_loop ::
    while_and ::
    while_if ::
    while_while ::
    while_while_break ::
    Nil
}

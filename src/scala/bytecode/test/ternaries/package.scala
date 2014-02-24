package scala.bytecode.test

package object ternaries {
  val cases: List[ASTCase] =
    zero_stack ::
    nonzero_stack ::
    Nil
}

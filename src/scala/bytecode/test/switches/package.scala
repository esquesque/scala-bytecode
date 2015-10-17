package scala.bytecode.test

package object switches {
  val cases: List[ASTCase] =
    table_switch ::
    table_switch_no_exit ::
    Nil
}

package scala.bytecode.test

object test_switches extends Suite {
  import scala.bytecode._
  val transforms = CollapseTernaryExprs :: AnchorFloatingStmts :: Nil
  val cases = scala.bytecode.test.switches.cases
}

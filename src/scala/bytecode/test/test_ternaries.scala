package scala.bytecode.test

object test_ternaries extends Suite {
  import scala.bytecode._
  val transforms = CollapseTernaryExprs :: AnchorFloatingStmts :: Nil
  val cases = ternaries.cases
}

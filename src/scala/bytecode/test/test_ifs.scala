package scala.bytecode.test

object test_ifs extends Suite {
  import scala.bytecode._
  val transforms = CollapseTernaryExprs :: AnchorFloatingStmts :: Nil
  val cases = ifs.cases
}

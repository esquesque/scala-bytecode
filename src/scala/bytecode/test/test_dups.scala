package scala.bytecode.test

object test_dups extends Suite {
  import scala.bytecode._
  val transforms = CollapseStackManipulations :: AnchorFloatingStmts :: Nil
  val cases = dups.cases
}

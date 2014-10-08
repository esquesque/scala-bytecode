package scala.bytecode.test

object test_loops extends Suite {
  import scala.bytecode._
  val transforms = CollapseStackManipulations ::
		   CollapseTernaryExprs ::
		   AnchorFloatingStmts :: Nil
  val cases = loops.cases
}

package scala.bytecode.test

object test_all extends Suite {
  import scala.bytecode._
  val transforms = CollapseStackManipulations ::
		   CollapseTernaryExprs ::
		   AnchorFloatingStmts :: Nil
  val cases = dups.cases ++
	      ifs.cases ++
	      ternaries.cases ++
	      try_catches.cases
}

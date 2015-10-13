package scala.bytecode.test.loops

import scala.bytecode.asm._
import scala.bytecode.ast._

object while_if extends scala.bytecode.test.ASTCase {
  val maxStack = 1
  val maxLocals = 2
  val name = "while_if"
  val desc = "(ZZ)V"
  val insns = {
    val (lbl0, lbl1, lbl2) = (label(), label(), label())
    insnList(
      lbl0,
      iload(0),
      ifeq(lbl2),
      iload(1),
      ifeq(lbl1),
      invokestatic("foo", "bar", "()V"),
      lbl1,
      goto(lbl0),
      lbl2,
      vreturn())
  }

  val test: Test = {
    case Exec(Label(_) ::
	      While(True(_),
		    If(True(_), _) ::
		    Label(_) ::
		    Goto(_, _) :: Nil) ::
	      Label(_) ::
	      Return(_) ::
	      Nil) => true
    case tree => false
  }
}

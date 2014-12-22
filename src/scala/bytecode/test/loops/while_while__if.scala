package scala.bytecode.test.loops

import scala.bytecode.asm._
import scala.bytecode.ast._

object while_while__if extends scala.bytecode.test.ASTCase {
  val maxStack = 1
  val maxLocals = 3
  val name = "while_while__if"
  val desc = "(ZZZ)V"
  val insns = {
    val (lbl0, lbl1, lbl2, lbl3, lbl4) =
      (label(), label(), label(), label(), label())
    insnList(
      lbl0,
      iload(0),
      ifeq(lbl4),
      lbl1,
      iload(1),
      ifeq(lbl2),
      invokestatic("foo", "bar", "()V"),
      goto(lbl1),
      lbl2,
      iload(2),
      ifeq(lbl3),
      invokestatic("foo", "baz", "()V"),
      lbl3,
      goto(lbl0),
      lbl4,
      vreturn())
  }

  val test: Test = {
    case Exec(Label(_) ::
	      While(True(_),
		    Label(_) ::
		    While(True(_), _) ::
		    Label(_) ::
		    If(True(_), _) :: Nil) ::
	      Label(_) ::
	      Return(_) ::
	      Nil) => true
    case tree => false
  }
}

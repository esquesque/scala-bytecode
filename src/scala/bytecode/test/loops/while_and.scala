package scala.bytecode.test.loops

import scala.bytecode.asm._
import scala.bytecode.ast._

object while_and extends scala.bytecode.test.ASTCase {
  val maxStack = 1
  val maxLocals = 2
  val name = "while__&&_"
  val desc = "(ZZ)V"
  val insns = {
    val lbl0 = label()
    val lbl1 = label()
    insnList(
      lbl0,
      iload(0),
      ifeq(lbl1),
      iload(1),
      ifeq(lbl1),
      invokestatic("foo", "bar", "()V"),
      goto(lbl0),
      lbl1,
      vreturn())
  }

  val test: Test = {
    case Exec(Label(_) ::
	      While(And(True(_), True(_)), _) ::
	      Label(_) ::
	      Return(_) ::
	      Nil) => true
    case tree => false
  }
}

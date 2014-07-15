package scala.bytecode.test.ifs

import scala.bytecode.asm._
import scala.bytecode.ast._

object if_and_and extends scala.bytecode.test.ASTCase {
  val maxStack = 1
  val maxLocals = 3
  val name = "if__&&(_&&_)"
  val desc = "(ZZZ)V"
  val insns = {
    val lbl0 = label()
    insnList(
      iload(0),
      ifeq(lbl0),
      iload(1),
      ifeq(lbl0),
      iload(2),
      ifeq(lbl0),
      invokestatic("foo", "bar", "()V"),
      lbl0,
      vreturn())
  }

  val test: Test = {
    case Exec(If(And(True(_), And(True(_), True(_))), _) ::
	      Label(_) ::
	      Return(_) ::
	      Nil) => true
    case tree => false
  }
}

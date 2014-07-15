package scala.bytecode.test.ifs

import scala.bytecode.asm._
import scala.bytecode.ast._

object if_and_Or_AND extends scala.bytecode.test.ASTCase {
  val maxStack = 1
  val maxLocals = 4
  val name = "if__&&(_||(_&&_))"
  val desc = "(ZZZZ)V"
  val insns = {
    val lbl0 = label()
    val lbl1 = label()
    insnList(
      iload(0),
      ifeq(lbl1),
      iload(1),
      ifne(lbl0),
      iload(2),
      ifeq(lbl1),
      iload(3),
      ifeq(lbl1),
      lbl0,
      invokestatic("foo", "bar", "()V"),
      lbl1,
      vreturn())
  }

  val test: Test = {
    case Exec(If(And(True(_), Or(True(_), And(True(_), True(_)))), _) ::
	      Label(_) ::
	      Return(_) ::
	      Nil) => true
    case tree => false
  }
}

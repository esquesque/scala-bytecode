package scala.bytecode.test.ifs

import scala.bytecode.asm._
import scala.bytecode.ast._

object if_or_else extends scala.bytecode.test.ASTCase {
  val maxStack = 1
  val maxLocals = 2
  val name = "if__||__else"
  val desc = "(ZZ)V"
  val insns = {
    val lbl0 = label()
    val lbl1 = label()
    val lbl2 = label()
    insnList(
      iload(0),
      ifne(lbl0),
      iload(1),
      ifeq(lbl1),
      lbl0,
      invokestatic("foo", "bar", "()V"),
      goto(lbl2),
      lbl1,
      invokestatic("foo", "baz", "()V"),
      lbl2,
      vreturn())
  }

  val test: Test = {
    case Exec(If(Or(True(_), True(_)), _, _) :: _) => true
    case tree => false
  }
}

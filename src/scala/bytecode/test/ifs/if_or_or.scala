package scala.bytecode.test.ifs

import scala.bytecode.asm._
import scala.bytecode.ast._

object if_or_or extends scala.bytecode.test.ASTCase {
  val maxStack = 1
  val maxLocals = 3
  val name = "if__||(_||_)"
  val desc = "(ZZZ)V"
  val insns = {
    val lbl0 = label()
    val lbl1 = label()
    insnList(
      iload(0),
      ifne(lbl0),
      iload(1),
      ifne(lbl0),
      iload(2),
      ifeq(lbl1),
      lbl0,
      invokestatic("foo", "bar", "()V"),
      lbl1,
      vreturn())
  }

  val test: Test = {
    case Exec(If(Or(Ne(_, _), Or(Ne(_, _), Ne(_, _))), _) :: _) => true
    case tree => false
  }
}
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
    val lbl1 = label()
    val lbl2 = label()
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
    case Exec(If(And(Ne(_, _), And(Ne(_, _), Ne(_, _))), _) :: _) => true
    case tree => false
  }
}

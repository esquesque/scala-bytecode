package scala.bytecode.test.ternaries

import scala.bytecode.asm._

object zero_stack extends scala.bytecode.test.ASTCase {
  val maxStack = 3
  val maxLocals = 2
  val name = "z_stack"
  val desc = "(Z)V"
  val insns = {
    val lbl0 = label()
    val lbl1 = label()
    insnList(
      iload(0),
      ifeq(lbl0),
      apush("x"),
      goto(lbl1),
      lbl0,
      apush("y"),
      lbl1,
      astore(1),
      vreturn())
  }

  val test: Test = {
    case _ => false
  }
}

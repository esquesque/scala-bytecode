package scala.bytecode.test.ternaries

import scala.bytecode.asm._

object nonzero_stack extends scala.bytecode.test.ASTCase {
  val maxStack = 3
  val maxLocals = 2
  val name = "nz_stack"
  val desc = "(Z)V"
  val insns = {
    val lbl0 = label()
    val lbl1 = label()
    val lbl2 = label()
    insnList(
      ipush(0xabcd),
      iload(0),
      ifeq(lbl0),
      apush("x"),
      goto(lbl1),
      lbl0,
      apush("y"),
      lbl1,
      invokestatic("foo", "bar", "(Ljava/lang/String;)I"),
      if_icmpeq(lbl2),
      invokestatic("foo", "baz", "()V"),
      lbl2,
      vreturn())
  }

  val test: Test = {
    case _ => false
  }
}

package scala.bytecode.test.dups

import scala.bytecode.asm._

object dup_init_asto extends scala.bytecode.test.InsnsCase {
  val maxStack = 2
  val maxLocals = 1
  val name = "dup_init_a="
  val desc = "()V"
  val insns = insnList(
    anew("java/lang/Object"),
    dup(),
    invokespecial("java/lang/Object", "<init>", "()V"),
    astore(0),
    vreturn())

  val test: Test = {
    case initnew(_, _, _) :: astore(_) :: _ => true
    case _ => false
  }
}

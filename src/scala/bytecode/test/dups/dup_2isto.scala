package scala.bytecode.test.dups

import scala.bytecode.asm._

object dup_2isto extends scala.bytecode.test.InsnsCase {
  val maxStack = 2
  val maxLocals = 2
  val name = "dup_2i="
  val desc = "()V"
  val insns = insnList(ipush(0xa), dup(), istore(0), istore(1),
    vreturn())

  val test: Test = {
    case ipush(_) :: istore(x) ::
	 iload(y) :: istore(_) ::
	 iload(z) :: istore(_) :: _ => x == y && y == z
    case _ => false
  }
}

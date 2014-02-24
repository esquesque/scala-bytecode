package scala.bytecode.test.dups

import scala.bytecode.asm._

object dup_iadd_isto extends scala.bytecode.test.InsnsCase {
  val maxStack = 2
  val maxLocals = 1
  val name = "dup_i+_i="
  val desc = "()V"
  val insns = insnList(ipush(0xa), dup(), iadd(), istore(0),
    vreturn())

  val test: Test = {
    case ipush(_)  :: istore(x) ::
	 iload(y)  :: iload(z)  :: iadd() :: istore(_) :: _ => x == y && y == z
    case _ => false
  }
}

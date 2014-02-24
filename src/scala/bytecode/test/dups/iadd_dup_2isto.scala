package scala.bytecode.test.dups

import scala.bytecode.asm._

object iadd_dup_2isto extends scala.bytecode.test.InsnsCase {
  val maxStack = 2
  val maxLocals = 2
  val name = "i+_dup_2i="
  val desc = "()V"
  val insns = insnList(
    ipush(0xa), ipush(0xb), iadd(), dup(), istore(0), istore(1), vreturn())

  val test: Test = {
    case ipush(0xa) :: ipush(0xb) :: iadd() :: istore(x) ::
	 iload(y)   :: istore(_)  ::
	 iload(z)   :: istore(_)  :: _ => x == y && y == z
    case _ => false
  }
}

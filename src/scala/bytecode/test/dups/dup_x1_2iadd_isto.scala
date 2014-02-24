package scala.bytecode.test.dups

import scala.bytecode.asm._

object dup_x1_2iadd_isto extends scala.bytecode.test.InsnsCase {
  val maxStack = 3
  val maxLocals = 1
  val name = "dup_x1_2i+_i="
  val desc = "()V"
  val insns = insnList(
    ipush(0xa), ipush(0xb), dup_x1(), iadd(), iadd(), istore(0),
    vreturn())

  val test: Test = {
    case ipush(0xb) :: istore(x)  ::
         ipush(0xa) :: iload(y)   :: iadd() :: iload(z) :: iadd() ::
	 istore(_)  :: _ => x == y && y == z
    case _ => false
  }
}

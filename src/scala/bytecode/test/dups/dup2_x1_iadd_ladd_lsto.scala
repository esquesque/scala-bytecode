package scala.bytecode.test.dups

import scala.bytecode.asm._

object dup2_x1_iadd_ladd_lsto extends scala.bytecode.test.InsnsCase {
  val maxStack = 6
  val maxLocals = 2
  val name = "dup2_x1_i+_l+_l="
  val desc = "()V"
  val insns = insnList(
    ipush(0xa), lpush(0xb),
    dup2_x1(),
    l2i(), iadd(), i2l(), ladd(),
    lstore(0),
    vreturn())

  val test: Test = {
    case lpush(0xb) :: lstore(x)  ::
         ipush(0xa) :: lload(y)   :: l2i()      :: iadd() :: i2l() ::
	 lload(z)   :: ladd()     :: lstore(_)  :: _ => x == y && y == z
    case _ => false
  }
}

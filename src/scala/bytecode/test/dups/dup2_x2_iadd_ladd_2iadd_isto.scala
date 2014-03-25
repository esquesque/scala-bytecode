package scala.bytecode.test.dups

import scala.bytecode.asm._

object dup2_x2_iadd_ladd_2iadd_isto extends scala.bytecode.test.InsnsCase {
  val maxStack = 6
  val maxLocals = 1
  val name = "dup2_x2_i+_l+_2i+_i="
  val desc = "()V"
  val insns = insnList(
    lpush(0xabcd), ipush(0xe), ipush(0xf),
    dup2_x2(),
    iadd(), i2l(), ladd(), l2i(), iadd(), iadd(),
    istore(0),
    vreturn())

  val test: Test = {
    case ipush(0xf)    :: istore(x0) ::
         ipush(0xe)    :: istore(y0) ::
	 lpush(0xabcd) :: iload(y1)  :: iload(x1) ::
	 iadd()        :: i2l()      :: ladd() :: l2i() ::
	 iload(y2)     :: iload(x2)  ::
	 iadd()        :: iadd()     :: istore(_) :: _ =>
	   x0 == x1 && x1 == x2 && y0 == y1 && y1 == y2
    case _ => false
  }
}

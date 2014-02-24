package scala.bytecode.test.dups

import scala.bytecode.asm._

object dup2_x1_4iadd_isto extends scala.bytecode.test.InsnsCase {
  val maxStack = 5
  val maxLocals = 1
  val name = "dup2_x1_4i+_i="
  val desc = "()V"
  val insns = insnList(
    ipush(0xa), ipush(0xb), ipush(0xc),
    dup2_x1(),
    iadd(), iadd(), iadd(), iadd(),
    istore(0),
    vreturn())

  val test: Test = {
    case ipush(0xc) :: istore(x0) ::
         ipush(0xb) :: istore(y0) ::
	 ipush(0xa) :: iload(y1)  :: iload(x1)  :: iadd() :: iadd() ::
	 iload(y2)  :: iload(x2)  :: iadd() :: iadd() :: istore(_) :: _ =>
	   x0 == x1 && x1 == x2 && y0 == y1 && y1 == y2
    case _ => false
  }
}

package scala.bytecode.test.dups

import scala.bytecode.asm._

object _2iadd_dup2_4isto extends scala.bytecode.test.InsnsCase {
  val maxStack = 4
  val maxLocals = 4
  val name = "2i+_dup2_4i="
  val desc = "()V"
  val insns = insnList(
    ipush(0xa), ipush(0xb), iadd(),
    ipush(0xc), ipush(0xd), iadd(),
    dup2(),
    istore(0), istore(1), istore(2), istore(3),
    vreturn())

  val test: Test = {
    case ipush(0xc) :: ipush(0xd) :: iadd() :: istore(x0) ::
	 ipush(0xa) :: ipush(0xb) :: iadd() :: istore(y0) ::
	 iload(x1)  :: istore(0)  ::
	 iload(y1)  :: istore(1)  ::
	 iload(x2)  :: istore(2)  ::
	 iload(y2)  :: istore(3)  :: _ =>
	   x0 == x1 && x1 == x2 && y0 == y1 && y1 == y2
    case _ => false
  }
}

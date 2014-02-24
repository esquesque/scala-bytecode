package scala.bytecode.test.dups

import scala.bytecode.asm._

object dup_x2_3iadd_isto extends scala.bytecode.test.InsnsCase {
  val maxStack = 4
  val maxLocals = 1
  val name = "dup_x2_3i+_i="
  val desc = "()V"
  val insns = insnList(
    ipush(0xa), ipush(0xb), ipush(0xc),
    dup_x2(),
    iadd(), iadd(), iadd(), istore(0),
    vreturn())

  val test: Test = {
    case ipush(0xc) :: istore(x)  ::
	 ipush(0xa) :: ipush(0xb) :: iload(y) ::
	 iadd()     :: iadd()     :: iload(z) :: iadd() :: istore(_) :: _ =>
	   x == y && y == z
    case _ => false
  }
}

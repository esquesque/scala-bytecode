package scala.bytecode.test.dups

import scala.bytecode.asm._

object ladd_dup2_2lsto extends scala.bytecode.test.InsnsCase {
  val maxStack = 4
  val maxLocals = 4
  val name = "l+_dup2_2l="
  val desc = "()V"
  val insns = insnList(
    lpush(0xa), lpush(0xb), ladd(),
    dup2(),
    lstore(0), lstore(2),
    vreturn())

  val test: Test = {
    case lpush(0xa) :: lpush(0xb) :: ladd() :: lstore(x)  ::
	 lload(y)   :: lstore(_)  ::
	 lload(z)   :: lstore(_)  :: _ => x == y && y == z
    case _ => false
  }
}

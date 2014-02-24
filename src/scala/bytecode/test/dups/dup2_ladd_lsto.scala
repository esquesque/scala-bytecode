package scala.bytecode.test.dups

import scala.bytecode.asm._

object dup2_ladd_lsto extends scala.bytecode.test.InsnsCase {
  val maxStack = 4
  val maxLocals = 4
  val name = "dup2_l+_l="
  val desc = "()V"
  val insns = insnList(lpush(0xa), dup2(), ladd(), lstore(0),
    vreturn())

  val test: Test = {
    case lpush(_) :: lstore(x) ::
	 lload(y) :: lload(z)  :: ladd() :: lstore(_) :: _ => x == y && y == z
    case _ => false
  }
}

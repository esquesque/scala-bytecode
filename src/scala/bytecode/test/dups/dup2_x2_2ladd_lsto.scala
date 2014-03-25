package scala.bytecode.test.dups

import scala.bytecode.asm._

object dup2_x2_2ladd_lsto extends scala.bytecode.test.InsnsCase {
  val maxStack = 6
  val maxLocals = 2
  val name = "dup2_x2_2l+_l="
  val desc = "()V"
  val insns = insnList(
    lpush(0xabc),
    lpush(0xdef),
    dup2_x2(),
    ladd(),
    ladd(),
    lstore(0),
    vreturn())

  val test: Test = {
    case lpush(0xdef) :: lstore(x0) ::
         lpush(0xabc) :: lload(x1)  ::
	 ladd()       :: lload(x2)  :: ladd() :: lstore(_) :: _ =>
	   x0 == x1 && x1 == x2
    case _ => false
  }
}

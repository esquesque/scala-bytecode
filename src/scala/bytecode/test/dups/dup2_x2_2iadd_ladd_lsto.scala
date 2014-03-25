package scala.bytecode.test.dups

import scala.bytecode.asm._

object dup2_x2_2iadd_ladd_lsto extends scala.bytecode.test.InsnsCase {
  val maxStack = 6
  val maxLocals = 2
  val name = "dup2_x2_2i+_l+_l="
  val desc = "()V"
  val insns = insnList(
    ipush(0xa), ipush(0xb), lpush(0xcdef),
    dup2_x2(),
    l2i(), iadd(), iadd(), i2l(), ladd(),
    lstore(0),
    vreturn())

  val test: Test = {
    case lpush(0xcdef) :: lstore(x0) ::
         ipush(0xa)    :: ipush(0xb) :: lload(x1) ::
	 l2i()         :: iadd()     :: iadd()    :: i2l() ::
	 lload(x2)     :: ladd()     :: lstore(_) :: _ =>
	   x0 == x1 && x1 == x2
    case _ => false
  }
}

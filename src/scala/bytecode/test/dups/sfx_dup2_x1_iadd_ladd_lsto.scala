package scala.bytecode.test.dups

import scala.bytecode.asm._

object sfx_dup2_x1_iadd_ladd_lsto extends scala.bytecode.test.InsnsCase {
  val maxStack = 6
  val maxLocals = 2
  val name = "sfx_dup2_x1_i+_l+_l="
  val desc = "()V"
  val insns = insnList(
    invokestatic("foo", "bar", "()I"),
    invokestatic("foo", "baz", "()J"),
    dup2_x1(),
    l2i(), iadd(), i2l(), ladd(),
    lstore(0),
    vreturn())

  val test: Test = {
    case invokestatic(_, "bar", _) :: istore(x0) ::
	 invokestatic(_, "baz", _) :: lstore(y0) ::
         iload(x1)                 :: lload(y1)  ::
	 l2i()                     :: iadd()     :: i2l()      ::
	 lload(y2)                 :: ladd()     :: lstore(_)  :: _ =>
	   x0 == x1 && y0 == y1 && y1 == y2
    case _ => false
  }
}

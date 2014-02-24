package scala.bytecode.test.dups

import scala.bytecode.asm._

object sfx_dup_x1_2iadd_isto extends scala.bytecode.test.InsnsCase {
  val maxStack = 3
  val maxLocals = 1
  val name = "sfx_dup_x1_2i+_i="
  val desc = "()V"
  val insns = insnList(
    invokestatic("foo", "bar", "()I"),
    invokestatic("foo", "baz", "()I"),
    dup_x1(),
    iadd(), iadd(),
    istore(0),
    vreturn())

  val test: Test = {
    case invokestatic(_, "bar", _) :: istore(x0) ::
	 invokestatic(_, "baz", _) :: istore(y0) ::
	 iload(x1)                 :: iload(y1)  :: iadd() ::
	 iload(y2)                 :: iadd()     :: istore(_) :: _ =>
	   x0 == x1 && y0 == y1 && y1 == y2
    case _ => false
  }
}

package scala.bytecode.test.dups

import scala.bytecode.asm._

object sfx_dup_x2_3iadd_isto extends scala.bytecode.test.InsnsCase {
  val maxStack = 4
  val maxLocals = 1
  val name = "sfx_dup_x2_3i+_i="
  val desc = "()V"
  val insns = insnList(
    invokestatic("foo", "bar", "()I"),
    invokestatic("foo", "baz", "()I"),
    invokestatic("foo", "qux", "()I"),
    dup_x2(),
    iadd(), iadd(), iadd(), istore(0),
    vreturn())

  val test: Test = {
    case invokestatic(_, "bar", _) :: istore(x0) ::
         invokestatic(_, "baz", _) :: istore(y0) ::
	 invokestatic(_, "qux", _) :: istore(z0) ::
	 iload(x1)                 :: iload(y1)  :: iload(z1) ::
	 iadd()                    :: iadd()     :: iload(z2) :: iadd() ::
	 istore(_) :: _ => x0 == x1 && y0 == y1 && z0 == z1 && z1 == z2
    case _ => false
  }
}

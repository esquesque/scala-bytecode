package scala.bytecode.test.dups

import scala.bytecode.asm._

object sfx_dup2_x1_4iadd_isto extends scala.bytecode.test.InsnsCase {
  val maxStack = 5
  val maxLocals = 1
  val name = "sfx_dup2_x1_4i+_i="
  val desc = "()V"
  val insns = insnList(
    invokestatic("foo", "bar", "()I"),
    invokestatic("foo", "baz", "()I"),
    invokestatic("foo", "qux", "()I"),
    dup2_x1(),
    iadd(), iadd(), iadd(), iadd(),
    istore(0),
    vreturn())

  val test: Test = {
    case invokestatic(_, "bar", _) :: istore(w0) ::
	 invokestatic(_, "baz", _) :: istore(x0) ::
	 invokestatic(_, "qux", _) :: istore(y0) ::
	 iload(x1)                 :: istore(z0) ::
	 iload(w1)                 :: iload(z1)  :: iload(y1) ::
	 iadd()                    :: iadd()     ::
	 iload(z2)                 :: iload(y2)  ::
	 iadd()                    :: iadd()     :: istore(_) :: _ =>
	   w0 == w1 && x0 == x1 && y0 == y1 && y1 == y2 && z0 == z1 && z1 == z2
    case _ => false
  }
}

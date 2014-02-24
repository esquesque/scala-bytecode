package scala.bytecode.test.dups

import scala.bytecode.asm._

object sfx_2iadd_dup2_4isto extends scala.bytecode.test.InsnsCase {
  val maxStack = 4
  val maxLocals = 4
  val name = "sfx_2i+_dup2_4i="
  val desc = "()V"
  val insns = insnList(
    invokestatic("foo", "foo", "()I"),
    invokestatic("foo", "bar", "()I"), iadd(),
    invokestatic("foo", "baz", "()I"),
    invokestatic("foo", "qux", "()I"), iadd(),
    dup2(),
    istore(0), istore(1), istore(2), istore(3),
    vreturn())

  val test: Test = {
    case invokestatic(_, "foo", _) :: invokestatic(_, "bar", _) :: iadd() ::
	 istore(z0)                ::
	 invokestatic(_, "baz", _) :: invokestatic(_, "qux", _) :: iadd() ::
	 istore(y0)                ::
	 iload(z1)                 ::
	 istore(x0)                ::
	 iload(y1)                 :: istore(0)                 ::
	 iload(x1)                 :: istore(1)                 ::
	 iload(y2)                 :: istore(2)                 ::
	 iload(x2)                 :: istore(3)                 :: _ =>
	   x0 == x1 && x1 == x2 && y0 == y1 && y1 == y2 && z0 == z1
    case _ => false
  }
}

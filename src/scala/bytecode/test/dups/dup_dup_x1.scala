package scala.bytecode.test.dups

import scala.bytecode.asm._

object dup_dup_x1 extends scala.bytecode.test.InsnsCase {
  val maxStack = 5
  val maxLocals = 1
  val name = "dup_dup_x1_return(x.foo[--x.bar])"
  val desc = "(Lx;)I"
  val insns = insnList(
    aload(0),
    getfield("x", "foo", "[I"),
    aload(0),
    dup(),
    getfield("x", "bar", "I"),
    ipush(1),
    isub(),
    dup_x1(),
    putfield("x", "bar", "I"),
    array.iload(),
    ireturn())

/*  i wish this produced:
    aload(0)
    getfield(_, "bar", _)
    ipush(1)
    isub()
    istore(1)
    aload(0)
    iload(1)
    putfield(_, "bar", _)
    aload(0)
    getfield(_, "foo", _)
    iload(1)
    array.iload()
    ireturn()

    but it produces an excess of load/stores. an additional transform to reduce
    them may be necessitated, or modify the sideEffect portion of
    AnchorFloatingStmts
*/

  val test: Test = {
    case aload(0) :: getfield(_, "foo", _) :: astore(3) ::
	 aload(0) :: astore(1) :: aload(3) :: astore(4) ::
	 aload(1) :: astore(5) :: aload(1) :: getfield(_, "bar", _) ::
	 ipush(1) :: isub() :: istore(2) ::
	 aload(5) :: iload(2) :: putfield(_, "bar", _) ::
	 aload(4) :: iload(2) :: array.iload() :: ireturn() :: Nil => true
    case _ => false
  }
}

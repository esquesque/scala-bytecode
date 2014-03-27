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

  val test: Test = {
    case _ => false
  }
}

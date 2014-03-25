package scala.bytecode.test.try_catches

import scala.bytecode.asm._
import scala.bytecode.ast._

object try_catch___if extends scala.bytecode.test.ASTCase {
  val maxStack = 1
  val maxLocals = 2
  val name = "try_catch___if"
  val desc = "(Z)V"

  override def tryCatches =
    (0, 2, 4, Some("x")) :: Nil

  val insns = {
    val lbl0 = label()
    val lbl1 = label()
    insnList(
      label(),
      invokestatic("foo", "bar", "()V"),
      label(),
      goto(lbl0),
      label(),
      astore(1),
      aload(1),
      invokevirtual("foo", "baz", "()V"),
      lbl0,
      iload(0),
      ifeq(lbl1),
      invokestatic("foo", "qux", "()V"),
      lbl1,
      vreturn())
  }

  val test: Test = {
    case tree => false
  }
}

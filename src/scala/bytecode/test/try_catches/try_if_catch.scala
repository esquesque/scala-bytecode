package scala.bytecode.test.try_catches

import scala.bytecode.asm._
import scala.bytecode.ast._

object try_if_catch extends scala.bytecode.test.ASTCase {
  val maxStack = 1
  val maxLocals = 2
  val name = "try_if_catch"
  val desc = "(Z)V"

  override def tryCatches =
    (0, 5, 7, Some("x")) :: Nil

  val insns = {
    val lbl0 = label()
    val lbl1 = label()
    val lbl2 = label()
    insnList(
      label(),
      iload(0),
      lbl0,
      invokestatic("foo", "bar", "()I"),
      ifeq(lbl0),
      label(),
      goto(lbl2),
      label(),
      astore(1),
      aload(1),
      invokevirtual("foo", "baz", "()V"),
      lbl2,
      vreturn())
  }

  val test: Test = {
    case tree => false
  }
}
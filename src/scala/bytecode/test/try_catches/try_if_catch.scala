package scala.bytecode.test.try_catches

import scala.bytecode.asm._
import scala.bytecode.ast._

object try_if_catch extends scala.bytecode.test.ASTCase {
  val maxStack = 2
  val maxLocals = 2
  val name = "try_if_catch"
  val desc = "(Z)V"

  override def tryCatches =
    (0, 4, 6, Some("x")) :: Nil

  val insns = {
    val lbl0 = label()
    val lbl1 = label()
    insnList(
      label(),
      iload(0),
      ifeq(lbl0),
      invokestatic("foo", "bar", "()V"),
      lbl0,
      goto(lbl1),
      label(),
      astore(1),
      aload(1),
      invokevirtual("foo", "baz", "()V"),
      lbl1,
      vreturn())
  }

  val test: Test = {
    case Exec(Try(_ :: If(True(_), Then(_)) :: _, Catch(_, _)) :: _) => true
    case tree => false
  }
}

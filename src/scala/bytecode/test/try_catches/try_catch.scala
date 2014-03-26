package scala.bytecode.test.try_catches

import scala.bytecode.asm._
import scala.bytecode.ast._

object try_catch extends scala.bytecode.test.ASTCase {
  val maxStack = 1
  val maxLocals = 1
  val name = "try_catch"
  val desc = "()V"

  override def tryCatches =
    (0, 2, 4, Some("x")) :: Nil

  val insns = {
    val lbl = label()
    insnList(
      label(),
      invokestatic("foo", "bar", "()V"),
      label(),
      goto(lbl),
      label(),
      astore(0),
      aload(0),
      invokevirtual("foo", "baz", "()V"),
      lbl,
      vreturn())
  }

  val test: Test = {
    case Exec(Try(_, Catch(_, _)) :: _) => true
    case tree => false
  }
}

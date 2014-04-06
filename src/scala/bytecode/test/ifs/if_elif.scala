package scala.bytecode.test.ifs

import scala.bytecode.asm._
import scala.bytecode.ast._

object if_elif extends scala.bytecode.test.ASTCase {
  val maxStack = 1
  val maxLocals = 2
  val name = "if_elif"
  val desc = "(ZZ)V"
  val insns = {
    val lbl0 = label()
    val lbl1 = label()
    insnList(
      iload(0),
      ifeq(lbl0),
      invokestatic("foo", "bar", "()V"),
      goto(lbl1),
      lbl0,
      iload(1),
      ifeq(lbl1),
      invokestatic("foo", "baz", "()V"),
      lbl1,
      vreturn())
  }

  val test: Test = {
    case Exec(If(True(_),
		 Then(Void(Method(_, "bar", _, _, _)) :: _),
		 Else(Label(_) ::
		      If(True(_),
			 Then(Void(Method(_, "baz", _, _, _)) :: _)) ::
		      Nil)) :: _) => true
    case tree => false
  }
}

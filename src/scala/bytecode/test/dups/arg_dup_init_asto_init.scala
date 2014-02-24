package scala.bytecode.test.dups

import scala.bytecode.asm._

object arg_dup_init_asto_init extends scala.bytecode.test.InsnsCase {
  val maxStack = 3
  val maxLocals = 2
  val name = "arg_dup_init_a=_init"
  val desc = "()V"
  val insns = insnList(
    anew("java/lang/String"), dup(), apush("arg"),
    invokespecial("java/lang/String", "<init>", "(Ljava/lang/String;)V"),
    astore(0),
    anew("java/lang/String"),
    invokespecial("java/lang/String", "<init>", "()V"),
    vreturn())

  val test: Test = {
    case apush(_)               ::
	 initnew(_, _, _)       ::
	 astore(_)              ::
	 anew(_)                ::
	 invokespecial(_, _, _) :: _ => true
    case _ => false
  }
}

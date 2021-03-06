package scala.bytecode.test.loops

import scala.bytecode.asm._
import scala.bytecode.ast._

object while_while_break_outer extends scala.bytecode.test.ASTCase {
  val maxStack = 1
  val maxLocals = 2
  val name = "while_while_break_outer"
  val desc = "(ZZ)V"

  /* outer:
   * while (var_0) {
   *   while (var_1) {
   *     foo.bar();
   *     break outer;
   *   }
   * }
   */
  val insns = {
    val (lbl0, lbl1) = (label(), label())
    insnList(
      lbl0,
      iload(0),
      ifeq(lbl1),
      iload(1),
      ifeq(lbl0),
      invokestatic("foo", "bar", "()V"),
      goto(lbl1),
      lbl1,
      vreturn())
  }

  val test: Test = {
    case Exec(Label(_) ::
	      While(True(_),
		    While(True(_), _) :: Nil) ::
	      Label(_) ::
	      Return(_) ::
	      Nil) => true
    case tree => false
  }
}

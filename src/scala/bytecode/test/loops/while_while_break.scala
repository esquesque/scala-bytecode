package scala.bytecode.test.loops

import scala.bytecode.asm._
import scala.bytecode.ast._

object while_while_break extends scala.bytecode.test.ASTCase {
  val maxStack = 1
  val maxLocals = 2
  val name = "while_while_break"
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
    val (lbl0, lbl1, lbl2) = (label(), label(), label())
    insnList(
      lbl0,
      iload(0),
      ifeq(lbl2),
      iload(1),
      ifeq(lbl1),
      invokestatic("foo", "bar", "()V"),
      goto(lbl0),
      lbl1,
      goto(lbl0),
      lbl2,
      vreturn())
  }

  val test: Test = {
    case Exec(If(True(_), Then(_)) :: Label(_) :: Return(_) :: Nil) => true
    case tree => false
  }
}

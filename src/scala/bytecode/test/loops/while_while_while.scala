/*     0: iload_0       
       1: ifeq          23
       4: iload_1       
       5: ifeq          0
       8: iload_2       
       9: ifeq          4
      12: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
      15: ldc           #3                  // String look ma
      17: invokevirtual #4                  // Method java/io/PrintStream.println:(Ljava/lang/String;)V
      20: goto          8
      23: return    */

package scala.bytecode.test.loops

import scala.bytecode.asm._
import scala.bytecode.ast._

object while_while_while extends scala.bytecode.test.ASTCase {
  val maxStack = 1
  val maxLocals = 3
  val name = "while_while_while"
  val desc = "(ZZZ)V"
  val insns = {
    val (lbl0, lbl1, lbl2, lbl3) = (label(), label(), label(), label())
    insnList(
      lbl0,
      iload(0),
      ifeq(lbl3),
      lbl1,
      iload(1),
      ifeq(lbl0),
      lbl2,
      iload(2),
      ifeq(lbl1),
      invokestatic("foo", "bar", "()V"),
      goto(lbl2),
      lbl3,
      vreturn())
  }

  val test: Test = {
    case Exec(Label(_) ::
	      While(True(_),
		    Label(_) ::
		    While(True(_),
			  Label(_) ::
			  While(True(_), _) :: Nil) :: Nil) ::
	      Label(_) ::
	      Return(_) ::
	      Nil) => true
    case tree => false
  }
}

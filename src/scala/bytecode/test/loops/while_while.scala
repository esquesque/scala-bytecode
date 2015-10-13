package scala.bytecode.test.loops

import scala.bytecode.asm._
import scala.bytecode.ast._

object while_while extends scala.bytecode.test.ASTCase {
  val maxStack = 1
  val maxLocals = 2
  val name = "while_while"
  val desc = "(ZZ)V"
  val insns = {
    val (lbl0, lbl1, lbl2) = (label(), label(), label())
/*     0: iload_1       
       1: ifeq          19
       4: iload_2       
       5: ifeq          0
       8: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
      11: ldc           #3                  // String got ya
      13: invokevirtual #4                  // Method java/io/PrintStream.println:(Ljava/lang/String;)V
      16: goto          4
      19: return  */
    insnList(
      lbl0,
      iload(0),
      ifeq(lbl2),
      lbl1,
      iload(1),
      ifeq(lbl0),
      invokestatic("foo", "bar", "()V"),
      goto(lbl1),
      lbl2,
      vreturn())
  }

  val test: Test = {
    case Exec(Label(_) ::
	      While(True(_),
		    Label(_) ::
		    While(True(_), _) :: Nil) ::
	      Label(_) ::
	      Return(_) ::
	      Nil) => true
    case tree => false
  }
}

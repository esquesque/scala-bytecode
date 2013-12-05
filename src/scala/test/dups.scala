/*Facile bytecode analysis, disassembling, and manipulation in Scala.
 *Copyright (C) 2013  David Phillips
 *
 *This program is free software: you can redistribute it and/or modify
 *it under the terms of the GNU General Public License as published by
 *the Free Software Foundation, either version 3 of the License, or
 *(at your option) any later version.
 *
 *This program is distributed in the hope that it will be useful,
 *but WITHOUT ANY WARRANTY; without even the implied warranty of
 *MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *GNU General Public License for more details.
 *
 *You should have received a copy of the GNU General Public License
 *along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package scala.test

import scala.bytecode._
import scala.bytecode.asm._

object dups {
  type Test = List[Insn] => Boolean

  val dup_init_asto = insnList(
    anew("java/lang/Object"),
    dup(),
    invokespecial("java/lang/Object", "<init>", "()V"),
    astore(0))
  val test_dup_init_asto: Test = {
    case initnew(_, _, _) :: astore(_) :: _ => true
    case _ => false
  }

  val arg_dup_init_asto_init = insnList(
    anew("java/lang/String"), dup(), apush("arg"),
    invokespecial("java/lang/String", "<init>", "(Ljava/lang/String;)V"),
    astore(0),
    anew("java/lang/String"),
    invokespecial("java/lang/String", "<init>", "()V"))
  val test_arg_dup_init_asto_init: Test = {
    case apush(_)               ::
	 initnew(_, _, _)       ::
	 astore(_)              ::
	 anew(_)                ::
	 invokespecial(_, _, _) :: _ => true
    case _ => false
  }

  val dup_2isto = insnList(ipush(0xa), dup(), istore(0), istore(1))
  val test_dup_2isto: Test = {
    case ipush(_)  :: istore(x) ::
	 iload(y)  :: istore(_) ::
	 iload(z)  :: istore(_) :: _ => x == y && y == z
    case _ => false
  }

  val iadd_dup_2isto = insnList(
    ipush(0xa), ipush(0xb), iadd(), dup(), istore(0), istore(1))
  val test_iadd_dup_2isto: Test = {
    case ipush(0xa) :: ipush(0xb) :: iadd() :: istore(x) ::
	 iload(y)   :: istore(_)  ::
	 iload(z)   :: istore(_)  :: _ => x == y && y == z
    case _ => false
  }

  val dup_iadd_isto = insnList(ipush(0xa), dup(), iadd(), istore(0))
  val test_dup_iadd_isto: Test = {
    case ipush(_)  :: istore(x) ::
	 iload(y)  :: iload(z)  :: iadd() :: istore(_) :: _ => x == y && y == z
    case _ => false
  }

  val dup_x1_2iadd_isto = insnList(
    ipush(0xa), ipush(0xb), dup_x1(), iadd(), iadd(), istore(0))
  val test_dup_x1_2iadd_isto: Test = {
    case ipush(0xb) :: istore(x)  ::
         ipush(0xa) :: iload(y)   :: iadd() :: iload(z) :: iadd() ::
	 istore(_)  :: _ => x == y && y == z
    case _ => false
  }

  val dup_x2_3iadd_isto = insnList(
    ipush(0xa), ipush(0xb), ipush(0xc),
    dup_x2(),
    iadd(), iadd(), iadd(), istore(0))
  val test_dup_x2_3iadd_isto: Test = {
    case ipush(0xc) :: istore(x)  ::
	 ipush(0xa) :: ipush(0xb) :: iload(y) ::
	 iadd()     :: iadd()     :: iload(z) :: iadd() :: istore(_) :: _ =>
	   x == y && y == z
    case _ => false
  }

  val sfx_dup_x1_2iadd_isto = insnList(
    invokestatic("foo", "bar", "()I"),
    invokestatic("foo", "baz", "()I"),
    dup_x1(),
    iadd(), iadd(),
    istore(0))
  val test_sfx_dup_x1_2iadd_isto: Test = {
    case invokestatic(_, "bar", _) :: istore(x0) ::
	 invokestatic(_, "baz", _) :: istore(y0) ::
	 iload(x1)                 :: iload(y1)  :: iadd() ::
	 iload(y2)                 :: iadd()     :: istore(_) :: _ =>
	   x0 == x1 && y0 == y1 && y1 == y2
    case _ => false
  }

  val sfx_dup_x2_3iadd_isto = insnList(
    invokestatic("foo", "bar", "()I"),
    invokestatic("foo", "baz", "()I"),
    invokestatic("foo", "qux", "()I"),
    dup_x2(),
    iadd(), iadd(), iadd(), istore(0))
  val test_sfx_dup_x2_3iadd_isto: Test = {
    case invokestatic(_, "bar", _) :: istore(x0) ::
         invokestatic(_, "baz", _) :: istore(y0) ::
	 invokestatic(_, "qux", _) :: istore(z0) ::
	 iload(x1)                 :: iload(y1)  :: iload(z1) ::
	 iadd()                    :: iadd()     :: iload(z2) :: iadd() ::
	 istore(_) :: _ => x0 == x1 && y0 == y1 && z0 == z1 && z1 == z2
    case _ => false
  }

  val dup2_2lsto = insnList(lpush(0xa), dup2(), lstore(0), lstore(2))
  val test_dup2_2lsto: Test = {
    case lpush(0xa) :: lstore(x)  ::
	 lload(y)   :: lstore(_)  ::
	 lload(z)   :: lstore(_)  :: _ => x == y && y == z
    case _ => false
  }

  val ladd_dup2_2lsto = insnList(
    lpush(0xa), lpush(0xb), ladd(),
    dup2(),
    lstore(0), lstore(2))
  val test_ladd_dup2_2lsto: Test = {
    case lpush(0xa) :: lpush(0xb) :: ladd() :: lstore(x)  ::
	 lload(y)   :: lstore(_)  ::
	 lload(z)   :: lstore(_)  :: _ => x == y && y == z
    case _ => false
  }

  val dup2_ladd_lsto = insnList(lpush(0xa), dup2(), ladd(), lstore(0))
  val test_dup2_ladd_lsto: Test = {
    case lpush(_) :: lstore(x) ::
	 lload(y) :: lload(z)  :: ladd() :: lstore(_) :: _ => x == y && y == z
    case _ => false
  }

  val dup2_4isto = insnList(
    ipush(0xa), ipush(0xb),
    dup2(),
    istore(0), istore(1), istore(2), istore(3))
  val test_dup2_4isto: Test = {
    case ipush(0xb) :: istore(x0) ::
	 ipush(0xa) :: istore(y0) ::
	 iload(x1)  :: istore(0)  ::
	 iload(y1)  :: istore(1)  ::
	 iload(x2)  :: istore(2)  ::
	 iload(y2)  :: istore(3)  :: _ =>
	   x0 == x1 && x1 == x2 && y0 == y1 && y1 == y2
    case _ => false
  }

  val _2iadd_dup2_4isto = insnList(
    ipush(0xa), ipush(0xb), iadd(),
    ipush(0xc), ipush(0xd), iadd(),
    dup2(),
    istore(0), istore(1), istore(2), istore(3))
  val test_2iadd_dup2_4isto: Test = {
    case ipush(0xc) :: ipush(0xd) :: iadd() :: istore(x0) ::
	 ipush(0xa) :: ipush(0xb) :: iadd() :: istore(y0) ::
	 iload(x1)  :: istore(0)  ::
	 iload(y1)  :: istore(1)  ::
	 iload(x2)  :: istore(2)  ::
	 iload(y2)  :: istore(3)  :: _ =>
	   x0 == x1 && x1 == x2 && y0 == y1 && y1 == y2
    case _ => false
  }


  val dup2_x1_iadd_ladd_lsto = insnList(
    ipush(0xa), lpush(0xb),
    dup2_x1(),
    l2i(), iadd(), i2l(), ladd(),
    lstore(0))
  val test_dup2_x1_iadd_ladd_lsto: Test = {
    case lpush(0xb) :: lstore(x)  ::
         ipush(0xa) :: lload(y)   :: l2i()      :: iadd() :: i2l() ::
	 lload(z)   :: ladd()     :: lstore(_)  :: _ => x == y && y == z
    case _ => false
  }

  val dup2_x1_4iadd_isto = insnList(
    ipush(0xa), ipush(0xb), ipush(0xc),
    dup2_x1(),
    iadd(), iadd(), iadd(), iadd(),
    istore(0))
  val test_dup2_x1_4iadd_isto: Test = {
    case ipush(0xc) :: istore(x0) ::
         ipush(0xb) :: istore(y0) ::
	 ipush(0xa) :: iload(y1)  :: iload(x1)  :: iadd() :: iadd() ::
	 iload(y2)  :: iload(x2)  :: iadd() :: iadd() :: istore(_) :: _ =>
	   x0 == x1 && x1 == x2 && y0 == y1 && y1 == y2
    case _ => false
  }

  val sfx_dup2_x1_iadd_ladd_lsto = insnList(
    invokestatic("foo", "bar", "()I"),
    invokestatic("foo", "baz", "()J"),
    dup2_x1(),
    l2i(), iadd(), i2l(), ladd(),
    lstore(0))
  val test_sfx_dup2_x1_iadd_ladd_lsto: Test = {
    case invokestatic(_, "bar", _) :: istore(x0) ::
	 invokestatic(_, "baz", _) :: lstore(y0) ::
         iload(x1)                 :: lload(y1)  ::
	 l2i()                     :: iadd()     :: i2l()      ::
	 lload(y2)                 :: ladd()     :: lstore(_)  :: _ =>
	   x0 == x1 && y0 == y1 && y1 == y2
    case _ => false
  }

  val sfx_dup2_x1_4iadd_isto = insnList(
    invokestatic("foo", "bar", "()I"),
    invokestatic("foo", "baz", "()I"),
    invokestatic("foo", "qux", "()I"),
    dup2_x1(),
    iadd(), iadd(), iadd(), iadd(),
    istore(0))
  val test_sfx_dup2_x1_4iadd_isto: Test = {
    case invokestatic(_, "bar", _) :: istore(v0) ::
	 invokestatic(_, "baz", _) :: istore(w0) ::
	 invokestatic(_, "qux", _) :: istore(x0) ::
	 iload(w1)                 :: istore(y0) ::
	 iload(v1)                 :: iload(y1)  :: iload(x1) ::
	 iadd()                    :: iadd()     ::
	 iload(y2)                 :: iload(x2)  ::
	 iadd()                    :: iadd()     :: istore(_) :: _ =>
	   v0 == v1 && w0 == w1 && x0 == x1 && x1 == x2 && y0 == y1 && y1 == y2
    case _ => false
  }

  val cases: List[(MethodInfo, Test)] =(
    ("dup_init_a=",              2, 1,
     dup_init_asto,              test_dup_init_asto)              ::
    ("arg_dup_init_a=",          3, 2,
     arg_dup_init_asto_init,     test_arg_dup_init_asto_init)     ::
    ("dup_2i=",                  2, 2,
     dup_2isto,                  test_dup_2isto)                  ::
    ("i+_dup_2i=",               2, 2,
     iadd_dup_2isto,             test_iadd_dup_2isto)             ::
    ("dup_i+_i=",                2, 1,
     dup_iadd_isto,              test_dup_iadd_isto)              ::
    ("dup_x1_2i+_i=",            3, 1,
     dup_x1_2iadd_isto,          test_dup_x1_2iadd_isto)          ::
    ("dup_x2_3i+_i=",            4, 1,
     dup_x2_3iadd_isto,          test_dup_x2_3iadd_isto)          ::
    ("sfx_dup_x1_2i+_i=",        3, 1,
     sfx_dup_x1_2iadd_isto,      test_sfx_dup_x1_2iadd_isto)      ::
    ("sfx_dup_x2_3i+_i=",        4, 1,
     sfx_dup_x2_3iadd_isto,      test_sfx_dup_x2_3iadd_isto)      ::
    ("dup2_2l=",                 4, 4,
     dup2_2lsto,                 test_dup2_2lsto)                 ::
    ("l+_dup2_2l=",              4, 4,
     ladd_dup2_2lsto,            test_ladd_dup2_2lsto)            ::
    ("dup2_l+_l=",               4, 2,
     dup2_ladd_lsto,             test_dup2_ladd_lsto)             ::
    ("dup2_4i=",                 4, 4,
     dup2_4isto,                 test_dup2_4isto)                 ::
    ("2i+_dup2_4i=",             4, 4,
     _2iadd_dup2_4isto,          test_2iadd_dup2_4isto)           ::
    ("dup2_x1_i+_l+_l=",         6, 2,
     dup2_x1_iadd_ladd_lsto,     test_dup2_x1_iadd_ladd_lsto)     ::
    ("dup2_x1_4i+_i=",           5, 1,
     dup2_x1_4iadd_isto,         test_dup2_x1_4iadd_isto)         ::
    ("sfx_dup2_x1_i+_l+_l=",     6, 2,
     sfx_dup2_x1_iadd_ladd_lsto, test_sfx_dup2_x1_iadd_ladd_lsto) ::
    ("sfx_dup2_x1_4i+_i=",       5, 1,
     sfx_dup2_x1_4iadd_isto,     test_sfx_dup2_x1_4iadd_isto)     ::
    Nil) map {

    case (name, maxStack, maxLocals, insns, test) =>
      val method = ClassInfo.anonymous.newMethod('static)(
	name,
	"()V",
	insns += vreturn())
      method.node.maxStack = maxStack
      method.node.maxLocals = maxLocals
      (method, test)
  }

  import java.io.{ByteArrayOutputStream => BAOS, PrintStream => PS}
  def main(args: Array[String]) {
    val baos0 = new BAOS
    val baos1 = new BAOS
    val ps0 = new PS(baos0, true)
    val ps1 = new PS(baos1, true)
    val results = for ((method, test) <- cases) yield {
      println(">>>>>"+ method)
      val result = try {
	method.instructions.out(ps0)
	method.apply(CollapseStackManipulations)
	method.instructions.out(ps1)
	method.apply(AnchorFloatingStmts)
	//method.apply(AnchorFloatingStmts)
	method.tree.out()
	test(method.instructions.toList)
      } catch {
	case x: Throwable => x.printStackTrace; false
      }
      if (result) println(">PASS")
      else {
	println(">FAIL")
	println(baos0.toString)
	println(".....")
	println(baos1.toString)
	println(".....")
	method.instructions.out()
      }
      baos0.reset
      baos1.reset
      result
    }
    System.exit(if (results reduce (_ & _)) {
      println(">"+ results.length +" OK")
      0
    } else {
      println(">"+ (results filter (! _)).length + " FAILED")
      1
    } )
  }
}

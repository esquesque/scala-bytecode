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

object ifs {
  val ternary = {
    val lbl0 = label()
    val lbl1 = label()
    insnList(
      iload(0),
      ifeq(lbl0),
      apush("x"),
      goto(lbl1),
      lbl0,
      apush("y"),
      lbl1,
      astore(1))
  }
  val nonzero_ternary = {
    val lbl0 = label()
    val lbl1 = label()
    insnList(
      ipush(1),
      iload(0),
      ifeq(lbl0),
      apush("x"),
      goto(lbl1),
      lbl0,
      apush("y"),
      lbl1,
      invokestatic("foo", "bar", "(ILjava/lang/String;)V"))
  }

  val methods: List[MethodInfo] = (
    ("ternary",         "(I)V", 1, 2, ternary)         ::
    ("nonzero_ternary", "(I)V", 2, 2, nonzero_ternary) :: Nil) map {
      case (name, desc, maxStack, maxLocals, insns) =>
	val method = ClassInfo.anonymous.newMethod('static)(
	  name,
	  desc,
	  insns += vreturn())
      method.node.maxStack = maxStack
      method.node.maxLocals = maxLocals
      method
    }

  def main(args: Array[String]) {
    for (method <- methods) {
      method.instructions.out()
      println(".....")
      method apply CollapseTernaryExprs
      method.instructions.out()
      println(".....")
      method apply AnchorFloatingStmts
      method.instructions.out()
      method.tree.out()
    }
  }
}

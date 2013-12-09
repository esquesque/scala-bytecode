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

  def main(args: Array[String]) {
    val method = ClassInfo.anonymous.newMethod('static)(
      "ternary",
      "(I)V",
      ternary += vreturn())
    method.node.maxStack = 1
    method.node.maxLocals = 2
    method.instructions.out()
    println(".....")
    method apply AnchorTernaryStmts
    method.instructions.out()
    method.tree.out()
  }
}

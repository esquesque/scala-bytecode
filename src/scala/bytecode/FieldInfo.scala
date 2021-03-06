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

package scala.bytecode

import org.objectweb.asm.Opcodes._
import org.objectweb.asm.tree.FieldNode

class FieldInfo(val cxt: Cxt, val owner: ClassInfo, val node: FieldNode)
extends MemberInfo[FieldNode, ast.FieldDecl] {
  lazy val tree: ast.FieldDecl = null

  def modifiers: List[Symbol] = Cxt.fieldModifierAccess.toList map {
    case (sym, acc) if (acc & node.access) != 0 => Some(sym)
    case _ => None
  } filterNot (_.isEmpty) map (_.get)

  def name = node.name
  def desc = node.desc
  def verbose = owner.name +"/"+ name +" "+ desc

  val init: Option[Any] = if (node.value == null) None else Some(node.value)

  /*def dump(writer: java.io.Writer, indent: Int) {
    writer append " "*indent
    if (modifiers.nonEmpty) {
      writer append modifiers.map(_.name).mkString(" ")
      writer append " "
    }
    writer append string
    writer append "\n"
    writer.flush
  }*/
}

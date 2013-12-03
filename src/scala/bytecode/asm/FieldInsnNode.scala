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

package scala.bytecode.asm

import org.objectweb.asm.tree.{FieldInsnNode => F, AbstractInsnNode => N}

object FieldInsnNode {
  def apply(opcode: Int, owner: String, name: String, desc: String): F =
    new F(opcode, owner, name, desc)

  def unapply(n: N): Option[(Int, String, String, String)] = n match {
    case f: F => Some(f.getOpcode, f.owner, f.name, f.desc); case _ => None
  }
}

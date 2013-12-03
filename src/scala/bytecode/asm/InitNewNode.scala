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

import org.objectweb.asm.{MethodVisitor, Opcodes}
import org.objectweb.asm.tree.{AbstractInsnNode => N,
			       LabelNode => L,
			       MethodInsnNode => M}
import Opcodes.{INVOKESTATIC => OP}

class InitNewNode(val instance: String,
		  owner: String,
		  desc: String) extends M(OP, owner, "<init>", desc)

object InitNewNode {
  def apply(instance: String, owner: String, desc: String): InitNewNode =
    new InitNewNode(instance, owner, desc)

  def unapply(n: N): Option[(String, String, String)] = n match {
    case inn: InitNewNode => Some((inn.instance, inn.owner, inn.desc))
    case _ => None
  }
}

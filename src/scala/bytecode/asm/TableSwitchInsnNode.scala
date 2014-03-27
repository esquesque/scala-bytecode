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

import org.objectweb.asm.tree.{TableSwitchInsnNode => T,
			       AbstractInsnNode => N,
			       LabelNode => L}
import scala.collection.JavaConversions._

object TableSwitchInsnNode {
  def apply(min: Int, max: Int, default: L, labels: List[L]): T =
    new T(min, max, default, labels: _*)

  import java.util.{List => list}
  def unapply(n: N): Option[(Int, Int, L, List[L])] = n match {
    case t: T =>
      Some(t.min, t.max, t.dflt, t.labels.asInstanceOf[list[L]].toList)
    case _ => None
  }
}
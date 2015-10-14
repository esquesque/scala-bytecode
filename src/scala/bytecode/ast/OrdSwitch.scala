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

package scala.bytecode.ast

case class OrdSwitch[V](key: Expr,
			 matches: List[(V, Int)],
			 default: Int) extends Stmt {
  def out(ps: java.io.PrintStream, indent: Int) {
    ps append " "* indent
    ps append "switch (\n"
    ps append (key show false)
    ps append ") {\n"
    matches foreach { case (value, idx) =>
      ps append " "* (indent + 2)
      ps append "case "
      ps append value.toString
      ps append " @ #"
      ps append idx.toString
      ps append '\n'
    }
    ps append " "* (indent + 2)
    ps append "default @ #"
    ps append default.toString
    ps append '\n'
    ps append " "* indent
    ps append '}'
    ps.flush
  }
}

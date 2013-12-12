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

case class If(cond: Cond, stmts: Stmt*) extends Stmt {
  def out(ps: java.io.PrintStream, indent: Int) {
    ps append " "* indent
    ps append "if ("
    ps append (cond show false)
    ps append ") "
    stmts.toList match {
      case then :: Nil =>
	then.out(ps, if (then.isInstanceOf[Goto]) 0 else indent)
      case then :: elses =>
	then.out(ps, indent)
	elses foreach { els =>
	  els.out(ps, indent)
	}
    }
    ps.flush
  }
}

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

import scala.bytecode.MethodInfo

class MethodDecl(val modifiers: List[Symbol],
		 val name: String,
		 val desc: String,
		 val thrown: List[String],
		 val body: List[Stmt]) extends Exec {
  def out(ps: java.io.PrintStream, indent: Int) {
    ps append " "* indent
    if (modifiers.nonEmpty) {
      ps append modifiers.map(_.name).mkString(" ")
      ps append " "
    }
    ps append name
    ps append desc
    thrown match {
      case x if x.nonEmpty => ps append x.mkString(" throws ", ", ", "")
      case _ =>
    }
    if (modifiers contains 'abstract) {
      ps append "\n"
      return
    }
    ps append " {\n"
    body foreach { _.out(ps, indent + 2) }
    ps append " "* indent
    ps append "}\n"
    ps.flush
  }
}

import org.objectweb.asm.tree.analysis.Analyzer
object MethodDecl {
  def apply(method: MethodInfo): MethodDecl = {
    val analyzer = method.cfgAnalyzer(MethodInfo.sourceInterpreter)
    val frames = analyzer.analyze(method.owner.name, method.node)
    new MethodDecl(method.modifiers,
		   method.name,
		   method.desc,
		   method.thrown,
		   method.cfg.blocks(frames))
  }
}

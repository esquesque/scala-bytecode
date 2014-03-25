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

import scala.bytecode.ClassInfo

class ClassDecl(val modifiers: List[Symbol],
		val name: String,
		val superName: String,
		val interfaces: List[String],
		val getbody: () => List[Stmt]) extends Exec {
  lazy val body: List[Stmt] = getbody()

  def outHeader(ps: java.io.PrintStream, indent: Int) {
    val interface = modifiers contains 'interface
    ps append " "* indent
    if (modifiers.nonEmpty) {
      ps append (modifiers filter ('interface != _) map (_.name)).mkString(" ")
      ps append " "
    }
    ps append (if (interface) "interface " else "class ")
    ps append name
    ps append " extends "
    ps append superName
    interfaces match {
      case x if x.nonEmpty =>
	ps append x.mkString(if (interface) ", " else " implements ", ", ", "")
      case _ =>
    }
    ps.flush
  }

  def out(ps: java.io.PrintStream, indent: Int) {
    outHeader(ps, indent)
    ps append " {\n"
    body foreach { _.out(ps, indent + 2) }
    ps append "}\n"
    ps.flush
  }
}

object ClassDecl {
  def apply(info: ClassInfo): ClassDecl =
    new ClassDecl(info.modifiers, info.name, info.superName, info.interfaces,
		  () => (info.fields map FieldDecl.apply) ++
		  (info.methods map MethodDecl.apply))
}

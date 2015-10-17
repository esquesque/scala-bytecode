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

package object ast {
  trait AST

  type TryCatch = (Int, Int, Int, Option[String])

  def descOf(value: Any): String = value match {
    case _: Int => "I"
    case _: Long => "J"
    case _: Float => "F"
    case _: Double => "D"
    case _: Class[_] => "Ljava/lang/Class;"
    case _: String => "Ljava/lang/String;"
    case null => "Lnull;"
    case _ => throw new RuntimeException("descOf("+ value +")")
  }

  val Zero = Push(0)
  val Null = Push(null)

  object JumpBlock {
    def unapply(b: Block): Option[Block] = b.body.last match {
      case Goto(_, _) => Some(b)
      case If(_, _) => Some(b)
      case _ => None
    }
  }

  object BranchBlock {
    def unapply(b: Block): Option[(List[Stmt], Cond)] = {
      val body = b.body
      if (body.isEmpty)
	None
      else {
	val init = body match {
	  case _ :: Nil => Nil
	  case body => body.init
	}
	body.last match {
	  case If(cond, _) => Some((init, cond))
	  case _ => None
	}
      }
    }
  }

  object SwitchBlock {
    def unapply(b: Block): Option[(List[Stmt], Expr, List[(List[Int], Int)], Int)] = {
      val body = b.body
      if (body.isEmpty)
	None
      else {
	val init = body match {
	  case _ :: Nil => Nil
	  case body => body.init
	}
	body.last match {
	  case OrdSwitch(key, matches, default) =>
	    val matchMap = matches.groupBy(_._2)
	    val ords = matchMap.keys.toList.sorted
	    val ordCases = ords map (ord => (matchMap(ord) map (_._1), ord))
	    Some((init, key, ordCases, default))
	  case _ => None
	}
      }
    }
  }
}

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

import org.objectweb.asm.{Opcodes, Type}
import org.objectweb.asm.tree.{MethodNode,
			       InsnList,
			       AbstractInsnNode => Insn,
			       LabelNode,
			       TryCatchBlockNode => TCB}
import org.objectweb.asm.tree.analysis.{Analyzer,
					BasicInterpreter,
					BasicValue,
					Frame,
					Interpreter,
					SourceInterpreter,
					Value}

import java.util.{Iterator => iterator, List => list}
import scala.collection.JavaConversions._

/* Wrapper for asm.tree.MethodNode with goodies...
 * Intent is to make the expression of complex bytecode in scala as pain-free as
 * possible.
 */
class MethodInfo(val cxt: Cxt, val owner: ClassInfo, val node: MethodNode)
extends MemberInfo[MethodNode, ast.MethodDecl] {
  def tree = ast.MethodDecl(this)

  def modifiers: List[Symbol] = Cxt.methodModifierAccess.toList map {
    case (sym, acc) if (acc & node.access) != 0 => Some(sym)
    case _ => None
  } filterNot (_.isEmpty) map (_.get)

  def name = node.name
  def desc = node.desc
  def verbose =
    modifiers.map(sym => sym.name).mkString("", " ", " ") +
    owner.name +"/"+ name + desc

  def arguments: List[(Int, String)] = {
    val argTypes = Type.getArgumentTypes(desc).toList
    argTypes.map(_.getSize).scanLeft(if (is('static)) 0 else 1)(_ + _).init.zip(
      argTypes.map(_.getDescriptor))
  }

  def thrown: List[String] =
    node.exceptions.asInstanceOf[list[String]].toList

  val instructions: asm.RichInsnList = new asm.RichInsnList(node.instructions)

  /* @return a 4-ple to avoid the need for a wrapper for TryCatchBlockNode;
   *  fourth element is an optional exception descriptor (None for finally).
   */
  def tryCatches: List[(Int, Int, Int, Option[String])] =
    node.tryCatchBlocks.asInstanceOf[list[TCB]].toList map (tcb =>
      (instructions indexOf tcb.start,
       instructions indexOf tcb.end,
       instructions indexOf tcb.handler,
       if (tcb.`type` == null) None else Some(tcb.`type`)))

  def addTryCatch(tc: (Int, Int, Int, Option[String])) {
    addTryCatch(tc._1, tc._2, tc._3, tc._4)
  }

  @unchecked
  def addTryCatch(tryEntry: Int, tryExit: Int, catchIdx: Int,
		  excn: Option[String]) {
    (instructions(tryEntry),
     instructions(tryExit),
     instructions(catchIdx)) match {
       case (s: LabelNode, e: LabelNode, h: LabelNode) =>
	 node.tryCatchBlocks.asInstanceOf[list[TCB]].add(
	   new TCB(s, e, h, excn.orNull))
       case _ =>
	 throw new RuntimeException
     }
  }

  def apply(changes: MethodInfo.Changes): MethodInfo.Record = {
    val record = MethodInfo.Record(changes.maxStack map (node.maxStack -> _),
				   changes.maxLocals map (node.maxLocals -> _),
				   Nil)//gotta convert InsnSpec=>InsnSpecRecord
    //before mutate
    if (changes.maxStack.isDefined)  { node.maxStack = changes.maxStack.get   }
    if (changes.maxLocals.isDefined) { node.maxLocals = changes.maxLocals.get }
    if (changes.insnSpec.nonEmpty)   { instructions.mutate(changes.insnSpec)  }
    record
  }

  def apply(transform: MethodInfo.Transform): MethodInfo.Record =
    apply(transform(this))

  def apply(transforms: MethodInfo.Transform*): List[MethodInfo.Record] =
    transforms.map(transform => apply(transform(this))).toList

  def abstractStack: List[MethodInfo.Record] =
    apply(CollapseStackManipulations, AnchorFloatingStmts)

  private lazy val uniqueLocals: Array[Short] = new Array[Short](node.maxLocals)

  def uniqueLocalId(prefix: String, v: Int): Symbol = {
    val n = uniqueLocals(v)
    uniqueLocals(v) = (n + 1).toShort
    def suffix(m: Int, r: String): String = {
      val d = ((m % 26) + 97).toChar
      if (m < 26) r + d else suffix(m / 26, r + d)
    }
    Symbol(prefix + v + suffix(n, ""))
  }

  private var preds: Array[Array[Int]] = null
  private var succs: Array[Array[Int]] = null

  def cfgAnalyzer(intr: Interpreter): Analyzer = new Analyzer(intr) {
    override def analyze(ownerName: String, node: MethodNode): Array[Frame] = {
      val len = instructions.length
      preds = Array.fill(len)(null); succs = Array.fill(len)(null)
      super.analyze(ownerName, node)
    }

    override def newControlFlowEdge(p: Int, s: Int) {
      if (preds(s) == null) preds(s) = Array(p)
      else if (! (preds(s) contains p)) preds(s) :+= p
      if (succs(p) == null) succs(p) = Array(s)
      else if (! (succs(p) contains s)) succs(p) :+= s
    }
  }

  /* @return a fresh ControlFlowGraph.
   */
  def cfg: ControlFlowGraph = new ControlFlowGraph(this) {
    val bounds: List[(Int, Int)] = {
      val tcs = tryCatches
      val ends = ((1 until instructions.length) filter { idx =>
	if (tcs exists (tc => tc._1 == idx + 1 || tc._2 == idx + 1)) true
	else {
	  val nps = if (idx < instructions.length - 1) preds(idx + 1) else null
          val ss = succs(idx)
          nps == null || nps.length != 1 || nps(0) != idx ||
          ss == null || ss.length != 1 || ss(0) != idx + 1
	}
      } ).toList map (_ + 1)
      (0 :: ends) zip ends
    }

    val size = bounds.length

    val edges: List[((Int, Int), (Int, Int))] =
      (bounds map (b => predecessors(b._1) map (_ -> b))).flatten

    def predecessors(beg: Int): List[(Int, Int)] = preds(beg) match {
      case null => Nil
      case ps => bounds filter { case (_, end) => ps contains end - 1 }
    }

    def successors(end: Int): List[(Int, Int)] = succs(end - 1) match {
      case null => Nil
      case ss => bounds filter { case (beg, _) => ss contains beg }
    }
  }

  import asm._
  def out(ps: java.io.PrintStream, indent: Int) {
    ps append owner.name
    ps append '/'
    ps append name
    ps append desc
    ps append '\n'
    val idxstrlen = (String valueOf instructions.length).length
    instructions.zipWithIndex foreach { case (insn, idx) =>
      ps append " "*(indent + 2 + (idxstrlen - (String valueOf idx).length))
      ps append (idx +": ")
      ps append insnString(insn)
      ps append "\n"
    }
  }
}

object MethodInfo {
  def apply(modifiers: Symbol*)
	   (name: String, desc: String, insns: asm.RichInsnList)
	   (implicit parent: ClassInfo): MethodInfo = {
    val acc = (modifiers map Cxt.methodModifierAccess) reduce (_ | _)
    val node = new MethodNode(acc, name, desc, null, null)
    node.instructions = insns.insnList
    new MethodInfo(parent.cxt, parent, node)
  }

  type InsnSpec = List[((Int, Int), List[Insn])]
  type InsnSpecRecord = List[(Int, List[Insn], List[Insn])]

  case class Changes(maxStack: Option[Int],
		     maxLocals: Option[Int],
		     insnSpec: InsnSpec)

  case class Record(maxStack: Option[(Int, Int)],
		    maxLocals: Option[(Int, Int)],
		    insnSpecRecord: InsnSpecRecord)

  object NoChanges {
    def apply(): Changes = Changes(None, None, Nil)
    def unapply(any: Any): Boolean = any match {
      case Changes(None, None, Nil) => true
      case Record(None, None, Nil) => true
      case _ => false
    }
  }

  trait Transform extends Function1[MethodInfo, Changes]

  def basicInterpreter: BasicInterpreter = new BasicInterpreter
  def sourceInterpreter: SourceInterpreter = new SourceInterpreter

  val basicAnalyzer: Analyzer = new Analyzer(basicInterpreter)
  val sourceAnalyzer: Analyzer = new Analyzer(sourceInterpreter)

  trait AnalyzeTransform extends Transform
			 with Function1[MethodInfo, Changes] {
    def analyzer(method: MethodInfo): Analyzer
    def apply(method: MethodInfo, frames: Array[Frame]): Changes

    def apply(method: MethodInfo): Changes =
      apply(method, analyzer(method).analyze(method.owner.name, method.node))

    def stack(frame: Frame): List[(Int, Value)] = {
      /*var depth = 0
      val size = frame.getStackSize
      val values = new Array[(Int, Value)](size)
      while (depth < size) {
	val value = frame.getStack(depth)
	values(depth) = (depth + value.getSize, value)
	depth += value.getSize
      }
      values.toList filter (_ != null)*/
      var depth = 0
      (0 until frame.getStackSize).toList map { i =>
	val value = frame.getStack(i)
	depth += value.getSize
        (depth, value)
      }
    }

    def stackDescs(frame: Frame): List[Option[String]] = stack(frame).map {
      case (_, value) => valueDesc(value)
    }

    def stackDescs(frame: Frame, fromDepth: Int): List[Option[String]] =
      stack(frame) dropWhile (_._1 <= fromDepth) map {
	case (_, value) => valueDesc(value)
      }

    def stackZeroBounds(frames: Array[Frame]): List[(Int, Int)] = {
      val zeros = for (i <- 0 until frames.length
		       if frames(i).getStackSize == 0) yield i
      (zeros zip zeros.tail).toList :+ (zeros.last, frames.length)
    }

    import BasicValue._
    def valueDesc(value: Value): Option[String] = value match {
      case INT_VALUE =>       Some("I"); case LONG_VALUE =>   Some("J")
      case FLOAT_VALUE =>     Some("F"); case DOUBLE_VALUE => Some("D")
      case REFERENCE_VALUE => None
      case _ => throw new RuntimeException("unknown value "+ value)
    }
  }

  trait AnalyzeBasicTransform extends AnalyzeTransform {
    def analyzer(method: MethodInfo) = basicAnalyzer
  }

  trait AnalyzeSourceTransform extends AnalyzeTransform {
    def analyzer(method: MethodInfo) = sourceAnalyzer
  }

  trait CFGTransform extends AnalyzeTransform {
    def interpreter: Interpreter
    def apply(method: MethodInfo,
	      frames: Array[Frame],
	      cfg: ControlFlowGraph): Changes

    def analyzer(method: MethodInfo) = method.cfgAnalyzer(interpreter)

    def apply(method: MethodInfo, frames: Array[Frame]): Changes =
      apply(method, frames, method.cfg)
  }
}

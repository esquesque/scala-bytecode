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

import asm._
import org.objectweb.asm.tree.analysis.Frame

/* Intended to abstract direct stack manipulation with var store/loads.
 * 
 * I don't think pop/pop2 need to be implemented but they can be annoying in
 * "creative" bytecode when they are misused along with dups. Note to self:
 * keep an eye out for such behavior.
 * 
 * TODO: swap
 */
object CollapseStackManipulations extends MethodInfo.AnalyzeBasicTransform {
  def apply(method: MethodInfo, frames: Array[Frame]): MethodInfo.Changes = {
    var curLocal = method.node.maxLocals
    def nextLocal(desc: Option[String]): Int = desc match {
      case Some(wide) if (wide equals "J") || (wide equals "D") =>
	curLocal += 2; curLocal - 2
      case _ => curLocal += 1; curLocal - 1
    }
    val insns = method.instructions
    val zBounds: List[(Int, Int)] = stackZeroBounds(frames) match {
      case Nil => (0, insns.length) :: Nil; case zb => zb
    }
    val smBoundsByZ: List[List[(Int, Int)]] = zBounds map (insns.search(_, 2) {
      case anew(_) :: dup() :: _ => 2
      case dup()  :: _ => 1; case dup_x1()  :: _ => 1; case dup_x2()  :: _ => 1
      case dup2() :: _ => 1; case dup2_x1() :: _ => 1; case dup2_x2() :: _ => 1
      //case swap() :: _ => 1
      case _ => 0
    } )
    println(zBounds)
    val spec: MethodInfo.InsnSpec = ((zBounds zip smBoundsByZ) map {
      case (zBound, smBounds) => (smBounds map { smBound =>
	val insn = insns(smBound._1)
	if (smBound._2 - smBound._1 == 1) {
	  val smFrames = frames.slice(smBound._1, zBound._2)
	  val headFrame = smFrames.head
	  val descs = stackDescs(headFrame)
	  val desc = descs.last
	  val loc = nextLocal(desc)
	  def wide(x: Int): Boolean = (descs.reverse(x) filter {
	    case "J" => true; case "D" => true; case _ => false
	  } ).isDefined
	  /*(insns.zipWithIndex.slice(smBound._1, zBound._2) zip smFrames) foreach {
	    case ((insn, idx), frame) =>
	      println(idx +": "+ insnString(insn) +" stack_size="+ frame.getStackSize)
	  }*/
	  def insertIdx(headStackOffset: Int): Int = {
	    val idx = (((1 until smFrames.length - 1) find { idx =>
	      val frame = smFrames(idx)
	      val next = smFrames(idx + 1)
	      frame.getStackSize - next.getStackSize ==
	      frame.getStackSize - headFrame.getStackSize + headStackOffset
//	      next.getStackSize == headFrame.getStackSize - stackMod
	    } ) match {
	      case Some(idx) =>
	        if (smFrames(idx).getStackSize - smFrames(idx + 1).getStackSize > 1)
		  smBound._1 + idx + 1
		else smBound._1 + idx
	      case None =>
		throw new RuntimeException("no eq stack frame w/i "+
					   smBound._1 +"..."+ zBound._2)
	    } )
	    //println("idx="+ idx)
	    idx
	  }
	  val w0 = wide(0)
	  insn match {
	    case dup() =>
	      smBound -> List(store(loc, desc),
			      load(loc, desc),
			      load(loc, desc)) :: Nil
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	    case dup_x1() =>
	      val idx = insertIdx(1)
	      smBound -> List(store(loc, desc),
			      load(loc, desc)) ::
	      (idx, idx) -> List(load(loc, desc)) :: Nil
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	    case dup_x2() =>
	      val idx = insertIdx(if (wide(1)) 1 else 2)
	      smBound -> List(store(loc, desc),
			      load(loc, desc)) ::
	      (idx, idx) -> List(load(loc, desc)) :: Nil
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	    case dup2() if w0 =>
	      smBound -> List(store(loc, desc),
			      load(loc, desc),
			      load(loc, desc)) :: Nil
	    case dup2() =>
	      val xdesc = descs.init.last
	      val xloc = nextLocal(xdesc)
	      smBound -> List(store(loc, desc),
			      store(xloc, xdesc),
			      load(xloc, xdesc),
			      load(loc, desc),
			      load(xloc, xdesc),
			      load(loc, desc)) :: Nil
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	    case dup2_x1() if w0 =>
	      val idx = insertIdx(1)
	      smBound -> List(store(loc, desc),
			      load(loc, desc)) ::
	      (idx, idx) -> List(load(loc, desc)) :: Nil
	    case dup2_x1() =>
	      val idx = insertIdx(1)
	      val xdesc = descs.init.last
	      val xloc = nextLocal(xdesc)
	      smBound -> List(store(loc, desc),
			      store(xloc, xdesc),
			      load(xloc, xdesc),
			      load(loc, desc)) ::
	      (idx, idx) -> List(load(xloc, xdesc),
				 load(loc, desc)) :: Nil
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	    case dup2_x2() if w0 && wide(1) =>
	      val idx = insertIdx(1)
	      smBound -> List(store(loc, desc),
			      load(loc, desc)) ::
	      (idx, idx) -> List(load(loc, desc)) :: Nil
	    case dup2_x2() if w0 && ! wide(1) =>
	      val idx = insertIdx(2)
	      smBound -> List(store(loc, desc),
			      load(loc, desc)) ::
	      (idx, idx) -> List(load(loc, desc)) :: Nil
	    case dup2_x2() if !w0 && wide(2) =>
	      val idx = insertIdx(1)
	      val xdesc = descs.init.last
	      val xloc = nextLocal(xdesc)
	      smBound -> List(store(loc, desc),
			      store(xloc, xdesc),
			      load(xloc, xdesc),
			      load(loc, desc)) ::
	      (idx, idx) -> List(load(xloc, xdesc),
				 load(loc, desc)) :: Nil
	    case dup2_x2() if !w0 && ! wide(2) =>
	      val idx = insertIdx(2)
	      val xdesc = descs.init.last
	      val xloc = nextLocal(xdesc)
	      smBound -> List(store(loc, desc),
			      store(xloc, xdesc),
			      load(xloc, xdesc),
			      load(loc, desc)) ::
	      (idx, idx) -> List(load(xloc, xdesc),
				 load(loc, desc)) :: Nil
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	    //case swap() =>
	    case _ => throw new RuntimeException("unimpl")
	  }
	} else {
/* Abstract the common case of allocating an object, duping it, and using one
 * copy to initialize the object, by using a pseudo-instruction "initnew" which
 * initializes when it is called and returns the object being initialized.
 * 
 * anew Lobject; dup [...] invokespecial(args)V -> [...] initnew(args)Lobject;
 */
	  val instance = insn match { case anew(inst) => inst }
	  val init = insns.search((smBound._2, zBound._2), 1) {
	    case invokespecial(_, "<init>", _) :: _ => 1; case _ => 0
	  }.head
	  val initnew0 = insns(init._1) match {
	    case invokespecial(owner, _, desc) =>
	      val desc0 = if (frames(init._2).getStackSize == 0) desc
			  else desc.init +'L'+ instance +';'
	      initnew(instance, owner, desc0)
	  }
	  (smBound -> Nil) :: (init -> (initnew0 :: Nil)) :: Nil
	}
      } ).flatten
    } ).flatten
    MethodInfo.Changes(None, Some(curLocal), spec)
  }
}

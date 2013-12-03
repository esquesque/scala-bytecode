package scala.bytecode

import asm._
import org.objectweb.asm.tree.analysis.Frame

object CollapseStackManipulations extends MethodInfo.AnalyzeBasicTransform {
  def apply(info: MethodInfo, frames: Array[Frame]): MethodInfo.Changes = {
    var curLocal = info.node.maxLocals
    def nextLocal(desc: Option[String]): Int = desc match {
      case Some(wide) if (wide equals "J") || (wide equals "D") =>
	curLocal += 2; curLocal - 2
      case _ => curLocal += 1; curLocal - 1
    }
    val zBounds: List[(Int, Int)] = stackZeroBounds(frames)
    val insns = info.instructions
    val smBoundsByZ: List[List[(Int, Int)]] = zBounds map (insns.search(_, 2) {
      case anew(_) :: dup() :: _ => 2
      case dup()  :: _ => 1; case dup_x1()  :: _ => 1; case dup_x2()  :: _ => 1
      case dup2() :: _ => 1; case dup2_x1() :: _ => 1; case dup2_x2() :: _ => 1
      //case pop() :: _ => 1
      //case pop2() :: _ => 1
      //case swap() :: _ => 1
      case _ => 0
    } )
    val spec: MethodInfo.InsnSpec = ((zBounds zip smBoundsByZ) map {
      case (zBound, smBounds) => (smBounds map { smBound =>
	val insn = insns(smBound._1)
        if (smBound._2 - smBound._1 > 1) {
	  //anew dup ... invokespecial -> ... initnew
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
	} else {
	  //dupx_xx
	  val dupFrames = frames.slice(smBound._1, zBound._2)
	  val headFrame = dupFrames.head
	  val descs = stackDescs(headFrame)
	  val desc = descs.last
	  val loc = nextLocal(desc)
	  def wide(x: Int): Boolean = (descs.reverse(x) filter {
	    case "J" => true; case "D" => true; case _ => false
	  } ).isDefined
	  def insertIdx(stackMod: Int): Int = smBound._1 +
	    dupFrames.zipWithIndex.tail.reverse.find(
	      _._1.getStackSize == headFrame.getStackSize + stackMod).get._2
	  insn match {
	    case dup() =>
	      smBound -> List(store(loc, desc),
			      load(loc, desc),
			      load(loc, desc)) :: Nil
	    case dup_x1() =>
	      val idx = insertIdx(0)
	      smBound -> List(store(loc, desc),
			      load(loc, desc)) ::
	      (idx, idx) -> List(load(loc, desc)) :: Nil
	    case dup_x2() =>
	      val idx = insertIdx(-1)
	      smBound -> List(store(loc, desc),
			      load(loc, desc)) ::
	      (idx, idx) -> List(load(loc, desc)) :: Nil
	    case dup2() if wide(0) =>
	      smBound -> List(store(loc, desc),
			      load(loc, desc),
			      load(loc, desc)) :: Nil
	    case dup2() =>
	      val xloc = nextLocal(desc)
	      smBound -> List(store(loc, desc),
			      store(xloc, desc),
			      load(xloc, desc),
			      load(loc, desc),
			      load(xloc, desc),
			      load(loc, desc)) :: Nil
	    case dup2_x1() if wide(0) =>
	      val idx = insertIdx(0)
	      smBound -> List(store(loc, desc),
			      load(loc, desc)) ::
	      (idx, idx) -> List(load(loc, desc)) :: Nil
	    case dup2_x1() =>
	      val idx = insertIdx(0)
	      val xloc = nextLocal(desc)
	      smBound -> List(store(loc, desc),
			      store(xloc, desc),
			      load(xloc, desc),
			      load(loc, desc)) ::
	      (idx, idx) -> List(load(xloc, desc),
				 load(loc, desc)) :: Nil
	    //fuck case dup2_x2() =>
	    case _ => throw new RuntimeException("unimpl")
	  }
	}
      } ).flatten
    } ).flatten
    MethodInfo.Changes(None, Some(curLocal), spec)
  }
}

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

import org.objectweb.asm.tree.analysis.Frame
import scala.collection.mutable.{BitSet, HashMap, MutableList, Stack}

import ast.Block

abstract class ControlFlowGraph(val method: MethodInfo) {
  def bounds: List[(Int, Int)]
  def edges: List[((Int, Int), (Int, Int))]
  def predecessors(beg: Int): List[(Int, Int)]
  def successors(end: Int): List[(Int, Int)]

  def predecessors(b: (Int, Int)): List[(Int, Int)] = predecessors(b._1)
  def successors(b: (Int, Int)): List[(Int, Int)] = successors(b._2)

  def mkblocks(frames: Array[Frame]): List[Block] = {
    var blocks: List[Block] = null
    val getblocks: () => List[Block] = () => blocks
    blocks = bounds.zipWithIndex map {
      case (bound, ord) =>
	new Block(ord, bound, method, frames, this, getblocks)
    }
    blocks
  }

  def singleExitReverse: ControlFlowGraph = new ControlFlowGraph(method) {
    val cfg = ControlFlowGraph.this
    val dummyExit: (Int, Int) = (-1, -1)
    val bounds = dummyExit :: cfg.bounds.reverse
    val exits: List[(Int, Int)] =
      (cfg.bounds filter (b => (cfg successors b).isEmpty)).reverse
    val edges = (bounds map (b => successors(b) map (b -> _))).flatten

    def predecessors(beg: Int) = (cfg predecessors beg).reverse
    def successors(end: Int) = (cfg successors end).reverse

    override def predecessors(b: (Int, Int)): List[(Int, Int)] =
      if (b == dummyExit) Nil
      else if (successors(b._2).isEmpty) List(dummyExit)
      else successors(b._2)
    override def successors(b: (Int, Int)): List[(Int, Int)] =
      if (b == dummyExit) exits else predecessors(b._1)
  }

/*  def undirected: ControlFlowGraph = new ControlFlowGraph(method) {
    val cfg = ControlFlowGraph.this
    val bounds = cfg.bounds
    val edges = (bounds map (b => predecessors(b._1) map (_ -> b))).flatten

    def predecessors(beg: Int) = cfg predecessors beg
    def successors(end: Int) = cfg successors end

    override def predecessors(b: (Int, Int)) =
      predecessors(b._1) ++ successors(b._2)
    override def successors(b: (Int, Int)) =
      predecessors(b._1) ++ successors(b._2)
  }*/

  type BracketList = Stack[(Int, Int)]

  case class Node(n: Int, b: (Int, Int), succs: List[(Int, Int)]) {
    var hi: Int = 0
    var blist: BracketList = new BracketList

    def isAncestorOf(t: Tree, m: Int): Boolean = (t parent m) match {
      case None => false
      case Some(parent) => if (parent.n == n) true else isAncestorOf(t, parent.n)
    }

    def isDescendantOf(t: Tree, m: Int): Boolean = (t parent n) match {
      case None => false
      case Some(parent) =>
	if (parent.n == m) true else parent.isDescendantOf(t, m)
    }

    override def toString = "node n="+ n +" b="+ b +" blist="+ blist
  }

  /* Vigdorchik
   * https://scfa.googlecode.com/svn/trunk/src/cfa/dfst.scala
   */
  abstract class Tree {
    def size: Int
    def pre(n: Int): Node
    def post(n: Int): Node
    def parent(n: Int): Option[Node]
    def edgeKinds: HashMap[(Int, Int), EdgeKind]

    def preorder: IndexedSeq[Node] = (0 until size) map (pre)
    def postorder: IndexedSeq[Node] = (0 until size) map (post)
    def parents: IndexedSeq[Option[Node]] = (0 until size) map (parent)

    def children(n: Int): IndexedSeq[Node] = preorder filter { node =>
      parent(node.n) match {
	case None => false; case Some(p) => p.n == n
      }
    }
  }

  lazy val dfst: Tree = {
    println(bounds)
    val pre0 = Array.fill(bounds.length)(-1)
    val post0 = Array.fill(bounds.length)(-1)
    val pre1 = new Array[Node](bounds.length)
    val post1 = new Array[Node](bounds.length)
    val ps = new Array[Node](bounds.length)
    val eks: HashMap[(Int, Int), EdgeKind] = HashMap.empty
    def catchBounds(beg: Int): List[(Int, Int)] =
      method.tryCatches filter(_._1 == beg) map (tc =>
	bounds.find(b => b._1 == tc._3).get)
    def f(nodes: List[Node], m: Int, n: Int): Int = nodes match {
      case Nil => m
      case node :: nt => node.succs match {
	case Nil =>
	  post0(node.n) = n
	  post1(n) = node
          f(nt, m, n - 1)
	case succ :: st =>
	  val sidx = bounds indexOf succ
          val sn = Node(sidx, succ, successors(succ) ++ catchBounds(succ._1))
	  val ssn = Node(node.n, node.b, st)
          if (pre0(sn.n) >= 0 && post0(sn.n) < 0) {
	    eks(node.n -> sidx) = Back
            f(ssn :: nt, m, n)
          } else if (pre0(sn.n) >= 0 && post0(sn.n) >= 0) {
	    eks(node.n -> sidx) =
	      if (pre0(node.n) > pre0(sn.n)) Cross else Forward
            f(ssn :: nt, m, n)
          } else {
	    pre0(sn.n) = m
	    pre1(m) = sn
	    ps(sn.n) = node
	    eks(node.n -> sidx) = Tree
            f(sn :: ssn :: nt, m + 1, n)
          }
      }
    }
    val entry = Node(0, bounds.head, successors(bounds.head) ++ catchBounds(0))
    pre0(0) = 0
    pre1(0) = entry
    new Tree {
      val size = f(entry :: Nil, 1, bounds.length - 1)
      assert(size == bounds.length)
      def pre(n: Int) = pre1(n)
      def post(n: Int) = post1(n)
      def parent(n: Int) = ps(n) match {
	case null => None
	case parent => Some(parent)
      }
      val edgeKinds = eks
    }
  }

  /* Johnson, Pearson, Pingali; Cornell
   * The Program Structure Tree: Computing Control Regions in Linear Time
   * 
   * Procedure CycleEquiv(G) {
   *   #perform an undirected depth-first search
   *   for each node n in reverse depth-first order {
   *     #compute n.hi
   *     #min returns infinity (i.e. N + 1) whenever set is empty.
   *     hi_0 := min { t.dfstnum | (n, t) is a backedge };
   *     hi_1 := min { c.hi | c is a child of n };
   *     n.hi := min { hi_0, hi_1 } ;
   *     hichild := any child c of n having c.hi = hi_1;
   *     hi_2 := min { c.hi | c is a child of n other than hichild };
   *
   *     #compute bracketlist
   *     n.blist := create();
   *     for each child c of n {
   *       n.blist := concat(c.blist, n.blist);
   *     }
   *     for each capping backedge d from a descendant of n to n {
   *       delete(n.blist, d)
   *     }
   *     for each backedge b from a descendant of n to n {
   *       delete(n.blist, b);
   *       if b.class undefined {
   *         b.class := new-class();
   *       }
   *     }
   *     for each backedge e from n to an ancestor of n {
   *       push(n.blist, e);
   *     }
   *     if hi_2 < hi_0 {
   *       #create capping backedge
   *       d := (n, node[hi_2]);
   *       push(n.blist, d);
   *     }
   *
   *     #determine class for edge from parent(n) to n
   *     if n is not the root of dfs tree {
   *       let e be the tree edge from parent(n) to n;
   *       b := top(n.blist)
   *       if b.recentSize != size(n.blist) {
   *         b.recentSize := size(n.blist)
   *         b.recentClass := new-class();
   *       }
   *       e.class := b.recentClass;
   *
   *       #check for e, b equivalence
   *       if b.recentSize = 1 {
   *         b.class := e.class
   *       }
   *     }
   *   }
   * }
   */

  //it doesn't work

/*  def cycleEquiv: HashMap[(Int, Int), Int] = {
    val cycleEquivClasses: HashMap[(Int, Int), Int] = HashMap.empty
    val bracketRecentSizes: HashMap[(Int, Int), Int] = HashMap.empty
    val bracketRecentClasses: HashMap[(Int, Int), Int] = HashMap.empty
    val bracketClasses: HashMap[(Int, Int), Int] = HashMap.empty

    val cappingBackEdges: MutableList[(Int, Int)] = MutableList.empty

    val tree = dfst

    val backEdges = tree.edgeKinds filter {
      case (_, Back) => true; case _ => false
    } map (_._1)

    val treeEdges = tree.edgeKinds filter {
      case (_, Tree) => true; case _ => false
    } map (_._1)

    def min(seq: Seq[Int]): Int = if (seq.isEmpty) tree.size else seq.min
    def delete(n: Node, e: (Int, Int)) { n.blist = n.blist filter (_ != e) }

    var curClass = 0

    def newClass: Int = { val c = curClass; curClass += 1; c }

    for (node <- tree.preorder.reverse) {
      val hi_0 = min(backEdges.toSeq filter {
	case (ord, _) if ord == node.n => true; case _ => false
      } map {
	case (_, ord) => tree.pre(ord).n//what is dfsnum?
      } )
      val children = tree children node.n
      val hi_1 = min(children map (_.hi))
      node.hi = min(hi_0 :: hi_1 :: Nil)
      val hichild = children find (_.hi == hi_1)
      val hi_2 = hichild match {
	case None => println("***"+ children);0
	case Some(hc) => min(children filter (_ != hc) map (_.hi))
      }
      println("hi_0="+ hi_0 +" hi_1="+ hi_1 +" hi_2="+ hi_2)

      //compute bracketlist
      children foreach { c =>
	node.blist = node.blist ++ c.blist
      }
      cappingBackEdges foreach { d =>
	if (d._2 == node.n && node.isAncestorOf(tree, d._1))
          node.blist = node.blist filter (_ != d)
      }
      backEdges foreach { b =>
	if (b._2 == node.n && node.isAncestorOf(tree, b._1)) {
          node.blist = node.blist filter (_ != b)
          if (! (bracketClasses contains b))
            bracketClasses(b) = newClass
	}
      }
      backEdges foreach { e =>
	if (e._1 == node.n && node.isDescendantOf(tree, e._2))
	  node.blist push e
      }
      if (hi_2 < hi_0) {
	//create capping backedge
	val d = (node.n, tree.pre(hi_2).n)
	println("capping backedge hi_2="+ hi_2 +" d="+ d)
	node.blist push d
	cappingBackEdges += d
      }

      println(node)

      //determine class for edge from parent(n) to n
      if (node.n != 0) {
	val e = (treeEdges find {
	  case (from, to) => tree.parent(to).get.n == from
	} ).get
	val b = node.blist.top
	val size = node.blist.size
	if (! (bracketRecentSizes contains b)) bracketRecentSizes(b) = 0
	if (bracketRecentSizes(b) != size) {
	  bracketRecentSizes(b) = size
	  bracketRecentClasses(b) = newClass
	}
	bracketClasses(e) = bracketRecentClasses(b)

	//check for e,b equivalence
	if (bracketRecentSizes(b) == 1)
	  bracketClasses(b) = bracketClasses(e)
      }
    }

    bracketClasses
  }*/

  /* Cooper, Harvey, Kennedy; Rice
   * A Simple, Fast Dominance Algorithm
   * www.hipersoft.rice.edu/grads/publications/dom14.pdf
   */
  lazy val immediateDominators: Array[(Int, Int)] = {
    val tree = dfst
    val bs = bounds
    val rpo = tree.postorder.reverse
    //val rpons = rpo map (_.n)
    val rpobs = rpo map (_.b)
    val idoms = new Array[(Int, Int)](tree.size)
    idoms(0) = bs(0)
    var changed = true
    while (changed) {
      changed = false
      for (n <- rpo.init) {
	var idom: (Int, Int) = null
	for (pred <- predecessors(n.b) if idoms(bs indexOf pred) != null) {
	  if (idom == null)
	    idom = pred
	  else {
	    var b1 = idom
	    var b2 = pred
	    while (b1 != b2) {
	      while ((rpobs indexOf b1) < (rpobs indexOf b2))
		b1 = idoms(bs indexOf b1)
	      while ((rpobs indexOf b2) < (rpobs indexOf b1))
		b2 = idoms(bs indexOf b2)
	    }
	    idom = b1
	  }
	}
	if (idoms(n.n) != idom) {
	  idoms(n.n) = idom
	  changed = true
	}
      }
    }
    idoms

    //magick
    //this subfunction required quite a bit deviation from the pseudocode
/*    def intersect(b1: (Int, Int), b2: (Int, Int)): (Int, Int) = {
      var f1 = rpobs indexOf b1
      var f2 = rpobs indexOf b2
      var lf = -1
      while (f1 != f2) {
	while (f1 < f2) {
	  val f = if (doms(f1) == null) 0 else rpobs indexOf doms(f1)
	  f1 = if (lf == f1) f2 else f
	  lf = f
	}
	lf = -1
	while (f2 < f1) {
	  val f = if (doms(f2) == null) 0 else rpobs indexOf doms(f2)
	  f2 = if (lf == f2) f1 else f
	  lf = f
	}
	lf = -1
      }
      bs(rpons(f1))
      //////////////////////////////////////////////////////////////////////////
      var idx1 = bounds indexOf b1
      var idx2 = bounds indexOf b2
      println("intersect "+ idx1 +", "+ idx2)
      while ((tree post idx1).n != (tree post idx2).n) {
	while ((tree post idx1).n < (tree post idx2).n)
	  idx1 = bounds indexOf doms(idx1)
	while ((tree post idx2).n < (tree post idx1).n)
	  idx2 = bounds indexOf doms(idx2)
      }
      bounds(idx1)
    }*/

/*	val preds = predecessors(n.b)
	var idom = preds find (p => proc(bs indexOf p)) getOrElse {
	  if (preds.length == 0) {
	    val tc = method.tryCatches.find(_._3 == n.b._1).get
	    bs.find(_._1 == tc._1).get
	  } else preds(0)
	}
	for (p <- preds if !(p equals idom) && doms(bs indexOf p) != null) {
	  idom = intersect(p, idom)
	}*/
  }

  lazy val immediatelyDominatedBy: Array[BitSet] = {
    val idomd = Array.fill(bounds.length)(BitSet.empty)
    for (i <- 0 until bounds.length)
      idomd(bounds indexOf immediateDominators(i)) += i
    idomd
  }

  lazy val dominanceFrontiers: Array[BitSet] = {
    val dfs = Array.fill(bounds.length)(BitSet.empty)
    for (idx <- 0 until bounds.length) {
      val preds = predecessors(bounds(idx))
      if (preds.length > 1)
	for (pred <- preds) {
	  var run = pred
	  while (run != immediateDominators(idx)) {
	    val runIdx = bounds indexOf run
	    dfs(runIdx) += idx
	    run = immediateDominators(runIdx)

	    /*if (dominators(runIdx) != run) {
	      dfs(runIdx) += idx
	      run = dominators(runIdx)
	    } else run = dominators(idx)*/
	  }
	}
    }
    dfs
  }
}

package scala.bytecode.asm

import org.objectweb.asm.tree.InsnList

import scala.collection.JavaConversions._
import scala.collection.mutable.Buffer

class RichInsnList(val insnList: InsnList) extends Buffer[Insn] {
  def +=(insn: Insn): this.type = { insnList.add(insn); this }

  def +=:(insn: Insn): this.type = { insnList.insert(insn); this }

  def apply(i: Int): Insn = insnList.get(i)

  def clear { insnList.clear }

  def insertAll(i: Int, insns: Traversable[Insn]) {
    val il = new InsnList
    insns foreach (il add _)
    insnList.insertBefore(apply(i), il)
  }

  def iterator: Iterator[Insn] =
    asScalaIterator(insnList.iterator).asInstanceOf[Iterator[Insn]]

  def length: Int = insnList.size

  def remove(i: Int): Insn = {
    val insn = apply(i)
    insnList.remove(insn)
    insn
  }

  def update(i: Int, insn: Insn) {
    val insn0 = apply(i)
    insnList.set(insn0, insn)
  }

  def search(bound: (Int, Int), lim: Int)(
    func: List[Insn] => Int): List[(Int, Int)] = {
    def srch(idx: Int, res: List[(Int, Int)]): List[(Int, Int)] = {
      if (idx >= bound._2) res
      else {
	val n = func(slice(idx, idx + lim).toList)
	if (n > 0) srch(idx + n, (idx, idx + n) :: res)
	else       srch(idx + 1,                   res)
      }
    }
    srch(bound._1, Nil).reverse
  }

  def haveSideEffects(bound: (Int, Int)): Boolean =
    slice(bound._1, bound._2) map insnHasSideEffects reduce (_ | _)

  //spec must be sorted by range, or else undefined behavior
  //insns already belonging to the list need to be cloned for translocation
  //should prolly do something about this
  def mutate(spec: List[((Int, Int), List[Insn])]) = {
    var offset = 0
    (spec sortWith (_._1._1 < _._1._1)).foreach {
      case ((beg, end), insns) =>
	(beg until end) foreach (_ => remove(beg + offset))
	insertAll(beg + offset, insns)
	offset += insns.length - (end - beg)
    }
  }

  def out(ps: java.io.PrintStream = System.out) {
    map(insnString).zipWithIndex foreach {
      case (str, idx) => ps.println(idx +": "+ str)
    }
  }
}

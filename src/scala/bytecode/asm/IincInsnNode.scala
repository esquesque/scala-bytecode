package scala.bytecode.asm

import org.objectweb.asm.tree.{IincInsnNode => I, AbstractInsnNode => N}

object IincInsnNode {
  def apply(v: Int, n: Int): I = new I(v, n)

  def unapply(n: N): Option[(Int, Int)] = n match {
    case i: I => Some(i.`var`, i.incr); case _ => None
  }
}

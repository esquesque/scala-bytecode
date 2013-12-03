package scala.bytecode.asm

import org.objectweb.asm.tree.{VarInsnNode => V, AbstractInsnNode => N}

object VarInsnNode {
  def apply(opcode: Int, v: Int): V = new V(opcode, v)

  def unapply(n: N): Option[(Int, Int)] = n match {
    case v: V => Some(v.getOpcode, v.`var`); case _ => None
  }
}

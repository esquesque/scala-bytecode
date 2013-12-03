package scala.bytecode.asm

import org.objectweb.asm.tree.{TypeInsnNode => T, AbstractInsnNode => N}

object TypeInsnNode {
  def apply(opcode: Int, desc: String): T = new T(opcode, desc)

  def unapply(n: N): Option[(Int, String)] = n match {
    case t: T => Some(t.getOpcode, t.desc); case _ => None
  }
}

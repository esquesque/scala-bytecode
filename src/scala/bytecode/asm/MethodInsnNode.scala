package scala.bytecode.asm

import org.objectweb.asm.tree.{MethodInsnNode => M, AbstractInsnNode => N}

object MethodInsnNode {
  def apply(opcode: Int, owner: String, name: String, desc: String): M =
    new M(opcode, owner, name, desc)

  def unapply(n: N): Option[(Int, String, String, String)] = n match {
    case m: M => Some(m.getOpcode, m.owner, m.name, m.desc); case _ => None
  }
}

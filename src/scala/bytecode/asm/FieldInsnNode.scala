package scala.bytecode.asm

import org.objectweb.asm.tree.{FieldInsnNode => F, AbstractInsnNode => N}

object FieldInsnNode {
  def apply(opcode: Int, owner: String, name: String, desc: String): F =
    new F(opcode, owner, name, desc)

  def unapply(n: N): Option[(Int, String, String, String)] = n match {
    case f: F => Some(f.getOpcode, f.owner, f.name, f.desc); case _ => None
  }
}

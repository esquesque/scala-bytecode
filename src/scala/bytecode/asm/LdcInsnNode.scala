package scala.bytecode.asm

import org.objectweb.asm.tree.{LdcInsnNode => L, AbstractInsnNode => N}

object LdcInsnNode {
  def apply(const: Any): L = new L(const)

  def unapply(n: N): Option[Any] = n match {
    case l: L => Some(l.cst); case _ => None
  }
}

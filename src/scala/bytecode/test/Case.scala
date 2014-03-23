package scala.bytecode.test

import org.objectweb.asm.tree.{AbstractInsnNode => Insn, MethodNode}
import scala.bytecode.{Info, ClassInfo, MethodInfo}
import scala.bytecode.asm.RichInsnList
import scala.bytecode.ast.MethodDecl

trait Case[A] extends Function0[Boolean] {
  type Test = A => Boolean

  def maxStack: Int
  def maxLocals: Int
  def name: String
  def desc: String
  def insns: RichInsnList

  def tryCatches: List[((Int, Int), Int, Option[String])] = Nil

  lazy val method = {
    val m = ClassInfo.anonymous.newMethod('static)(
      name, desc, insns)
    m.node.maxStack = maxStack
    m.node.maxLocals = maxLocals
    tryCatches foreach m.addTryCatch
    m
  }

  def test: Test
  def apply: Boolean
}

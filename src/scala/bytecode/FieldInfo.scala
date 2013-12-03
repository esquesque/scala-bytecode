package scala.bytecode

import org.objectweb.asm.Opcodes._
import org.objectweb.asm.tree.FieldNode

class FieldInfo(val cxt: Cxt, val owner: ClassInfo, val node: FieldNode)
extends MemberInfo[FieldNode, ast.FieldDecl] {
  lazy val tree: ast.FieldDecl = null

  val modifiers = List[(Int, Symbol)](
    ACC_PUBLIC    -> 'public,
    ACC_PRIVATE   -> 'private,
    ACC_PROTECTED -> 'protected,
    ACC_STATIC    -> 'static,
    ACC_FINAL     -> 'final,
    ACC_VOLATILE  -> 'volatile,
    ACC_TRANSIENT -> 'transient,
    ACC_SYNTHETIC -> 'synthetic,
    ACC_ENUM      -> 'enum) map {
      case (mod, sym) if (mod & node.access) != 0 => Some(sym)
      case _ => None
    } filterNot (_.isEmpty) map (_.get)

  val name = node.name
  val desc = node.desc
  val verbose = owner.name +"/"+ name +" "+ desc

  val init: Option[Any] = if (node.value == null) None else Some(node.value)

  /*def dump(writer: java.io.Writer, indent: Int) {
    writer append " "*indent
    if (modifiers.nonEmpty) {
      writer append modifiers.map(_.name).mkString(" ")
      writer append " "
    }
    writer append string
    writer append "\n"
    writer.flush
  }*/
}

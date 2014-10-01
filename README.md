scala-bytecode
==============

## Facile bytecode analysis, disassembling, and manipulation in Scala.

### Built on top of ObjectWeb ASM 4.0.

# Bytecode in a Can

####Notice:

  This work is incomplete. Please exercise caution.

* scala.bytecode

  A base library for loading and manipulating bytecode data structures with
  ASM. The master object is Cxt which is much like a ClassLoader, keeping track
  of relevant classes and their relationships.

  These are the main entrypoints to the library:
  ```scala
  Cxt.default: Cxt
  Cxt.resolve(node: ClassNode ~
              name: String ~
              bytes: Array[Byte] ~
              is: InputStream ~
              f: File): ClassInfo
  Cxt.resolveDir(dir: File): List[ClassInfo]
  Cxt.resolveJar(jf: java.util.jar.JarFile): List[Either[ClassInfo, InputStream]]
  ```

  ClassInfo, MethodInfo, and FieldInfo are the main objects of analysis (ASM
  wrappers) and translation into intermediate-representation (IR) and beyond.

  Functions useful for code analysis are:
  ```scala
  MethodInfo.cfg: ControlFlowGraph
  ControlFlowGraph.mkblocks(frames: Array[org.objectweb.asm.tree.analysis.Frame]): List[ast.Block]
  ast.Block.predecessors: List[Block]
  ast.Block.successors: List[Block]
  ast.Block.edgesIn: List[(Block, EdgeKind)]
  ast.Block.edgesOut: List[(Block, EdgeKind)]
  ast.Block.parent: Option[Block]
  ast.Block.children: List[Block]
  ast.Block.immediateDominator: Option[Block]
  ast.Block.dominators: List[Block]
  ast.Block.immediatelyDominated: List[Block]
  ast.Block.immediatePostdominator: Option[Block]
  ast.Block.postdominators: List[Block]
  ast.Block.dominates(block: Block): Boolean
  ast.Block.strictlyDominates(block: Block): Boolean
  ast.Block.immediatelyDominates(block: Block): Boolean
  ast.Block.postdominates(block: Block): Boolean
  ast.Block.strictlyPostdominates(block: Block): Boolean
  ast.Block.immediatelyPostdominates(block: Block): Boolean
  ast.Block.dominanceFrontier: List[Block]
  Info.tree[Tree <: ast.AST]
  ```

  Descendants of the trait ast.AST are pattern-matchable case classes. See
  contents of the test package for examples of what this looks like.

  Includes bytecode transforms essential for well-behaved IR:

  CollapseStackManipulations: transforms dup* stacks out into local store/loads
  CollapseTernaryExprs: transforms ternary expression stacks out into stmts/
                        local store/loads
  AnchorFloatingStmts: tares statements that are "floating" in a non-0 stack and
                       moves locals accordingly; use after CollapseTernaryExprs

- - - -

* scala.bytecode.asm

  A set of convenience methods and extractor-object wrappers for ASM's
  instruction library. Allows easy declaration and matching of bytecode.

  Example:
  ```scala
  import scala.bytecode.asm._

  //System.out.println((2*2)*2-1);
  val instructions = insnList(
    getfield("java/lang/System", "out", "Ljava/io/PrintStream;"),
    ipush(2), dup(), imul(),
    ipush(2), imul(),
    ipush(1), isub(),
    invokevirtual("java/io/PrintStream", "println", "(I)V"))
  (instructions map insnString).zipWithIndex foreach {
    case (str, idx) => println(idx +": "+ str)
  }
  ```

- - - -

* scala.bytecode.ast

  A cogent, concise, and pattern-matchable AST IR for bytecodes. Still under
  development.

  Example:
  ```scala
  import scala.bytecode._

  val classInfo = Cxt.default resolve new java.io.File("x.class")
  val classDecl = classInfo.tree
  classDecl.out()//print Java-style IR
  ```

  Things that work: if short-circuit structuring; if-else structuring

  Things slated for development: loop support, try-catch-finally support

- - - -

* scala.bytecode.test

  Testing framework to verify the datums.

  Example:
  ```scala
  //file src/scala/bytecode/test/ifs/org_objectweb_asm_Frame_merge_cw_t_types_index.scala

  package scala.bytecode.test.ifs

  import scala.bytecode.asm._
  import scala.bytecode.ast._

  object org_objectweb_asm_Frame_merge_cw_t_types_index
  extends scala.bytecode.test.ASTCase {
    val maxStack = 5
    val maxLocals = 7
    val name = "org_objectweb_asm_Frame_merge_cw_t_types_index"

    val test: Test = {
      case Exec(LocalStore(_, _) ::
                If(_,
                   Then(Return(_) ::
                        Nil)) ::
                Label(_) ::
                If(_,
                   Then(If(_,
                           Then(Return(_) ::
                                Nil)) ::
                        Label(_) ::
                        LocalStore(_, _) ::
                        Nil)) ::
                Label(_) ::
                If(_,
                   Then(ArrayStore(_, _, _) ::
                        Return(_) ::
                        Nil)) ::
                Label(_) ::
                If(Or(_, _),
                   Then(Label(_) ::
                        If(_,
                           Then(Return(_) ::
                                Nil)) ::
                        Label(_) ::
                        If(_,
                           Then(If(_,
                                   Then(LocalStore(_, _) ::
                                        Goto(_, _) ::
                                        Nil),
                                   Else(Label(_) ::
                                        LocalStore(_, _) ::
                                        Goto(_, _) ::
                                        Nil)) ::
                                Nil),
                           Else(Label(_) ::
                                If(Or(_, _),
                                   Then(Label(_) ::
                                        LocalStore(_, _) ::
                                        Goto(_, _) ::
                                        Nil),
                                   Else(Label(_) ::
                                        LocalStore(_, _) ::
                                        Goto(_, _) ::
                                        Nil)) ::
                                Nil)) ::
                        Nil),
                   Else(Label(_) ::
                        If(_,
                           Then(If(Or(_, _),
                                   Then(Label(_) ::
                                        LocalStore(_, _) ::
                                        Goto(_, _) :: Nil),
                                   Else(Label(_) ::
                                        LocalStore(_, _) :: Nil)) ::
                                Label(_) ::
                                LocalStore(_, Phi(_, _)) ::
                                LocalStore(_, _) ::
                                Goto(_, _) :: Nil),
                           Else(Label(_) ::
                                LocalStore(_, _) :: Nil)) :: Nil)) ::
                Label(_) ::
                LocalStore(_, Phi(_, _, _, _, _, _)) ::
                If(_,
                   Then(ArrayStore(_, _, _) ::
                        Return(_) ::
                        Nil)) ::
                Label(_) ::
                Return(_) ::
                Nil) => true
      case tree => false
    }

    val desc = "(Lorg/objectweb/asm/ClassWriter;I[II)Z"
    val insns = {
      val lbl0 = label()
      val lbl1 = label()
      val lbl2 = label()
      val lbl3 = label()
      val lbl4 = label()
      val lbl5 = label()
      val lbl6 = label()
      val lbl7 = label()
      val lbl8 = label()
      val lbl9 = label()
      val lbl10 = label()
      val lbl11 = label()
      val lbl12 = label()
      val lbl13 = label()
      val lbl14 = label()
      val lbl15 = label()
      val lbl16 = label()
      insnList(
  //#0 0..7
        aload(2),
        iload(3),
        array.iload(),
        istore(4),
        iload(4),
        iload(1),
        if_icmpne(lbl0),
  //#1 7..9
        push(0),
        ireturn(),
  //#2 9..15
        lbl0,
        iload(1),
        push(268435455),
        iand(),
        push(16777221),
        if_icmpne(lbl2),
  //#3 15..18
        iload(4),
        push(16777221),
        if_icmpne(lbl1),
  //#4 18..20
        push(0),
        ireturn(),
  //#5 20..23
        lbl1,
        push(16777221),
        istore(1),
  //#6
        lbl2,
        iload(4),
        ifne(lbl3),
  //#7
        aload(2),
        iload(3),
        iload(1),
        array.istore(),
        push(1),
        ireturn(),
  //#8
        lbl3,
        iload(4),
        push(267386880),
        iand(),
        push(24117248),
        if_icmpeq(lbl4),
  //#9
        iload(4),
        push(-268435456),
        iand(),
        ifeq(lbl10),
  //#10
        lbl4,
        iload(1),
        push(16777221),
        if_icmpne(lbl5),
  //#11
        push(0),
        ireturn(),
  //#12
        lbl5,
        iload(1),
        push(-1048576),
        iand(),
        iload(4),
        push(-1048576),
        iand(),
        if_icmpne(lbl7),
  //#13
        iload(4),
        push(267386880),
        iand(),
        push(24117248),
        if_icmpne(lbl6),
  //#14
        iload(1),
        push(-268435456),
        iand(),
        push(24117248),
        ior(),
        aload(0),
        iload(1),
        push(1048575),
        iand(),
        iload(4),
        push(1048575),
        iand(),
        invokevirtual("org/objectweb/asm/ClassWriter", "a", "(II)I"),
        ior(),
        istore(5),
        goto(lbl15),
  //#15
        lbl6,
        push(24117248),
        aload(0),
        push("java/lang/Object"),
        invokevirtual("org/objectweb/asm/ClassWriter", "c", "(Ljava/lang/String;)I"),
        ior(),
        istore(5),
        goto(lbl15),
  //#16
        lbl7,
        iload(1),
        push(267386880),
        iand(),
        push(24117248),
        if_icmpeq(lbl8),
  //#17
        iload(1),
        push(-268435456),
        iand(),
        ifeq(lbl9),
  //#18
        lbl8,
        push(24117248),
        aload(0),
        push("java/lang/Object"),
        invokevirtual("org/objectweb/asm/ClassWriter", "c", "(Ljava/lang/String;)I"),
        ior(),
        istore(5),
        goto(lbl15),
  //#19
        lbl9,
        push(16777216),
        istore(5),
        goto(lbl15),
  //#20
        lbl10,
        iload(4),
        push(16777216),
        if_icmpne(lbl14),
  //#21
        iload(1),
        push(267386880),
        iand(),
        push(24117248),
        if_icmpeq(lbl11),
  //#22
        iload(1),
        push(-268435456),
        iand(),
        ifeq(lbl12),
  //#23
        lbl11,
        iload(1),
        goto(lbl13),
  //#24
        lbl12,
        push(16777216),
  //#25
        lbl13,
        istore(5),
        goto(lbl15),//201->208
  //#26
        lbl14,
        push(16777216),//204
        istore(5),
  //#27
        lbl15,//208
        iload(4),
        iload(5),
        if_icmpeq(lbl16),//212->222
  //#28
        aload(2),
        iload(3),
        iload(5),
        array.istore(),
        push(1),
        ireturn(),
  //#29
        lbl16,//222
        push(0),
        ireturn())
    }
  }
  ```
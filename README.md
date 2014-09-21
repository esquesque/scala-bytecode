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
  instruction library. Allows easy matching and declaration of bytecode.

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

  A cogent, concise, and pattern-matchable abstract syntax tree IR for bytecodes
  Still under development.

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

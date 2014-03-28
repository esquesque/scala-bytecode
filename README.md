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
  `Cxt.resolve (ClassNode/name: String/Array[Byte]/InputStream/File): ClassInfo`

  `Cxt.resolveDir(File): List[ClassInfo]`

  `Cxt.resolveJar(java.util.jar.JarFile): Either[ClassInfo, InputStream]`

    ClassInfo, MethodInfo, and FieldInfo are the main objects of analysis (ASM
  wrappers) and translation into intermediate-representation (IR) and beyond (?)

    Includes bytecode transforms essential for well-behaved IR:

  *CollapseStackManipulations
    *transforms dup stacks out into local store/loads

  *CollapseTernaryExprs
    *transforms ternary expression stacks out onto local store/loads

  *AnchorFloatingStmts
    *tares statements that are stranded in a non-0 stack

    ControlFlowGraph contains a few procedural methods, which I am not proud of.
  I think it works.

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

* scala.bytecode.ast

    A cogent, concise, and pattern-matchable abstract syntax tree IR for
  bytecodes. Still under development.

  Example:
```scala
    import scala.bytecode._

    val classInfo = Cxt.default resolve new java.io.File("x.class")
    val classDecl = classInfo.tree
    classDecl.out()//print Java-style IR
```

  *Things that work:
    *if short-circuit structuring

  *Things that kind of work:
    *proper else-if structuring

  *Things that don't work (yet):
    *try-catch-finally support

  >while and for loop support

* scala.bytecode.test

    Testing framework to verify the datums.

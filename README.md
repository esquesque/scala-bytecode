scala-bytecode
==============

Facile bytecode analysis, disassembling, and manipulation in Scala.

Built on top of ObjectWeb ASM 4.0.

Still hypothetically but quite possibly this library could be used to decompile even badly-behaved classfiles.
First, CollapseStackManipulations is applied to a MethodInfo to transform dups and such into local var manipulations.
Then, AnchorFloatingStmts is applied to get rid of "floating" stmts created by CollapseStackManipulations or existing in
ordinary code. If this transform is not applied, stmts existing within other stmts will be ignored and code will be wrong.
TODO -- redundant vars can be eliminated in some situations such as: iload(x), istore(y), ... iload(y) -> ... iload(x)

Next, the AST is created. TODO -- apply transform to the AST to obtain short-circuit or and and if stmts;
apply transform to that AST to obtain for and while stmts. Somewhere in there a dominance frontier analysis is done
to determine which var stores are actually declarations.



package scala.bytecode.test

package object try_catches {
  val cases: List[ASTCase] =
    try_catch ::
    try_catch___if ::
    try_if_catch ::
    Nil
}

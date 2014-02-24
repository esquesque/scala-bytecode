package scala.bytecode.test

package object ifs {
  val cases: List[ASTCase] =
    //if_then ::
    //if_else ::
    if_and ::
    if_and_else ::
    if_or ::
    if_or_else ::
//
    if_and_and ::
    //if_and_and_else ::
    if_or_or ::
    //if_or_or_else ::
//
    if_And_or ::
    //if_And_or_else ::
    if_and_Or ::
    //if_and_Or_else ::
//
    if_Or_and ::
    //if_Or_and_else ::
    if_or_And ::
    //if_or_And_else ::
//
    //how many combinations of 4-cond 3-short-circuits are there?
    Nil
}

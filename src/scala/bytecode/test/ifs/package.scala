package scala.bytecode.test

package object ifs {
  val cases: List[ASTCase] =
    if_then ::
    //if_else ::
    if_and ::
    if_and_else ::
    if_or ::
    if_or_else ::
//
    if_and_and ::
    if_or_or ::
//
    if_And_or ::
    if_and_Or ::
    if_Or_and ::
    if_or_And ::
//
    //how many combinations of 4-cond 3-short-circuits are there?
    if_and_and_and ::
    if_or_or_or ::
//
    if_and_Or_AND ::
    if_AND_Or_and ::
    if_and_Or_and ::
    if_And_or_And ::
    Nil
}

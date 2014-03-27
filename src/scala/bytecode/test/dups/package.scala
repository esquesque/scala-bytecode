package scala.bytecode.test

package object dups {
  val cases: List[InsnsCase] =
    dup_init_asto                ::
    arg_dup_init_asto_init       ::
    dup_2isto                    ::
    iadd_dup_2isto               ::
    dup_iadd_isto                ::
    dup_x1_2iadd_isto            ::
    dup_x2_3iadd_isto            ::
    sfx_dup_x1_2iadd_isto        ::
    sfx_dup_x2_3iadd_isto        ::
    dup2_2lsto                   ::
    ladd_dup2_2lsto              ::
    dup2_ladd_lsto               ::
    dup2_4isto                   ::
    _2iadd_dup2_4isto            ::
    dup2_x1_iadd_ladd_lsto       ::
    dup2_x1_4iadd_isto           ::
    sfx_2iadd_dup2_4isto         ::
    sfx_dup2_x1_iadd_ladd_lsto   ::
    sfx_dup2_x1_4iadd_isto       ::
    dup2_x2_2ladd_lsto           :://JJ
    dup2_x2_iadd_ladd_2iadd_isto :://JII
    dup2_x2_2iadd_ladd_lsto      :://IIJ
    dup2_x2_5iadd_isto           :://IIII
    dup_dup_x1                   ::
    Nil
}

package scala.bytecode.test

package object ifs {
/*I have picked a random sequence off OEIS that fit the experimental data I have
 *derived about this hellish kind of combinatorial logic:

 *(A162213) a(n) = the smallest positive multiple of n with exactly n digits
 *                 when written in binary.

 *So how many combinations of 4-bool 3-operation SCs are there?
 *Answer: 4 associativities (with 6 demonstrable duplicates due to adjacent
 *        (and)s or (or)s) for each combination of which there are demonstrably 6,
 *        plus (and_and_and) and (or_or_or) which gives 4*6-6+2=20=a(5)
 */
  val cases: List[ASTCase] =
//n=1 1-bool
//a(1)=1 0b1
    if_then ::
    //if_else ::
//n=2 2-bool 1-operation short-circuits (SCs)
//a(2)=2 0b10 
    if_and ::
    if_and_else ::
    if_or ::
    if_or_else ::
//n=3 3-bool 2-operation SCs
//a(3)=2^2+2=6 0b110
    if_and_and ::
    if_or_or ::
    if_And_or ::
    if_and_Or ::
    if_Or_and ::
    if_or_And ::
//n=5 4-bool 3-operation SCs
// ...why n=5 and not 4? (this would only give 8 accd. to the sequence)
//a(5)=2^2^2+2^2=20 0b10100 0x14
    if_and_and_and ::
    if_or_or_or ::
    if_AND_Or_and ::
    if_and_Or_AND ::
    if_And_or_And ::
    if_and_Or_and ::
    if_OR_And_and :: Nil
    //if_or_And_AND ::

/*a(n)
 *  1 1
 *  2 2
 *  3 6
 *  4 8
 *  5 20
 *  6 36
 *  7 70
 *  8 128
 *  9 261
 *  10 520
 *  11 1034
 *  12 2052
 *  13 4108
 *  14 8204
 *  15 16395
 *  16 32768
 *  17 65552
 *  18 131076
 *  19 262162
 *  20 524300
 *  21 1048593
 *  22 2097172
 *  23 4194326
 *  24 8388624
 *  25 16777225
 *  26 33554456
 *  27 67108878
 *  28 134217748
 *  29 268435484
 *  30 536870940
 *  31 1073741854
 *  32 2147483648
 *I really don't know how this scales, it's still being investigated.
 *2^2^2^2+2^2^2=65552=a(17) which would give n=[1,2,3,5,17] which are all prime.
 */
}

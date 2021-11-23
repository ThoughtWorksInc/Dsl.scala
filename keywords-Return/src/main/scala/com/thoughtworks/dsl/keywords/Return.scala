package com.thoughtworks.dsl
package keywords
import Dsl.IsKeyword
import Dsl.Lift
import Dsl.Typed

/** A [[Dsl.Keyword]] to early return a lifted value from the enclosing function.
  *
  * @author
  *   杨博 (Yang Bo)
  * @example
  *   Suppose you are generating a random integer less than 100, whose first digit and second digit is different. A
  *   solution is generating integers in an infinite loop, and [[Return]] from the loop when the generated integer
  *   conforms with requirements.
  *
  * {{{
  *           import scala.util.Random
  *           import scala.util.control.TailCalls
  *           import scala.util.control.TailCalls.TailRec
  *           def randomInt(): TailRec[Int] = {
  *             while (true) {
  *               val r = Random.nextInt(100)
  *               if (r % 10 != r / 10) {
  *                 !Return(TailCalls.done(r))
  *               }
  *             }
  *             throw new AssertionError("Unreachable code");
  *           }
  *
  *           val r = randomInt().result
  *           r should be < 100
  *           r % 10 should not be r / 10
  * }}}
  *
  * @example
  *   Since this [[Return]] keyword can automatically lift the return type, `TailCalls.done` can be omitted.
  *
  * {{{
  *           import scala.util.Random
  *           import scala.util.control.TailCalls
  *           import scala.util.control.TailCalls.TailRec
  *           def randomInt(): TailRec[Int] = {
  *             while (true) {
  *               val r = Random.nextInt(100)
  *               if (r % 10 != r / 10) {
  *                 !Return(r)
  *               }
  *             }
  *             throw new AssertionError("Unreachable code");
  *           }
  *
  *           val r = randomInt().result
  *           r should be < 100
  *           r % 10 should not be r / 10
  * }}}
  */
opaque type Return[ReturnValue] = ReturnValue
object Return {
  @inline def cast[ReturnValue]: ReturnValue <:< Return[ReturnValue] = implicitly

  @inline def apply[ReturnValue]: ReturnValue =:= Return[ReturnValue] = summon[ReturnValue =:= Return[ReturnValue]]
  given [ReturnValue]: IsKeyword[Return[ReturnValue], Nothing] with {}

  given [ReturnValue, Domain](using lift: Lift[ReturnValue, Domain]): Dsl[Return[ReturnValue], Domain, Nothing] with {
    def cpsApply(keyword: Return[ReturnValue], handler: Nothing => Domain): Domain = {
      lift(keyword)
    }
  }
}

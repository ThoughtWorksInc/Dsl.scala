package com.thoughtworks.dsl.keywords

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.Keyword

import scala.language.implicitConversions

/** A [[Dsl.Keyword]] to early return a lifted value from the enclosing function.
  *
  * @author 杨博 (Yang Bo)
  * @example Suppose you are generating a random integer less than 100,
  *          whose first digit and second digit is different.
  *          A solution is generating integers in an infinite loop,
  *          and [[Return]] from the loop when the generated integer conforms with requirements.
  *
  *          {{{
  *          import scala.util.Random
  *          import scala.util.control.TailCalls
  *          import scala.util.control.TailCalls.TailRec
  *          def randomInt(): TailRec[Int] = {
  *            while (true) {
  *              val r = Random.nextInt(100)
  *              if (r % 10 != r / 10) {
  *                !Return(TailCalls.done(r))
  *              }
  *            }
  *            throw new AssertionError("Unreachable code");
  *          }
  *
  *          val r = randomInt().result
  *          r should be < 100
  *          r % 10 should not be r / 10
  *          }}}
  *
  * @example Since this [[Return]] keyword can automatically lift the return type,
  *          `TailCalls.done` can be omitted.
  *
  *          {{{
  *          import scala.util.Random
  *          import scala.util.control.TailCalls
  *          import scala.util.control.TailCalls.TailRec
  *          def randomInt(): TailRec[Int] = {
  *            while (true) {
  *              val r = Random.nextInt(100)
  *              if (r % 10 != r / 10) {
  *                !Return(r)
  *              }
  *            }
  *            throw new AssertionError("Unreachable code");
  *          }
  *
  *          val r = randomInt().result
  *          r should be < 100
  *          r % 10 should not be r / 10
  *          }}}
  */
final case class Return[ReturnValue](returnValue: ReturnValue) extends AnyVal with Keyword[Return[ReturnValue], Nothing]

object Return {

  implicit def returnDsl[ReturnValue, Domain >: ReturnValue]: Dsl[Return[ReturnValue], Domain, Nothing] =
    new Dsl[Return[ReturnValue], Domain, Nothing] {
      def cpsApply(keyword: Return[ReturnValue], handler: Nothing => Domain): Domain = {
        keyword.returnValue
      }
    }
}

package com.thoughtworks.dsl
package keywords

import com.thoughtworks.dsl.bangnotation.{`*`, reify, reset, unary_!}
import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.!!
import com.thoughtworks.dsl.Dsl.AsKeyword
import com.thoughtworks.dsl.keywords.TryFinally
import com.thoughtworks.dsl.Dsl.cpsApply

import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions
import scala.util.control.NonFatal

/** This [[Using]] keyword automatically manage resources in
  * [[scala.concurrent.Future]], [[domains.task.Task]], and other asynchronous
  * domains derived from `Future` or `Task`.
  *
  * @author
  *   杨博 (Yang Bo)
  * @see
  *   [[dsl]] for usage of this [[Using]] keyword in continuations
  */
opaque type Using[R <: AutoCloseable] = R

object Using {
  given [R <: AutoCloseable]: AsKeyword.FromKeyword[Using[R], R] with {}

  given [R <: AutoCloseable]: AsKeyword[R, Using[R], R] = Using(_)

  trait ScopeExitHandler extends AutoCloseable

  /** Returns a [[Using]] keyword to execute a [[ScopeExitHandler]] when exiting
    * the nearest enclosing scope that is annotated as [[Dsl.reset @reset]], (or
    * the nearest enclosing function if [[compilerplugins.ResetEverywhere]] is
    * enabled).
    *
    * @note
    *   This method is similar to [[apply]], except the parameter type is
    *   changed from a generic `R` to the SAM type [[ScopeExitHandler]], which
    *   allows for function literal expressions in Scala 2.12+ or Scala 2.11
    *   with `-Xexperimental` compiler option.
    *
    * @example
    *   The following function will perform `n *= 2` after `n += 20`:
    *
    * {{{
    *           import scala.concurrent.Future
    *           import com.thoughtworks.dsl.keywords.Using.scopeExit
    *           import com.thoughtworks.dsl.bangnotation._
    *           var n = 1
    *           def multiplicationAfterAddition = *[Future] {
    *             !scopeExit { () =>
    *               n *= 2
    *             }
    *             n += 20
    *           }
    * }}}
    *
    * Therefore, the final value of `n` should be `(1 + 20) * 2 = 42`.
    *
    * {{{
    *           multiplicationAfterAddition.map { _ =>
    *             n should be(42)
    *           }
    * }}}
    */
  def scopeExit(r: ScopeExitHandler) = r

  def apply[R <: AutoCloseable]: R =:= Using[R] = summon

  given [
      R <: AutoCloseable,
      Mapped,
      MappedValue,
      OuterDomain,
      BlockDomain,
      FinalizerDomain
  ](using
      AsKeyword.FromKeyword[Mapped, MappedValue],
      Dsl.TryFinally[MappedValue, OuterDomain, BlockDomain, FinalizerDomain],
      Dsl.PolyCont[Mapped, BlockDomain, MappedValue]
  ): Dsl.PolyCont[FlatMap[Using[R], R, Mapped], OuterDomain, MappedValue] = {
    case (FlatMap(r, flatMapper), handler) =>
      reset {
        handler(try {
          !flatMapper(r)
        } finally {
          r.close()
        })
      }
  }

}

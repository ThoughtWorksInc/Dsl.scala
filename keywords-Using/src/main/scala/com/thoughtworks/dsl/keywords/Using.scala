package com.thoughtworks.dsl
package keywords

import com.thoughtworks.dsl.reset, reset._
import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.!!
import com.thoughtworks.dsl.Dsl.IsKeyword
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
opaque type Using[+R] <: Dsl.Keyword.Opaque = Dsl.Keyword.Opaque.Of[R]

object Using {
  given [R]: IsKeyword[Using[R], R] with {}

  extension [R](inline r: R)(using
      inline notKeyword: util.NotGiven[
        R <:< Dsl.Keyword
      ]
  )
    transparent inline def unary_! : R =
      !Using[R](r)

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
    *           import com.thoughtworks.dsl.keywords.Using.unary_!
    *           import com.thoughtworks.dsl.reset, reset._
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
  def scopeExit(r: ScopeExitHandler): Using[ScopeExitHandler] = Using(r)

  def apply[R]: R =:= Using[R] = Dsl.Keyword.Opaque.Of.apply

  given [
      R <: AutoCloseable,
      Mapped <: Dsl.Keyword.Opaque | Dsl.Keyword.Trait,
      MappedValue,
      OuterDomain,
      BlockDomain,
      FinalizerDomain
  ](using
      IsKeyword[Mapped, MappedValue],
      Dsl.TryFinally[MappedValue, OuterDomain, BlockDomain, FinalizerDomain],
      Dsl[Mapped, BlockDomain, MappedValue]
  ): Dsl.Composed[FlatMap[Using[R], Mapped], OuterDomain, MappedValue] = {
    case (FlatMap(r, flatMapper: (Using[R] @unchecked => Mapped)), handler) =>
      reset {
        handler(try {
          !flatMapper(r)
        } finally {
          r.close()
        })
      }
  }

}

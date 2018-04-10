package com.thoughtworks.dsl

import com.thoughtworks.dsl.Dsl.!!

import scala.annotation._
import scala.util.control.{NonFatal, TailCalls}
import scala.util.control.TailCalls.TailRec

/** The domain-specific interpreter for `Keyword` in `Domain`,
  * which is a dependent type type class that registers an asynchronous callback function,
  * to handle the `Value` inside `Keyword`.
  *
  * @tparam Value The value held inside `Keyword`.
  * @author 杨博 (Yang Bo)
  * @example Creating a collaborative DSL in [[https://github.com/ThoughtWorksInc/Dsl.scala Dsl.scala]] is easy.
  *          Only two steps are required:
  *
  *           - Defining their domain-specific [[com.thoughtworks.dsl.Dsl.Keyword Keyword]].
  *           - Implementing thi [[Dsl]] type class, which is an interpreter for an [[com.thoughtworks.dsl.Dsl.Keyword Keyword]].
  *
  *          TODO: an example for creating a DSL
  */
@implicitNotFound("The keyword ${Keyword} is not supported inside a function that returns ${Domain}.")
trait Dsl[-Keyword, Domain, +Value] {

  /** Registers an asynchronous callback `handler` on `keyword`, to handle the `Value`. */
  def interpret(keyword: Keyword, handler: Value => Domain): Domain

}

private[dsl] trait LowPriorityDsl1 {

  implicit def continuationDsl[Keyword, Domain, MiddleValue, KeywordValue](
      implicit restDsl: Dsl[Keyword, Domain, KeywordValue]
  ): Dsl[Keyword, Domain !! MiddleValue, KeywordValue] = {
    new Dsl[Keyword, Domain !! MiddleValue, KeywordValue] {
      def interpret(keyword: Keyword, handler: KeywordValue => Domain !! MiddleValue): Domain !! MiddleValue = {
        (continue: MiddleValue => Domain) =>
          restDsl.interpret(keyword, { a =>
            handler(a)(continue)
          })
      }
    }
  }
}

private[dsl] trait LowPriorityDsl0 extends LowPriorityDsl1 {

  // FIXME: Shift[Domain, Throwable]
  @inline
  private def jvmCatch[Domain](eh: => Domain !! Throwable)(failureHandler: Throwable => Domain): Domain = {
    val protectedContinuation: Domain !! Throwable = try {
      eh
    } catch {
      case NonFatal(e) =>
        return failureHandler(e)
    }
    protectedContinuation(failureHandler)
  }

  implicit def throwableContinuationDsl[Keyword, Domain, KeywordValue](
      implicit restDsl: Dsl[Keyword, Domain, KeywordValue]
  ): Dsl[Keyword, Domain !! Throwable, KeywordValue] = {
    new Dsl[Keyword, Domain !! Throwable, KeywordValue] {
      def interpret(keyword: Keyword, handler: KeywordValue => Domain !! Throwable): Domain !! Throwable = {
        (continue: Throwable => Domain) =>
          restDsl.interpret(keyword, { a =>
            jvmCatch(handler(a))(continue)
          })
      }
    }
  }
}

object Dsl extends LowPriorityDsl0 {

  implicit def liftTailRecDsl[Keyword, Domain, Value](
      implicit restDsl: Dsl[Keyword, Domain, Value]): Dsl[Keyword, TailRec[Domain], Value] =
    new Dsl[Keyword, TailRec[Domain], Value] {
      def interpret(keyword: Keyword, handler: Value => TailRec[Domain]): TailRec[Domain] = TailCalls.done {
        restDsl.interpret(keyword, { value =>
          handler(value).result
        })
      }
    }

  // TODO: Support any middle types other than Throwable
  implicit def liftThrowableTailRecDsl[Keyword, Domain, Value](
      implicit restDsl: Dsl[Keyword, Domain !! Throwable, Value]): Dsl[Keyword, TailRec[Domain] !! Throwable, Value] =
    new Dsl[Keyword, TailRec[Domain] !! Throwable, Value] {
      def interpret(keyword: Keyword, handler: Value => TailRec[Domain] !! Throwable): TailRec[Domain] !! Throwable = {
        tailRecFailureHandler =>
          TailCalls.done(restDsl.interpret(keyword, { value => failureHandler =>
            handler(value) { e =>
              TailCalls.done(failureHandler(e))
            }.result
          }) { e =>
            tailRecFailureHandler(e).result
          })
      }
    }

  type Continuation[R, +A] = (A => R @reset) => R

  object Continuation {
    @inline
    def now[R, A](a: A): R !! A = _(a)

    @inline
    def empty[R, A](r: R): R !! A = Function.const(r)

    @inline
    def delay[R, A](a: () => A): R !! A = _(a())

    @inline
    def reset[R, A](a: => A): (R !! A) @reset = delay(a _)
  }

  type !![R, +A] = Continuation[R, A]
  val !! = Continuation

  private[dsl] /* sealed */ trait ResetAnnotation extends Annotation with StaticAnnotation
  private[dsl] final class nonTypeConstraintReset extends ResetAnnotation with StaticAnnotation

  /** An annotation to explicitly perform reset control operator on a code block. */
  final class reset extends ResetAnnotation with StaticAnnotation with TypeConstraint

  /** An annotation to mark a method is a shift control operator. */
  final class shift extends StaticAnnotation

  def apply[Keyword, Domain, Value](implicit typeClass: Dsl[Keyword, Domain, Value]): Dsl[Keyword, Domain, Value] =
    typeClass

  /**
    *
    * @tparam Self the self type
    * @see [[https://en.wikipedia.org/wiki/Curiously_recurring_template_pattern Curiously recurring template pattern]]
    *      for the reason why we need the `Self` type parameter
    */
  trait Keyword[Self, Value] extends Any { this: Self =>

    @shift
    @compileTimeOnly(
      """This method requires the compiler plugin: `addCompilerPlugin("com.thoughtworks.dsl" %% "compilerplugin" % "latest.release")` and must only be called inside a code block annotated as `@reset`.""")
    final def unary_! : Value = {
      throw new IllegalAccessException(
        """This method requires the compiler plugin: `addCompilerPlugin("com.thoughtworks.dsl" %% "compilerplugin" % "latest.release")` and must only be called inside a code block annotated as `@reset`."""
      )
    }

    final def cpsApply[Domain](handler: Value => Domain)(implicit dsl: Dsl[Self, Domain, Value]): Domain = {
      dsl.interpret(this, handler)
    }

  }

}

package com.thoughtworks.dsl

import com.thoughtworks.dsl.Dsl.!!

import scala.annotation._
import scala.util.control.TailCalls
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
@implicitNotFound("Cannot interpret the DSL keyword ${Keyword} inside a function that returns ${Domain}.")
trait Dsl[-Keyword, Domain, +Value] {

  /** Registers an asynchronous callback `handler` on `keyword`, to handle the `Value`. */
  def interpret(keyword: Keyword, handler: Value => Domain): Domain

}

private[dsl] trait LowPriorityDsl0 {

  implicit def continuationDsl[Keyword, Domain, FinalResult, KeywordValue](
      implicit restDsl: Dsl[Keyword, Domain, KeywordValue]
  ): Dsl[Keyword, Domain !! FinalResult, KeywordValue] = {
    new Dsl[Keyword, Domain !! FinalResult, KeywordValue] {
      def interpret(keyword: Keyword,
                    handler: KeywordValue => Domain !! FinalResult): Domain !! FinalResult = {
        (continue: FinalResult => Domain) =>
          restDsl.interpret(keyword, { a =>
            handler(a)(continue)
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

  type Continuation[R, +A] = (A => R @reset) => R
  type !![R, +A] = Continuation[R, A]

  private[dsl] /* sealed */ trait ResetAnnotation extends Annotation with StaticAnnotation
  private[dsl] final class nonTypeConstraintReset extends ResetAnnotation with StaticAnnotation

  /** An annotation to explicitly perform reset control operator on a code block. */
  final class reset extends ResetAnnotation with StaticAnnotation with TypeConstraint

  /** An annotation to mark a method is a shift control operator. */
  final class shift extends StaticAnnotation

  def apply[Keyword, Domain, Value](
      implicit typeClass: Dsl[Keyword, Domain, Value]): Dsl[Keyword, Domain, Value] =
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

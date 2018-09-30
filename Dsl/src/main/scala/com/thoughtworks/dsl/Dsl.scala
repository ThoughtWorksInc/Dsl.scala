package com.thoughtworks.dsl

import com.thoughtworks.dsl.Dsl.!!
import com.thoughtworks.enableMembersIf

import scala.annotation._
import scala.collection._
import scala.collection.mutable.Builder
import scala.concurrent.Future
import scala.language.higherKinds
import scala.util.{Failure, Success, Try}
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
  def cpsApply(keyword: Keyword, handler: Value => Domain): Domain

}

private[dsl] trait LowPriorityDsl3 {

  import Dsl._
  import Scala211Or212._
  import Scala213._

  implicit def nothingCollectionDsl[Keyword, Element, Collection[_]](
      implicit factory: Factory[Element, Collection[Element]],
      restDsl: Dsl[Keyword, Element, Nothing]
  ): Dsl[Keyword, Collection[Element], Nothing] =
    new Dsl[Keyword, Collection[Element], Nothing] {
      def cpsApply(keyword: Keyword, handler: Nothing => Collection[Element]): Collection[Element] = {
        singleton(resetDomain(keyword))
      }
    }
}

private[dsl] trait LowPriorityDsl2 extends LowPriorityDsl3 {

  implicit def derivedFunction1Dsl[Keyword, State, Domain, Value](
      implicit restDsl: Dsl[Keyword, Domain, Value]
  ): Dsl[Keyword, State => Domain, Value] =
    new Dsl[Keyword, State => Domain, Value] {
      def cpsApply(keyword: Keyword, handler: Value => State => Domain): State => Domain = {
        val restDsl1 = restDsl
        locally { state: State =>
          val handler1 = handler
          restDsl1.cpsApply(keyword, handler1(_)(state))
        }
      }
    }

}

private[dsl] trait LowPriorityDsl1 extends LowPriorityDsl2 {

//  // FIXME: Shift
//  implicit def continuationDsl[Keyword, LeftDomain, RightDomain, Value](
//      implicit restDsl: Dsl[Keyword, LeftDomain, Value],
//      shiftDsl2: Dsl[Shift[LeftDomain, RightDomain], LeftDomain, RightDomain]
//  ): Dsl[Keyword, LeftDomain !! RightDomain, Value] = {
//    new Dsl[Keyword, LeftDomain !! RightDomain, Value] {
//      def cpsApply(keyword: Keyword, handler: Value => LeftDomain !! RightDomain): LeftDomain !! RightDomain = {
//        (continue: RightDomain => LeftDomain) =>
//          restDsl.cpsApply(keyword, { a =>
//            restDsl2.cpsApply(handler(a), continue)
//          })
//      }
//    }
//  }

  @inline implicit def derivedContinuationDsl[Keyword, LeftDomain, RightDomain, Value](
      implicit restDsl: Dsl[Keyword, LeftDomain, Value]
  ): Dsl[Keyword, LeftDomain !! RightDomain, Value] = derivedFunction1Dsl(restDsl)

}

private[dsl] trait LowPriorityDsl0 extends LowPriorityDsl1 {

  implicit def throwableContinuationDsl[Keyword, LeftDomain, Value](
      implicit restDsl: Dsl[Keyword, LeftDomain, Value]
  ): Dsl[Keyword, LeftDomain !! Throwable, Value] = {
    new Dsl[Keyword, LeftDomain !! Throwable, Value] {
      def cpsApply(keyword: Keyword, handler: Value => LeftDomain !! Throwable): LeftDomain !! Throwable = {
        (continue: Throwable => LeftDomain) =>
          restDsl.cpsApply(
            keyword,
            new (Value => LeftDomain) {
              def apply(value: Value): LeftDomain = {
                val protectedContinuation = try {
                  handler(value)
                } catch {
                  case NonFatal(e) =>
                    return continue(e)
                }
                // FIXME: Shift[Domain, Throwable]
                protectedContinuation(continue)
              }
            }
          )
      }
    }
  }
}

object Dsl extends LowPriorityDsl0 {

  @enableMembersIf(scala.util.Properties.versionNumberString.matches("""^2\.1(1|2)\..*$"""))
  private[dsl] object Scala211Or212 {
    type Factory[-A, +C] = scala.collection.generic.CanBuildFrom[Nothing, A, C]

    @inline
    def singleton[A, C](a: A)(implicit factory: Factory[A, C]): C = {
      val builder = factory()
      builder.sizeHint(1)
      builder += a
      builder.result()
    }

  }

  @enableMembersIf(scala.util.Properties.versionNumberString.matches("""^2\.13\..*$"""))
  private[dsl] object Scala213 {

    @inline
    def singleton[A, C](a: A)(implicit factory: Factory[A, C]): C = {
      factory.fromSpecific(a :: Nil)
    }

  }

  @inline
  private[dsl] def resetDomain[Keyword, Domain](keyword: Keyword)(
      implicit dsl: Dsl[Keyword, Domain, Domain]): Domain = {
    dsl.cpsApply(keyword, identity)
  }

  implicit def nothingContinuationDsl[Keyword, LeftDomain, RightDomain](
      implicit restDsl: Dsl[Keyword, RightDomain, Nothing]): Dsl[Keyword, LeftDomain !! RightDomain, Nothing] =
    new Dsl[Keyword, LeftDomain !! RightDomain, Nothing] {
      def cpsApply(keyword: Keyword, handler: Nothing => LeftDomain !! RightDomain): LeftDomain !! RightDomain =
        _(resetDomain(keyword))
    }

  implicit def nothingFutureDsl[Keyword, Domain](
      implicit restDsl: Dsl[Keyword, Domain, Nothing]): Dsl[Keyword, Future[Domain], Nothing] =
    new Dsl[Keyword, Future[Domain], Nothing] {
      def cpsApply(keyword: Keyword, handler: Nothing => Future[Domain]): Future[Domain] = {
        Future.successful(resetDomain(keyword))
      }
    }

  implicit def derivedTailRecDsl[Keyword, Domain, Value](
      implicit restDsl: Dsl[Keyword, Domain, Value]): Dsl[Keyword, TailRec[Domain], Value] =
    new Dsl[Keyword, TailRec[Domain], Value] {
      def cpsApply(keyword: Keyword, handler: Value => TailRec[Domain]): TailRec[Domain] = TailCalls.done {
        restDsl.cpsApply(keyword, { value =>
          handler(value).result
        })
      }
    }

  implicit def derivedThrowableTailRecDsl[Keyword, LeftDomain, Value](
      implicit restDsl: Dsl[Keyword, LeftDomain !! Throwable, Value])
    : Dsl[Keyword, TailRec[LeftDomain] !! Throwable, Value] =
    new Dsl[Keyword, TailRec[LeftDomain] !! Throwable, Value] {
      def cpsApply(keyword: Keyword,
                   handler: Value => TailRec[LeftDomain] !! Throwable): TailRec[LeftDomain] !! Throwable = {
        tailRecFailureHandler =>
          TailCalls.done(restDsl.cpsApply(keyword, { value => failureHandler =>
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
    def apply[R, A](a: => A): (R !! A) @reset = delay(a _)

    def toTryContinuation[LeftDomain, Value](task: LeftDomain !! Throwable !! Value): LeftDomain !! Try[Value] = {
      handler =>
        task { a => failureHandler =>
          handler(Success(a))
        } { e =>
          handler(Failure(e))
        }
    }

  }

  type !![R, +A] = Continuation[R, A]
  val !! = Continuation

  private[dsl] /* sealed */ trait ResetAnnotation extends Annotation with StaticAnnotation
  private[dsl] final class nonTypeConstraintReset extends ResetAnnotation with StaticAnnotation

  /** An annotation to explicitly perform reset control operator on a code block.
    *
    * @note This annotation can be automatically added
    *       if [[compilerplugins.ResetEverywhere ResetEverywhere]] compiler plug-in is enabled.
    */
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
      """This method requires the compiler plugin: `addCompilerPlugin("com.thoughtworks.dsl" %% "compilerplugins-bangnotation" % "latest.release")` and must only be called inside a code block annotated as `@reset`.""")
    final def unary_! : Value = {
      throw new IllegalAccessException(
        """This method requires the compiler plugin: `addCompilerPlugin("com.thoughtworks.dsl" %% "compilerplugins-bangnotation" % "latest.release")` and must only be called inside a code block annotated as `@reset`."""
      )
    }

    @inline
    final def cpsApply[Domain](handler: Value => Domain)(implicit dsl: Dsl[Self, Domain, Value]): Domain = {
      dsl.cpsApply(this, handler)
    }

    /** An alias to [[cpsApply]]. */
    @inline
    final def apply[Domain](handler: Value => Domain)(implicit dsl: Dsl[Self, Domain, Value]): Domain = {
      cpsApply(handler)
    }

  }

}

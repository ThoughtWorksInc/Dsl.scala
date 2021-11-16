package com.thoughtworks.dsl

import com.thoughtworks.dsl.Dsl.!!
import com.thoughtworks.enableMembersIf

import scala.annotation._
import scala.collection._
import scala.collection.mutable.Builder
import scala.concurrent.{ExecutionContext, Future}
import scala.language.higherKinds
import scala.util.control.Exception.Catcher
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
  *           - Implementing this [[Dsl]] type class, which is an interpreter for an [[com.thoughtworks.dsl.Dsl.Keyword Keyword]].
  */
@implicitNotFound("The keyword ${Keyword} is not supported inside a function that returns ${Domain}.")
trait Dsl[-Keyword, Domain, +Value] {

  /** Registers an asynchronous callback `handler` on `keyword`, to handle the `Value`. */
  def cpsApply(keyword: Keyword, handler: Value => Domain): Domain

}

private[dsl] trait LowPriorityDsl2 {

  import Dsl._
  import Scala211Or212._
  import Scala213._

  implicit def nothingCollectionDsl[Keyword, Element, Collection[_]](implicit
      factory: Factory[Element, Collection[Element]],
      restDsl: Dsl[Keyword, Element, Nothing]
  ): Dsl[Keyword, Collection[Element], Nothing] = { (keyword, handler) =>
    singleton(resetDomain(keyword))
  }
}

private[dsl] trait LowPriorityDsl1 extends LowPriorityDsl2 {

  implicit def derivedFunction1Dsl[Keyword, State, Domain, Value](implicit
      restDsl: Dsl[Keyword, Domain, Value]
  ): Dsl[Keyword, State => Domain, Value] = { (keyword, handler) =>
    val restDsl1 = restDsl
    locally { state: State =>
      val handler1 = handler
      restDsl1.cpsApply(keyword, handler1(_)(state))
    }

  }

}

private[dsl] trait LowPriorityDsl0 extends LowPriorityDsl1 {

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

  implicit def throwableContinuationDsl[Keyword, LeftDomain, Value](implicit
      restDsl: Dsl[Keyword, LeftDomain, Value]
  ): Dsl[Keyword, LeftDomain !! Throwable, Value] = { (keyword, handler) => continue =>
    restDsl.cpsApply(
      keyword,
      new (Value => LeftDomain) {
        def apply(value: Value): LeftDomain = {
          val protectedContinuation =
            try {
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
  private[dsl] def resetDomain[Keyword, Domain](
      keyword: Keyword
  )(implicit dsl: Dsl[Keyword, Domain, Domain]): Domain = {
    dsl.cpsApply(keyword, implicitly)
  }

  implicit def nothingContinuationDsl[Keyword, LeftDomain, RightDomain](implicit
      restDsl: Dsl[Keyword, RightDomain, Nothing]
  ): Dsl[Keyword, LeftDomain !! RightDomain, Nothing] = { (keyword, handler) =>
    _(resetDomain(keyword))
  }

  implicit def nothingFutureDsl[Keyword, Domain](implicit
      restDsl: Dsl[Keyword, Domain, Nothing]
  ): Dsl[Keyword, Future[Domain], Nothing] = { (keyword, handler) =>
    Future.successful(resetDomain(keyword))
  }

  implicit def derivedTailRecDsl[Keyword, Domain, Value](implicit
      restDsl: Dsl[Keyword, Domain, Value]
  ): Dsl[Keyword, TailRec[Domain], Value] = { (keyword, handler) =>
    TailCalls.done {
      restDsl.cpsApply(
        keyword,
        { value =>
          handler(value).result
        }
      )
    }
  }

  implicit def derivedThrowableTailRecDsl[Keyword, LeftDomain, Value](implicit
      restDsl: Dsl[Keyword, LeftDomain !! Throwable, Value]
  ): Dsl[Keyword, TailRec[LeftDomain] !! Throwable, Value] = { (keyword, handler) => tailRecFailureHandler =>
    TailCalls.done(
      restDsl.cpsApply(
        keyword,
        { value => failureHandler =>
          handler(value) { e =>
            TailCalls.done(failureHandler(e))
          }.result
        }
      ) { e =>
        tailRecFailureHandler(e).result
      }
    )
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

    def toTryContinuation[LeftDomain, Value](
        task: LeftDomain !! Throwable !! Value
    )(handler: Try[Value] => LeftDomain): LeftDomain = {
      task { a => failureHandler =>
        handler(Success(a))
      } { e =>
        handler(Failure(e))
      }
    }

    def fromTryContinuation[LeftDomain, Value](
        continuation: LeftDomain !! Try[Value]
    )(successHandler: Value => LeftDomain !! Throwable)(failureHandler: Throwable => LeftDomain): LeftDomain = {
      continuation(
        new (Try[Value] => LeftDomain) {
          def apply(result: Try[Value]): LeftDomain = {
            result match {
              case Success(a) =>
                val protectedContinuation =
                  try {
                    successHandler(a)
                  } catch {
                    case NonFatal(e) =>
                      return failureHandler(e)
                  }
                protectedContinuation(failureHandler)
              case Failure(e) =>
                failureHandler(e)
            }
          }
        }
      )
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

  /** @tparam Self the self type
    * @see [[https://en.wikipedia.org/wiki/Curiously_recurring_template_pattern Curiously recurring template pattern]]
    *      for the reason why we need the `Self` type parameter
    */
  trait Keyword[Self, Value] extends Any { this: Self =>

    @shift
    @compileTimeOnly(
      """This method requires the compiler plugin: `addCompilerPlugin("com.thoughtworks.dsl" %% "compilerplugins-bangnotation" % "latest.release")` and must only be called inside a code block annotated as `@reset`."""
    )
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

  private def catchNativeException[A](futureContinuation: Future[A] !! A): Future[A] = {
    try {
      futureContinuation(Future.successful)
    } catch {
      case NonFatal(e) =>
        Future.failed(e)
    }
  }

  /** The type class to support `try` ... `catch` ... `finally` expression for `OutputDomain`.
    *
    * !-notation is allowed by default for `? !! Throwable` and [[scala.concurrent.Future Future]] domains,
    * with the help of this type class.
    */
  @implicitNotFound(
    "The `try` ... `catch` ... `finally` expression cannot contain !-notation inside a function that returns ${OuterDomain}."
  )
  trait TryCatchFinally[Value, OuterDomain, BlockDomain, FinalizerDomain] {
    def tryCatchFinally(
        block: BlockDomain !! Value,
        catcher: Catcher[BlockDomain !! Value],
        finalizer: FinalizerDomain !! Unit,
        outerSuccessHandler: Value => OuterDomain
    ): OuterDomain
  }

  object TryCatchFinally {
    implicit final class Ops[Value, OuterDomain, BlockDomain, FinalizerDomain](
        outerSuccessHandler: Value => OuterDomain
    )(implicit
        typeClass: TryCatchFinally[Value, OuterDomain, BlockDomain, FinalizerDomain]
    ) {
      def apply(
          block: BlockDomain !! Value,
          catcher: Catcher[BlockDomain !! Value],
          finalizer: FinalizerDomain !! Unit
      ): OuterDomain =
        typeClass.tryCatchFinally(block, catcher, finalizer, outerSuccessHandler)
    }

    implicit def fromTryCatchTryFinally[Value, OuterDomain, BlockDomain, FinalizerDomain](implicit
        tryFinally: TryFinally[Value, OuterDomain, BlockDomain, FinalizerDomain],
        tryCatch: TryCatch[Value, BlockDomain, BlockDomain]
    ): TryCatchFinally[Value, OuterDomain, BlockDomain, FinalizerDomain] = {
      (
          block: BlockDomain !! Value,
          catcher: Catcher[BlockDomain !! Value],
          finalizer: FinalizerDomain !! Unit,
          outerSuccessHandler: Value => OuterDomain
      ) =>
        tryFinally.tryFinally(
          {
            tryCatch.tryCatch(block, catcher, _)
          },
          finalizer,
          outerSuccessHandler
        )
    }
  }

  @implicitNotFound(
    "The `try` ... `catch` expression cannot contain !-notation inside a function that returns ${OuterDomain}."
  )
  trait TryCatch[Value, OuterDomain, BlockDomain] {
    def tryCatch(
        block: BlockDomain !! Value,
        catcher: Catcher[BlockDomain !! Value],
        outerSuccessHandler: Value => OuterDomain
    ): OuterDomain
  }

  private[dsl] trait LowPriorityTryCatch {
    implicit def liftFunction1TryCatch[Value, OuterDomain, BlockDomain, State](implicit
        restTryCatch: TryCatch[Value, OuterDomain, BlockDomain]
    ): TryCatch[Value, State => OuterDomain, State => BlockDomain] = {
      (
          block: (State => BlockDomain) !! Value,
          catcher: Catcher[(State => BlockDomain) !! Value],
          outerSuccessHandler: Value => State => OuterDomain
      ) => (state: State) =>
        def withState(blockContinuation: (State => BlockDomain) !! Value) = { blockHandler: (Value => BlockDomain) =>
          blockContinuation { value: Value => (state: State) =>
            blockHandler(value)
          }(state)
        }

        restTryCatch.tryCatch(withState(block), catcher.andThen(withState _), outerSuccessHandler(_)(state))
    }
  }

  object TryCatch extends LowPriorityTryCatch {

    implicit final class Ops[Value, OuterDomain, BlockDomain](outerSuccessHandler: Value => OuterDomain)(implicit
        typeClass: TryCatch[Value, OuterDomain, BlockDomain]
    ) {
      def apply(block: BlockDomain !! Value, catcher: Catcher[BlockDomain !! Value]) = {
        typeClass.tryCatch(block, catcher, outerSuccessHandler)
      }
    }

    implicit def futureTryCatch[BlockValue, OuterValue](implicit
        executionContext: ExecutionContext
    ): TryCatch[BlockValue, Future[OuterValue], Future[BlockValue]] = {
      (
          block: Future[BlockValue] !! BlockValue,
          catcher: Catcher[Future[BlockValue] !! BlockValue],
          outerSuccessHandler: BlockValue => Future[OuterValue]
      ) =>
        catchNativeException(block)
          .recoverWith { case e: Throwable =>
            def recover(): Future[BlockValue] = {
              (try {
                catcher.lift(e)
              } catch {
                case NonFatal(extractorException) =>
                  return Future.failed(extractorException)
              }) match {
                case None =>
                  Future.failed(e)
                case Some(recovered) =>
                  catchNativeException(recovered)
              }
            }
            recover()
          }
          .flatMap(outerSuccessHandler)
    }

    implicit def throwableContinuationTryCatch[LeftDomain, Value]
        : TryCatch[Value, LeftDomain !! Throwable, LeftDomain !! Throwable] = {

      (
          block: LeftDomain !! Throwable !! Value,
          catcher: Catcher[LeftDomain !! Throwable !! Value],
          outerSuccessHandler: Value => LeftDomain !! Throwable
      ) => outerFailureHandler =>
        def innerFailureHandler(e: Throwable): LeftDomain = {
          catcher.lift(e) match {
            case None =>
              outerFailureHandler(e)
            case Some(recovered) =>
              @inline
              def recoveredHandler(): LeftDomain = {
                locally {
                  try {
                    recovered(outerSuccessHandler)
                  } catch {
                    case NonFatal(nativeThrown) =>
                      return outerFailureHandler(nativeThrown)
                  }
                }(outerFailureHandler)
              }

              recoveredHandler()
          }
        }

        def runBlock(): LeftDomain = {
          (try {
            block { a => hookedFailureHandler =>
              @inline
              def successHandler(): LeftDomain = {
                locally {
                  try {
                    outerSuccessHandler(a)
                  } catch {
                    case NonFatal(nativeThrown) =>
                      return outerFailureHandler(nativeThrown)
                  }
                }(outerFailureHandler)
              }

              successHandler()
            }
          } catch {
            case NonFatal(e) =>
              return innerFailureHandler(e)
          })(innerFailureHandler)
        }
        runBlock()
    }
  }

  @implicitNotFound(
    "The `try` ... `finally` expression cannot contain !-notation inside a function that returns ${OuterDomain}."
  )
  trait TryFinally[Value, OuterDomain, BlockDomain, FinalizerDomain] {
    def tryFinally(
        block: BlockDomain !! Value,
        finalizer: FinalizerDomain !! Unit,
        outerSuccessHandler: Value => OuterDomain
    ): OuterDomain
  }

  private[dsl] trait LowPriorityTryFinally {
    implicit def liftFunction1TryCatch[Value, OuterDomain, BlockDomain, FinalizerDomain, State](implicit
        restTryFinally: TryFinally[Value, OuterDomain, BlockDomain, FinalizerDomain]
    ): TryFinally[Value, State => OuterDomain, State => BlockDomain, State => FinalizerDomain] = {
      (
          block: (State => BlockDomain) !! Value,
          finalizer: (State => FinalizerDomain) !! Unit,
          outerSuccessHandler: Value => State => OuterDomain
      ) => state =>
        def withState[Domain, Value](blockContinuation: (State => Domain) !! Value) = {
          blockHandler: (Value => Domain) =>
            blockContinuation { value: Value => (state: State) =>
              blockHandler(value)
            }(state)
        }

        restTryFinally.tryFinally(withState(block), withState(finalizer), outerSuccessHandler(_)(state))
    }
  }

  object TryFinally extends LowPriorityTryFinally {

    implicit final class Ops[Value, OuterDomain, BlockDomain, FinalizerDomain] @inline() (
        outerSuccessHandler: Value => OuterDomain
    )(implicit typeClass: TryFinally[Value, OuterDomain, BlockDomain, FinalizerDomain]) {
      @inline
      def apply(block: BlockDomain !! Value, finalizer: FinalizerDomain !! Unit): OuterDomain = {
        typeClass.tryFinally(block, finalizer, outerSuccessHandler)
      }
    }

    implicit def futureTryFinally[BlockValue, OuterValue](implicit
        executionContext: ExecutionContext
    ): TryFinally[BlockValue, Future[OuterValue], Future[BlockValue], Future[Unit]] = {
      (
          block: Future[BlockValue] !! BlockValue,
          finalizer: Future[Unit] !! Unit,
          outerSuccessHandler: BlockValue => Future[OuterValue]
      ) =>
        @inline
        def injectFinalizer[A](f: Unit => Future[A]): Future[A] = {
          catchNativeException(finalizer).flatMap(f)
        }

        catchNativeException(block)
          .recoverWith { case e: Throwable =>
            injectFinalizer { _: Unit =>
              Future.failed(e)
            }
          }
          .flatMap { a =>
            injectFinalizer { _: Unit =>
              outerSuccessHandler(a)
            }
          }
    }

    implicit def throwableContinuationTryFinally[LeftDomain, Value]
        : TryFinally[Value, LeftDomain !! Throwable, LeftDomain !! Throwable, LeftDomain !! Throwable] = {
      (block, finalizer, outerSuccessHandler) => outerFailureHandler =>
        @inline
        def injectFinalizer(finalizerHandler: Unit => LeftDomain !! Throwable): LeftDomain = {
          locally {
            try {
              finalizer(finalizerHandler)
            } catch {
              case NonFatal(e) =>
                return outerFailureHandler(e)
            }
          }(outerFailureHandler)
        }

        @inline
        def hookedFailureHandler(e: Throwable) =
          injectFinalizer { _: Unit =>
            _(e)
          }

        def runBlock(): LeftDomain = {
          (try {
            block { value => hookedFailureHandler =>
              injectFinalizer { _: Unit =>
                outerSuccessHandler(value)
              }
            }
          } catch {
            case NonFatal(e) =>
              return hookedFailureHandler(e)
          })(hookedFailureHandler)
        }
        runBlock()
    }
  }
}

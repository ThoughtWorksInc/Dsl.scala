package com.thoughtworks.dsl
import com.thoughtworks.dsl.Dsl.!!

import scala.annotation._
import scala.collection._
import scala.collection.mutable.Builder
import scala.concurrent.{ExecutionContext, Future}
import scala.language.higherKinds
import scala.util.NotGiven
import scala.util.control.Exception.Catcher
import scala.util.{Failure, Success, Try}
import scala.util.control.{NonFatal, TailCalls}
import scala.util.control.TailCalls.TailRec

/** The domain-specific interpreter for `Keyword` in `Domain`, which is a dependent type type class that registers an
  * asynchronous callback function, to handle the `Value` inside `Keyword`.
  *
  * @tparam Value
  *   The value held inside `Keyword`.
  * @author
  *   杨博 (Yang Bo)
  * @example
  *   Creating a collaborative DSL in [[https://github.com/ThoughtWorksInc/Dsl.scala Dsl.scala]] is easy. Only two steps
  *   are required:
  *
  *   - Defining their domain-specific [[com.thoughtworks.dsl.Dsl.Keyword Keyword]].
  *   - Implementing this [[Dsl]] type class, which is an interpreter for an
  *     [[com.thoughtworks.dsl.Dsl.Keyword Keyword]].
  */
@implicitNotFound("The keyword:\n ${Keyword}\nis not supported inside a function that returns:\n${Domain}.")
trait Dsl[-Keyword, Domain, +Value] {

  /** Registers an asynchronous callback `handler` on `keyword`, to handle the `Value`. */
  def cpsApply(keyword: Keyword, handler: Value => Domain): Domain

}

private[dsl] trait LowPriorityDsl1 { this: Dsl.type =>

  implicit def derivedFunction1Dsl[Keyword, State, Domain, Value](implicit
      restDsl: Dsl[Keyword, Domain, Value]
  ): Derived[Keyword, State => Domain, Value] = { (keyword: Keyword, handler: Value => State => Domain) =>
    val restDsl1 = restDsl
    locally { (state: State) =>
      val handler1 = handler
      restDsl1.cpsApply(keyword, handler1(_)(state))
    }

  }

}

private[dsl] trait LowPriorityDsl0 extends LowPriorityDsl1 { this: Dsl.type =>

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
  ): Derived[Keyword, LeftDomain !! Throwable, Value] = { (keyword, handler) => continue =>
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
  trait Derived[-Keyword, Domain, +Value] extends Dsl[Keyword, Domain, Value]

  implicit def derivedTailRecDsl[Keyword, Domain, Value](implicit
      restDsl: Dsl[Keyword, Domain, Value]
  ): Derived[Keyword, TailRec[Domain], Value] = { (keyword, handler) =>
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
  ): Derived[Keyword, TailRec[LeftDomain] !! Throwable, Value] = { (keyword, handler) => tailRecFailureHandler =>
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

  type Continuation[R, +A] = (A => R) => R

  object Continuation {
    @inline
    def now[R, A](a: A): R !! A = _(a)

    @inline
    def empty[R, A](r: R): R !! A = Function.const(r)

    @inline
    def delay[R, A](a: () => A): R !! A = _(a())

    @inline
    def apply[R, A](a: => A): (R !! A) = delay(() => a)

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

  @deprecated("Use bangnotation.reset instead", "Dsl.scala 2.0.0")
  private[dsl] /* sealed */ trait ResetAnnotation extends Annotation with StaticAnnotation
  @deprecated("Use bangnotation.reset instead", "Dsl.scala 2.0.0")
  private[dsl] final class nonTypeConstraintReset extends ResetAnnotation with StaticAnnotation

  /** An annotation to explicitly perform reset control operator on a code block.
    *
    * @note
    *   This annotation can be automatically added if [[compilerplugins.ResetEverywhere ResetEverywhere]] compiler
    *   plug-in is enabled.
    */
  @deprecated("Use bangnotation.reset instead", "Dsl.scala 2.0.0")
  final class reset extends ResetAnnotation with StaticAnnotation with TypeConstraint

  /** An annotation to mark a method is a shift control operator. */
  @deprecated("Use bangnotation.reset instead", "Dsl.scala 2.0.0")
  final class shift extends StaticAnnotation

  def apply[Keyword, Domain, Value](implicit typeClass: Dsl[Keyword, Domain, Value]): Dsl[Keyword, Domain, Value] =
    typeClass

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
    * !-notation is allowed by default for `? !! Throwable` and [[scala.concurrent.Future Future]] domains, with the help of this
    * type class.
    */
  @implicitNotFound(
    "The `try` ... `catch` ... `finally` expression cannot contain !-notation inside a function that returns ${OuterDomain}."
  )
  @deprecated("Use com.thoughtworks.dsl.keywords.TryCatchFinally instead", "Dsl.scala 2.0.0")
  trait TryCatchFinally[Value, OuterDomain, BlockDomain, FinalizerDomain] {
    def tryCatchFinally(
        block: BlockDomain !! Value,
        catcher: Catcher[BlockDomain !! Value],
        finalizer: FinalizerDomain !! Unit,
        outerSuccessHandler: Value => OuterDomain
    ): OuterDomain
  }

  @deprecated("Use com.thoughtworks.dsl.keywords.TryCatchFinally instead", "Dsl.scala 2.0.0")
  object TryCatchFinally {
    @deprecated("Use com.thoughtworks.dsl.keywords.TryCatchFinally instead", "Dsl.scala 2.0.0")
    implicit final class Ops[Value, OuterDomain, BlockDomain, FinalizerDomain](
        outerSuccessHandler: Value => OuterDomain
    )(implicit
        typeClass: TryCatchFinally[Value, OuterDomain, BlockDomain, FinalizerDomain]
    ) {
      @deprecated("Use com.thoughtworks.dsl.keywords.TryCatchFinally instead", "Dsl.scala 2.0.0")
      def apply(
          block: BlockDomain !! Value,
          catcher: Catcher[BlockDomain !! Value],
          finalizer: FinalizerDomain !! Unit
      ): OuterDomain =
        typeClass.tryCatchFinally(block, catcher, finalizer, outerSuccessHandler)
    }

    @deprecated("Use com.thoughtworks.dsl.keywords.TryCatchFinally instead", "Dsl.scala 2.0.0")
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
  @deprecated("Use com.thoughtworks.dsl.keywords.TryCatch instead", "Dsl.scala 2.0.0")
  trait TryCatch[Value, OuterDomain, BlockDomain] {
    def tryCatch(
        block: BlockDomain !! Value,
        catcher: Catcher[BlockDomain !! Value],
        outerSuccessHandler: Value => OuterDomain
    ): OuterDomain
  }

  @deprecated("Use com.thoughtworks.dsl.keywords.TryCatch instead", "Dsl.scala 2.0.0")
  private[dsl] trait LowPriorityTryCatch {
    @deprecated("Use com.thoughtworks.dsl.keywords.TryCatch instead", "Dsl.scala 2.0.0")
    implicit def liftFunction1TryCatch[Value, OuterDomain, BlockDomain, State](implicit
        restTryCatch: TryCatch[Value, OuterDomain, BlockDomain]
    ): TryCatch[Value, State => OuterDomain, State => BlockDomain] = {
      (
          block: (State => BlockDomain) !! Value,
          catcher: Catcher[(State => BlockDomain) !! Value],
          outerSuccessHandler: Value => State => OuterDomain
      ) => (state: State) =>
        def withState(blockContinuation: (State => BlockDomain) !! Value) = { (blockHandler: (Value => BlockDomain)) =>
          blockContinuation { (value: Value) => (state: State) =>
            blockHandler(value)
          }(state)
        }

        restTryCatch.tryCatch(withState(block), catcher.andThen(withState _), outerSuccessHandler(_)(state))
    }
  }

  @deprecated("Use com.thoughtworks.dsl.keywords.TryCatch instead", "Dsl.scala 2.0.0")
  object TryCatch extends LowPriorityTryCatch {

    @deprecated("Use com.thoughtworks.dsl.keywords.TryCatch instead", "Dsl.scala 2.0.0")
    implicit final class Ops[Value, OuterDomain, BlockDomain](outerSuccessHandler: Value => OuterDomain)(implicit
        typeClass: TryCatch[Value, OuterDomain, BlockDomain]
    ) {
      def apply(block: BlockDomain !! Value, catcher: Catcher[BlockDomain !! Value]) = {
        typeClass.tryCatch(block, catcher, outerSuccessHandler)
      }
    }

    @deprecated("Use com.thoughtworks.dsl.keywords.TryCatch instead", "Dsl.scala 2.0.0")
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

    @deprecated("Use com.thoughtworks.dsl.keywords.TryCatch instead", "Dsl.scala 2.0.0")
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
  @deprecated("Use com.thoughtworks.dsl.keywords.TryFinally instead", "Dsl.scala 2.0.0")
  trait TryFinally[Value, OuterDomain, BlockDomain, FinalizerDomain] {
    def tryFinally(
        block: BlockDomain !! Value,
        finalizer: FinalizerDomain !! Unit,
        outerSuccessHandler: Value => OuterDomain
    ): OuterDomain
  }

  private[dsl] trait LowPriorityTryFinally {
    @deprecated("Use com.thoughtworks.dsl.keywords.TryCatch instead", "Dsl.scala 2.0.0")
    implicit def liftFunction1TryCatch[Value, OuterDomain, BlockDomain, FinalizerDomain, State](implicit
        restTryFinally: TryFinally[Value, OuterDomain, BlockDomain, FinalizerDomain]
    ): TryFinally[Value, State => OuterDomain, State => BlockDomain, State => FinalizerDomain] = {
      (
          block: (State => BlockDomain) !! Value,
          finalizer: (State => FinalizerDomain) !! Unit,
          outerSuccessHandler: Value => State => OuterDomain
      ) => state =>
        def withState[Domain, Value](blockContinuation: (State => Domain) !! Value) = {
          (blockHandler: (Value => Domain)) =>
            blockContinuation { (value: Value) => (state: State) =>
              blockHandler(value)
            }(state)
        }

        restTryFinally.tryFinally(withState(block), withState(finalizer), outerSuccessHandler(_)(state))
    }
  }

  @deprecated("Use com.thoughtworks.dsl.keywords.TryFinally instead", "Dsl.scala 2.0.0")
  object TryFinally extends LowPriorityTryFinally {

    @deprecated("Use com.thoughtworks.dsl.keywords.TryFinally instead", "Dsl.scala 2.0.0")
    implicit final class Ops[Value, OuterDomain, BlockDomain, FinalizerDomain] @inline() (
        outerSuccessHandler: Value => OuterDomain
    )(implicit typeClass: TryFinally[Value, OuterDomain, BlockDomain, FinalizerDomain]) {
      @inline
      @deprecated("Use com.thoughtworks.dsl.keywords.TryFinally instead", "Dsl.scala 2.0.0")
      def apply(block: BlockDomain !! Value, finalizer: FinalizerDomain !! Unit): OuterDomain = {
        typeClass.tryFinally(block, finalizer, outerSuccessHandler)
      }
    }

    @deprecated("Use com.thoughtworks.dsl.keywords.TryFinally instead", "Dsl.scala 2.0.0")
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
            injectFinalizer { (_: Unit) =>
              Future.failed(e)
            }
          }
          .flatMap { a =>
            injectFinalizer { (_: Unit) =>
              outerSuccessHandler(a)
            }
          }
    }

    @deprecated("Use com.thoughtworks.dsl.keywords.TryFinally instead", "Dsl.scala 2.0.0")
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
          injectFinalizer { (_: Unit) =>
            _(e)
          }

        def runBlock(): LeftDomain = {
          (try {
            block { value => hookedFailureHandler =>
              injectFinalizer { (_: Unit) =>
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

  @FunctionalInterface
  trait Lift[From, To] extends (From => To)

  object Lift {
    @FunctionalInterface
    trait OneStep[From, To] extends Lift[From, To]

    given [CastFrom, CastTo >: CastFrom]: Lift[CastFrom, CastTo] with {
      def apply(from: CastFrom): CastTo = {
        from
      }
    }

    given [From, Intermediate, To](using
        step1: => OneStep[Intermediate, To],
        step0: => Lift[From, Intermediate]
    ): Lift[From, To] with {
      def apply(from: From): To = {
        step1(step0(from))
      }
    }

    import Dsl.!!
    given [LeftDomain, RightDomain]: OneStep[RightDomain, LeftDomain !! RightDomain] = r => _(r)
    given [R, F, A](using
        isFunction: (A => R) <:< F
    ): OneStep[R, F] = { r => isFunction(Function.const(r)) }

    given [Element](using
        ExecutionContext
    ): OneStep[Element, Future[Element]] = {
      Future.successful
    }

    given [State, Element](using
        ExecutionContext
    ): OneStep[Element, State => Element] = {
      Function.const
    }

    given [Element, Collection <: IterableOnce[Element] | Array[Element]](using
        factory: collection.Factory[Element, Collection]
    ): OneStep[Element, Collection] = { element =>
      factory.fromSpecific(element :: Nil)
    }

    // given[Element, Array <: scala.Array[Element]](
    //   given factory: collection.Factory[Element, Array]
    // ): OneStep[Element, Array] = { element =>
    //   factory.fromSpecific(element :: Nil)
    // }
    // given[Collection[_], Element](
    //   given factory: collection.Factory[Element, Collection[Element]]
    // ): OneStep[Element, Collection[Element]] = { element =>
    //   factory.fromSpecific(element :: Nil)
    // }

    given [Collection[a] >: List[a], Element]: OneStep[Element, Collection[Element]] = { element =>
      element :: Nil
    }

  }

  given [Keyword, Domain, State, InnerDomain, Value](using
      // Return Dsl[Keyword, Domain, Value] instead of more specific Dsl[Keyword, State => InnerDomain, Value], in order to lower down the priority
      isFunctionDsl: Dsl[Keyword, State => InnerDomain, Value] <:< Dsl[Keyword, Domain, Value],
      restDsl: => Dsl[Keyword, InnerDomain, Value]
  ): Dsl[Keyword, Domain, Value] = isFunctionDsl(new Dsl[Keyword, State => InnerDomain, Value] {
    def cpsApply(keyword: Keyword, handler: Value => State => InnerDomain): State => InnerDomain = {
      val restDsl1 = restDsl
      locally { state =>
        val handler1 = handler
        restDsl1.cpsApply(
          keyword,
          { value =>
            handler1.apply(value).apply(state)
          }
        )
      }
    }
  })

  trait Run[Keyword, Domain, Value] extends (Keyword => Domain)

  object Run {

    trait RunThenLift[Keyword, Domain, Value] extends Run[Keyword, Domain, Value]

    given [Keyword, FromDomain, ToDomain, Value](using
        lift: /*=>*/ Lift.OneStep[FromDomain, ToDomain],
        run: /*=>*/ Run[Keyword, FromDomain, Value]
    ): RunThenLift[Keyword, ToDomain, Value] with {
      @inline def apply(typedKeyword: Keyword): ToDomain = {
        lift(run(typedKeyword))
      }
    }

    given [Keyword, Domain, Value](
        using /* erased */
        not: util.NotGiven[RunThenLift[Keyword, Domain, Value]]
    )(using
        dsl: /*=>*/ Dsl[Keyword, Domain, Value],
        lift: /*=>*/ Lift[Value, Domain]
    ): Run[Keyword, Domain, Value] with {
      @inline def apply(keyword: Keyword): Domain = {
        dsl.cpsApply(keyword, lift)
      }
    }

  }

  // TODO: Move to bangnotation
  /** A type annotated keyword */
  opaque type Typed[Keyword, Value] = Keyword
  object Typed {
    given [Keyword, Value]: IsKeyword[Typed[Keyword, Value], Value] with {}
    given [Keyword, Domain, Value](using dsl: Dsl[Keyword, Domain, Value]): Dsl[Typed[Keyword, Value], Domain, Value] =
      dsl

    // TODO: Remove
    given ToTypedKeyword[Keyword]: AnyRef with {
      extension [Value](keyword: Keyword)
        @inline def typed: Typed[Keyword, Value] = {
          keyword
        }
    }

    given [Keyword, Value]: AnyRef with {
      extension [NewValue](typedKeyword: Typed[Keyword, Value])
        @inline def withValueType: Typed[Keyword, NewValue] = typedKeyword
    }

    @inline def cast[Keyword, Value]: Keyword =:= Typed[Keyword, Value] = summon[Keyword =:= Typed[Keyword, Value]]

    def apply[Keyword, Value](keyword: Keyword): Typed[Keyword, Value] = keyword

  }

  def IsKeyword[Keyword, Value](using IsKeyword[Keyword, Value]) = summon[IsKeyword[Keyword, Value]]

  trait IsKeyword[Keyword, Value]

  extension [Keyword, Domain, Value](keyword: Keyword)
    @inline def cpsApply(using
        dsl: Dsl[Keyword, Domain, Value]
    )(handler: Value => Domain)(using DummyImplicit): Domain = {
      dsl.cpsApply(keyword, handler)
    }

}

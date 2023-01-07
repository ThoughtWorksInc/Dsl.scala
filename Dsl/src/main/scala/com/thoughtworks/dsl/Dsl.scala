package com.thoughtworks.dsl

private type !![R, +A] = (A => R) => R

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

/** The domain-specific interpreter for `Keyword` in `Domain`, which is a
  * dependent type type class that registers an asynchronous callback function,
  * to handle the `Value` inside `Keyword`.
  *
  * @tparam Value
  *   The value held inside `Keyword`.
  * @author
  *   杨博 (Yang Bo)
  * @example
  *   Creating a collaborative DSL in
  *   [[https://github.com/ThoughtWorksInc/Dsl.scala Dsl.scala]] is easy. Only
  *   two steps are required:
  *
  *   - Defining their domain-specific
  *     [[com.thoughtworks.dsl.Dsl.Keyword Keyword]].
  *   - Implementing this [[Dsl]] type class, which is an interpreter for an
  *     [[com.thoughtworks.dsl.Dsl.Keyword Keyword]].
  */
@implicitNotFound(
  "The keyword:\n ${Keyword}\nis not supported inside a function that returns:\n${Domain}."
)
opaque type Dsl[-Keyword, Domain, +Value] <: (
    Keyword,
    (Value => Domain)
) => Domain = (Keyword, (Value => Domain)) => Domain

private[dsl] trait LowPriorityDsl1 { this: Dsl.type =>

  opaque type Searching[-Keyword, Domain, +Value] <: Dsl[
    Keyword,
    Domain,
    Value
  ] = Dsl[Keyword, Domain, Value]
  object Searching
      extends Searching.AtomicThenStackSafeDerivedThenComposedThenStackUnsafeDerived:
    private[Searching] trait StackUnsafeDerived:
      given [Keyword, UnsafeDomain, Value](using
          dsl: Dsl.Derived.StackUnsafe[Keyword, UnsafeDomain, Value]
      ): Searching[Keyword, UnsafeDomain, Value] = dsl
    private[Searching] trait StackSafeDerivedThenStackUnsafeDerived
        extends Searching.StackUnsafeDerived:
      given [Keyword, DerivedDomain, Value](using
          dsl: Dsl.Derived.StackSafe[Keyword, DerivedDomain, Value]
      ): Searching[Keyword, DerivedDomain, Value] = dsl
    private[Searching] trait ComposedThenStackSafeDerivedThenStackUnsafeDerived
        extends Searching.StackSafeDerivedThenStackUnsafeDerived:
      given [ComposedKeyword, Domain, Value](using
          dsl: Dsl.Composed[ComposedKeyword, Domain, Value]
      ): Searching[ComposedKeyword, Domain, Value] = dsl
    private[Searching] trait AtomicThenComposedThenStackSafeDerivedThenStackUnsafeDerived
        extends Searching.ComposedThenStackSafeDerivedThenStackUnsafeDerived:
      given [Keyword, Domain, Value](using
          dsl: Dsl.Original[Keyword, Domain, Value]
      ): Searching[Keyword, Domain, Value] = dsl
    object AtomicThenComposedThenStackSafeDerivedThenStackUnsafeDerived
        extends AtomicThenComposedThenStackSafeDerivedThenStackUnsafeDerived

    private[Searching] trait ComposedThenStackUnsafeDerived
        extends Searching.StackUnsafeDerived:
      given [ComposedKeyword, Domain, Value](using
          dsl: Dsl.Composed[ComposedKeyword, Domain, Value]
      ): Searching[ComposedKeyword, Domain, Value] = dsl
    private[Searching] trait StackSafeDerivedThenComposedThenStackUnsafeDerived
        extends Searching.ComposedThenStackUnsafeDerived:
      given [Keyword, DerivedDomain, Value](using
          dsl: Dsl.Derived.StackSafe[Keyword, DerivedDomain, Value]
      ): Searching[Keyword, DerivedDomain, Value] = dsl
    private[Searching] trait AtomicThenStackSafeDerivedThenComposedThenStackUnsafeDerived
        extends Searching.StackSafeDerivedThenComposedThenStackUnsafeDerived:
      given [Keyword, Domain, Value](using
          dsl: Dsl.Original[Keyword, Domain, Value]
      ): Searching[Keyword, Domain, Value] = dsl
    object AtomicThenStackSafeDerivedThenComposedThenStackUnsafeDerived
        extends AtomicThenStackSafeDerivedThenComposedThenStackUnsafeDerived
  end Searching

  private def deriveFunction1Dsl[Keyword, State, Domain, Value](using
      restDsl: Dsl.Searching[Keyword, Domain, Value]
  ): Dsl[Keyword, State => Domain, Value] = Dsl {
    (keyword: Keyword, handler: Value => State => Domain) =>
      val restDsl1 = restDsl
      locally { (state: State) =>
        val handler1 = handler
        restDsl1(keyword, handler1(_)(state))
      }
  }

  given [Keyword, State, Domain, Value](using
      Dsl.IsStackSafe[Domain],
      Dsl.Searching[Keyword, Domain, Value]
  ): Dsl.Derived.StackSafe[Keyword, State => Domain, Value] =
    Dsl.Derived.StackSafe(deriveFunction1Dsl)

  given [Keyword, State, Domain, Value](using
      util.NotGiven[Dsl.IsStackSafe[Domain]],
      Dsl.Searching[Keyword, Domain, Value]
  ): Dsl.Derived.StackUnsafe[Keyword, State => Domain, Value] =
    Dsl.Derived.StackUnsafe(deriveFunction1Dsl)

}

private[dsl] trait LowPriorityDsl0 extends LowPriorityDsl1 { this: Dsl.type =>

  private def throwableContinuationDsl[Keyword, LeftDomain, Value](implicit
      restDsl: Dsl.Searching[Keyword, LeftDomain, Value]
  ): Dsl[Keyword, LeftDomain !! Throwable, Value] = Dsl {
    (keyword, handler) => (continue: Throwable => LeftDomain) =>
      restDsl(
        keyword,
        { value =>
          TrampolineContinuation { () =>
            handler(value)
          }(continue)
        }
      )
  }

  given [Keyword, LeftDomain, Value](using
      Dsl.IsStackSafe[LeftDomain],
      Dsl.Searching[Keyword, LeftDomain, Value]
  ): Dsl.Derived.StackSafe[Keyword, LeftDomain !! Throwable, Value] =
    Dsl.Derived.StackSafe(throwableContinuationDsl)
  given [Keyword, LeftDomain, Value](using
      util.NotGiven[Dsl.IsStackSafe[LeftDomain]],
      Dsl.Searching[Keyword, LeftDomain, Value]
  ): Dsl.Derived.StackUnsafe[Keyword, LeftDomain !! Throwable, Value] =
    Dsl.Derived.StackUnsafe(throwableContinuationDsl)

}

inline def Dsl[Keyword, Domain, Value](using
    dummyImplicit: DummyImplicit = DummyImplicit.dummyImplicit
): (
  (
      Keyword,
      (Value => Domain)
  ) => Domain
) =:= Dsl[Keyword, Domain, Value] =
  summon

object Dsl extends LowPriorityDsl0 {

  private[dsl] abstract class TrampolineFunction1[-A, +R] extends (A => R) {
    protected def step(): A => R
    @tailrec
    protected final def last(): A => R = {
      step() match {
        case trampoline: TrampolineFunction1[A, R] =>
          trampoline.last()
        case notTrampoline =>
          notTrampoline
      }
    }

    def apply(state: A): R = {
      last()(state)
    }

  }
  object TrampolineFunction1 {
    def apply[A, R](trampoline: TrampolineFunction1[A, R]) = trampoline
  }

  private[dsl] abstract class TrampolineContinuation[LeftDomain]
      extends TrampolineFunction1[Throwable => LeftDomain, LeftDomain] {

    override final def apply(handler: Throwable => LeftDomain): LeftDomain = {
      val protectedContinuation: LeftDomain !! Throwable =
        try {
          last()
        } catch {
          case NonFatal(e) =>
            return handler(e)
        }
      protectedContinuation(handler)
    }
  }
  private[dsl] object TrampolineContinuation {
    def apply[LeftDomain](continuation: TrampolineContinuation[LeftDomain]) =
      continuation
  }

  trait IsStackSafe[Domain]
  object IsStackSafe extends IsStackSafe.LowPriority0:
    private[IsStackSafe] trait LowPriority0:
      given [R, A]: IsStackSafe[R => A] with {}
    given [A]: IsStackSafe[TailRec[A]] with {}

  /** Includes [[Dsl]]s derived from other [[Dsl]]s for the same [[Keyword]] in
    * a simpler domain. For example, a [[Dsl.Derived.StackUnsafe]] for
    * [[keywords.Yield]] in the domain `Vector[Int] !! String` is derived from
    * the [[Dsl]] for [[keywords.Yield]] in the domain `Vector[Int]`.
    */
  object Derived:
    /** A [[Dsl]] derived from a stack-safe domain, e.g. [[domains.Task]] or
      * [[scala.util.control.TailCalls.TailRec]].
      */
    opaque type StackSafe[-Keyword, Domain, +Value] <: Dsl[
      Keyword,
      Domain,
      Value
    ] =
      Dsl[Keyword, Domain, Value]
    def StackSafe[Keyword, Domain, Value]: (
      (
          Keyword,
          (Value => Domain)
      ) => Domain
    ) =:= StackSafe[Keyword, Domain, Value] =
      summon

    /** A [[Dsl]] derived from a stack-unsafe domain, e.g.
      * [[scala.concurrent.Future]]
      */
    opaque type StackUnsafe[-Keyword, Domain, +Value] <: Dsl[
      Keyword,
      Domain,
      Value
    ] =
      Dsl[Keyword, Domain, Value]
    def StackUnsafe[Keyword, Domain, Value]: (
      (
          Keyword,
          (Value => Domain)
      ) => Domain
    ) =:= StackUnsafe[Keyword, Domain, Value] =
      summon

  /** An [[Dsl]] for a control flow [[Keyword]], composed of other [[Dsl]]s for
    * subtree of the [[Keyword]]. For example, a [[Dsl.Composed]] for
    * [[keywords.If]] is composed of the [[Dsl]] for the condition, the [[Dsl]]
    * for the `then` clause, and the [[Dsl]] for the `else` clause.
    */
  opaque type Composed[-Keyword, Domain, +Value] <: Dsl[
    Keyword,
    Domain,
    Value
  ] = Dsl[Keyword, Domain, Value]
  def Composed[Keyword, Domain, Value]: (
    (
        Keyword,
        (Value => Domain)
    ) => Domain
  ) =:= Composed[Keyword, Domain, Value] =
    summon

  /** An original [[Dsl]] for a [[Keyword]], i.e. neither [[Derived.StackSafe]]
    * nor [[Derived.StackUnsafe]] nor [[Composed]].
    */
  opaque type Original[-Keyword, Domain, +Value] <: Dsl[
    Keyword,
    Domain,
    Value
  ] =
    Dsl[Keyword, Domain, Value]

  def Original[Keyword, Domain, Value]: (
    (
        Keyword,
        (Value => Domain)
    ) => Domain
  ) =:= Original[Keyword, Domain, Value] =
    summon

  extension [Keyword, Value](
      inline from: Keyword
  )(using inline asKeyword: Dsl.IsKeyword[Keyword, Value])
    transparent inline def unary_! : Value = {
      Dsl.shift[Keyword, Value](from)
    }

  sealed trait HasValueOrElement[KeywordOrView, Element]:
    extension (keywordOrView: KeywordOrView)
      def flatMap[Mapped <: For.Yield[MappedElement], MappedElement](
          flatMapper: Element => Mapped
      ) = For.Yield.FlatMap(keywordOrView, flatMapper)
      def map[Mapped](mapper: Element => Mapped) =
        For.Yield.Map(keywordOrView, mapper)
      def foreach[Nested <: For.Do](action: Element => Nested) =
        For.Do.FlatForeach(keywordOrView, action)
      def foreach(action: Element => Unit) =
        For.Do.Foreach(keywordOrView, action)
      def withFilter(filter: Element => Boolean) =
        For.Yield.WithFilter(keywordOrView, filter)
  // TODO: Implement `foreach` and `map` in macros to support !-notation in `do` block or `yield` block
  object HasValueOrElement {
    given [KeywordOrView <: For.Yield[Element], Element]
        : HasValueOrElement[KeywordOrView, Element] with {}
  }

  /** The AST returned from a `for`...`yield` or a `for`...`do` expression.
    *
    * Note that a [[For]] does not directly support !-notation. Instead,
    * [[keywords.Each.ToView]] is used to convert a [[For]] to a [[Keyword]]
    * that supports !-notation.
    */
  sealed trait For
  object For {

    /** The AST returned from a `for`...`do` expression. */
    sealed trait Do extends For
    object Do {
      final case class KeywordForeach[Upstream, UpstreamElement, UnitKeyword](
          upstream: Upstream,
          action: UpstreamElement => UnitKeyword
      ) extends Do
      final case class Foreach[Upstream, UpstreamElement](
          upstream: Upstream,
          action: UpstreamElement => Unit
      ) extends Do
      final case class FlatForeach[Upstream, UpstreamElement, Nested <: Do](
          upstream: Upstream,
          action: UpstreamElement => Nested
      ) extends Do
    }

    /** The AST returned from a `for`...`yield` expression. */
    sealed trait Yield[Element] extends For
    object Yield {
      final case class KeywordMap[
          Upstream,
          UpstreamElement,
          ElementKeyword,
          Element
      ](upstream: Upstream, mapper: UpstreamElement => ElementKeyword)
          extends Yield[Element]
      final case class Map[Upstream, UpstreamElement, Element](
          upstream: Upstream,
          mapper: UpstreamElement => Element
      ) extends Yield[Element]
      final case class FlatMap[Upstream, UpstreamElement, Mapped <: Yield[
        Element
      ], Element](upstream: Upstream, flatMapper: UpstreamElement => Mapped)
          extends Yield[Element]
      final case class WithFilter[Upstream, Element](
          upstream: Upstream,
          filter: Element => Boolean
      ) extends Yield[Element]
    }
  }

  private def derivedTailRecDsl[Keyword, Domain, Value](implicit
      restDsl: Dsl.Searching[Keyword, Domain, Value]
  ): Dsl[Keyword, TailRec[Domain], Value] = Dsl {
    (keyword: Keyword, handler: (Value => TailRec[Domain])) =>
      TailCalls.done {
        restDsl(
          keyword,
          { value =>
            handler(value).result
          }
        )
      }
  }
  given [Keyword, Domain, Value](using
      Dsl.IsStackSafe[Domain],
      Dsl.Searching[Keyword, Domain, Value]
  ): Dsl.Derived.StackSafe[Keyword, TailRec[Domain], Value] =
    Dsl.Derived.StackSafe(derivedTailRecDsl)
  given [Keyword, Domain, Value](using
      util.NotGiven[Dsl.IsStackSafe[Domain]],
      Dsl.Searching[Keyword, Domain, Value]
  ): Dsl.Derived.StackUnsafe[Keyword, TailRec[Domain], Value] =
    Dsl.Derived.StackUnsafe(derivedTailRecDsl)

  private def derivedThrowableTailRecDsl[Keyword, LeftDomain, Value](implicit
      restDsl: Dsl.Searching[Keyword, LeftDomain !! Throwable, Value]
  ): Dsl[Keyword, TailRec[LeftDomain] !! Throwable, Value] =
    Dsl {
      (
          keyword: Keyword,
          handler: (Value => TailRec[LeftDomain] !! Throwable)
      ) => (tailRecFailureHandler: Throwable => TailRec[LeftDomain]) =>
        TailCalls.done(
          restDsl(
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
  given [Keyword, LeftDomain, TailRecValue](using
      Dsl.IsStackSafe[LeftDomain],
      Dsl.Searching[Keyword, LeftDomain !! Throwable, TailRecValue]
  ): Dsl.Derived.StackSafe[Keyword, TailRec[
    LeftDomain
  ] !! Throwable, TailRecValue] =
    Dsl.Derived.StackSafe(derivedThrowableTailRecDsl)
  given [Keyword, LeftDomain, TailRecValue](using
      util.NotGiven[Dsl.IsStackSafe[LeftDomain]],
      Dsl.Searching[Keyword, LeftDomain !! Throwable, TailRecValue]
  ): Dsl.Derived.StackUnsafe[Keyword, TailRec[
    LeftDomain
  ] !! Throwable, TailRecValue] =
    Dsl.Derived.StackUnsafe(derivedThrowableTailRecDsl)

  @FunctionalInterface
  trait Lift[From, +To] extends (From => To)
  private[dsl] trait LowPriorityLift0 { this: Lift.type =>

    given [From, Intermediate, To](using
        step1: OneStep[Intermediate, To],
        step0: Lift[From, Intermediate]
    ): Lift[From, To] with {
      def apply(from: From): To = {
        step1(step0(from))
      }
    }

  }
  object Lift extends LowPriorityLift0 {
    @FunctionalInterface
    trait OneStep[From, +To] extends Lift[From, To]

    given [CastFrom, CastTo >: CastFrom]: Lift[CastFrom, CastTo] with {
      def apply(from: CastFrom): CastTo = {
        from
      }
    }

    private[Lift] trait LowPriorityOneStep1 { this: OneStep.type =>
      given [Collection, Element](using
          factory: collection.Factory[Element, Collection]
      ): OneStep[Element, Collection] = { element =>
        factory.fromSpecific(element :: Nil)
      }
    }

    private[Lift] trait LowPriorityOneStep0 extends LowPriorityOneStep1 {
      this: OneStep.type =>
      given [R, F, A]: OneStep[R, A => R] = { r => Function.const(r) }
    }

    object OneStep extends LowPriorityOneStep0 {

      given [LeftDomain, RightDomain]
          : OneStep[RightDomain, LeftDomain !! RightDomain] = r => _(r)

      given [Element](using
          ExecutionContext
      ): OneStep[Element, Future[Element]] = {
        Future.successful
      }
    }

  }

  opaque type Run[Keyword, Domain, Value] <: Keyword => Domain =
    Keyword => Domain

  object Run {

    given [Keyword, Domain, Value](using
        dsl: /*=>*/ Dsl.Searching[Keyword, Domain, Value],
        lift: /*=>*/ Lift[Value, Domain]
    ): Run[Keyword, Domain, Value] = { dsl.apply(_, lift) }

  }

  type Keyword = Keyword.Opaque | Keyword.Trait
  object Keyword {

    /** A marker trait that denotes a keyword class, enabling extension method
      * defined in [[Dsl]] for subclasses of [[Keyword.Trait]].
      */
    trait Trait extends Any

    /** A marker trait that denotes a keyword opaque type, enabling extension
      * method defined in [[Dsl]] for its subtypes of [[Keyword.Opaque]].
      */
    opaque type Opaque = Any
    object Opaque {
      opaque type Of[+Self] <: Self & Opaque = Self
      def Of[Self]: Self =:= Of[Self] = summon
    }
  }

  trait IsKeyword[Keyword, Value] extends HasValueOrElement[Keyword, Value]:
    extension (keyword: Keyword)
      @inline def to[Domain[_]](using
          run: Run[Keyword, Domain[Value], Value]
      ): Domain[Value] = {
        run(keyword)
      }

      @inline def as[Domain](using
          run: Run[Keyword, Domain, Value]
      ): Domain = {
        run(keyword)
      }

  extension [Keyword, Value](keyword: Keyword)
    @inline def cpsApply[Domain](using
        dsl: Dsl.Searching[Keyword, Domain, Value]
    )(handler: Value => Domain): Domain = {
      dsl(keyword, handler)
    }

  @annotation.compileTimeOnly(
    """This method must be called only inside a `reset` or `*` code block."""
  )
  def shift[Keyword, Value](keyword: Keyword): Value = ???

}

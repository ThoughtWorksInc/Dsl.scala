package com.thoughtworks.dsl
import concurrent.Future
import concurrent.ExecutionContext

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
@annotation.implicitNotFound("The keyword ${Keyword} is not supported inside a function that returns ${Domain}.")
trait Dsl[-Keyword, Domain, +Value] {

  /** Registers an asynchronous callback `handler` on `keyword`, to handle the `Value`. */
  def cpsApply(keyword: Keyword, handler: Value => Domain): Domain

}

object Dsl {

  type Continuation[R, +A] = (A => R) => R

  object Continuation {}

  type !![R, +A] = Continuation[R, A]
  val !! = Continuation

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
        step1: /*=>*/ OneStep[Intermediate, To],
        step0: /*=>*/ Lift[From, Intermediate]
    ): Lift[From, To] with {
      def apply(from: From): To = {
        step1(step0(from))
      }
    }

    import Dsl.!!
    given [LeftDomain, RightDomain]: OneStep[RightDomain, LeftDomain !! RightDomain] = r => _(r)

    given [Element](using
        concurrent.ExecutionContext
    ): OneStep[Element, concurrent.Future[Element]] = {
      concurrent.Future.successful
    }

    given [State, Element](using
        concurrent.ExecutionContext
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

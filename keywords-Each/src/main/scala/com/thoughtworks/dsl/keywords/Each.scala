package com.thoughtworks.dsl
package keywords

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.{!!, AsKeyword}

import scala.collection._
import scala.language.implicitConversions

import scala.collection.mutable.Builder

/** Iterates though each element in [[elements]].
  * @author
  *   杨博 (Yang Bo)
  *
  * @example
  *   [[Each]] keywords can be used to calculate cartesian product.
  *
  * {{{
  *           import com.thoughtworks.dsl.reset, reset._
  *           def cartesianProduct = reset (List(!Each(Array(1, 2, 3)) * !Each(Vector(1, 10, 100, 1000))))
  *           cartesianProduct should be(List(1, 10, 100, 1000, 2, 20, 200, 2000, 3, 30, 300, 3000))
  * }}}
  * @see
  *   [[Dsl.For]] if you want to use traditional `for` comprehension instead of
  *   !-notation.
  */
final case class Each[Element](elements: Traversable[Element])
    extends Dsl.Keyword.Trait
object Each {
  opaque type ToView[Comprehension] <: Dsl.Keyword.Opaque =
    Dsl.Keyword.Opaque.Of[Comprehension]

  object ToView {

    def apply[Comprehension]
        : Comprehension =:= Each.ToView[Comprehension] =
      Dsl.Keyword.Opaque.Of.apply

    def toKeyword[ComprehensionOrKeyword, Keyword](
        comprehension: ComprehensionOrKeyword
    )(using
        typeClass: ToKeyword[ComprehensionOrKeyword, Keyword]
    ): Keyword = typeClass(comprehension)

    opaque type ToKeyword[-ComprehensionOrKeyword, +Keyword] <: (
        ComprehensionOrKeyword => Keyword
    ) = ComprehensionOrKeyword => Keyword
    object ToKeyword {

      def apply[ComprehensionOrKeyword, Keyword]: (
          ComprehensionOrKeyword => Keyword
      ) =:= ToKeyword[ComprehensionOrKeyword, Keyword] = summon

      given [
          Upstream,
          UpstreamElement,
          Nested <: Dsl.For.Do,
          UpstreamKeyword,
          NestedKeyword
      ](using
          upstreamToKeyword: ToKeyword[
            Upstream,
            UpstreamKeyword
          ],
          mappedToKeyword: ToKeyword[Nested, NestedKeyword]
      ): ToKeyword[Dsl.For.Do.FlatForeach[
        Upstream,
        UpstreamElement,
        Nested,
      ], FlatMap[
        UpstreamKeyword,
        collection.View[UpstreamElement],
        FlatMap[Each[
          UpstreamElement
        ], UpstreamElement, NestedKeyword]
      ]] = { case Dsl.For.Do.FlatForeach(upstream, flatAction) =>
        FlatMap(
          upstreamToKeyword(upstream),
          { upstreamCollection =>
            FlatMap(
              Each(upstreamCollection),
              flatAction.andThen(mappedToKeyword)
            )
          }
        )
      }

      given [
          Upstream,
          UpstreamElement,
          Mapped <: Dsl.For.Yield[Element],
          Element,
          UpstreamKeyword,
          MappedKeyword
      ](using
          upstreamToKeyword: ToKeyword[
            Upstream,
            UpstreamKeyword
          ],
          mappedToKeyword: ToKeyword[Mapped, MappedKeyword]
      ): ToKeyword[Dsl.For.Yield.FlatMap[
        Upstream,
        UpstreamElement,
        Mapped,
        Element
      ], FlatMap[
        UpstreamKeyword,
        collection.View[UpstreamElement],
        FlatMap[Each[
          UpstreamElement
        ], UpstreamElement, MappedKeyword]
      ]] = { case Dsl.For.Yield.FlatMap(upstream, flatMapper) =>
        FlatMap(
          upstreamToKeyword(upstream),
          { upstreamCollection =>
            FlatMap(
              Each(upstreamCollection),
              flatMapper.andThen(mappedToKeyword)
            )
          }
        )
      }

      given [
          Upstream,
          UpstreamElement,
          UpstreamKeyword
      ](using
          upstreamToKeyword: ToKeyword[
            Upstream,
            UpstreamKeyword
          ]
      ): ToKeyword[Dsl.For.Do.Foreach[
        Upstream,
        UpstreamElement,
      ], FlatMap[
        UpstreamKeyword,
        collection.View[UpstreamElement],
        Pure[Unit]
      ]] = { case Dsl.For.Do.Foreach(upstream, action) =>
        FlatMap(
          upstreamToKeyword(upstream),
          { (upstreamCollection: collection.View[UpstreamElement]) =>
            Pure(
              upstreamCollection.foreach(action)
            )
          }
        )
      }

      given [
          Upstream,
          UpstreamElement,
          Element,
          UpstreamKeyword
      ](using
          upstreamToKeyword: ToKeyword[
            Upstream,
            UpstreamKeyword
          ]
      ): ToKeyword[Dsl.For.Yield.Map[
        Upstream,
        UpstreamElement,
        Element
      ], FlatMap[
        UpstreamKeyword,
        collection.View[UpstreamElement],
        Pure[collection.View[Element]]
      ]] = { case Dsl.For.Yield.Map(upstream, mapper) =>
        FlatMap(
          upstreamToKeyword(upstream),
          { (upstreamCollection: collection.View[UpstreamElement]) =>
            Pure(
              collection.View.Map(upstreamCollection, mapper)
            )
          }
        )
      }

      given [
          Upstream,
          Element,
          UpstreamKeyword
      ](using
          upstreamToKeyword: ToKeyword[
            Upstream,
            UpstreamKeyword
          ]
      ): ToKeyword[Dsl.For.Yield.WithFilter[
        Upstream,
        Element
      ], FlatMap[
        UpstreamKeyword,
        collection.View[Element],
        Pure[collection.View[Element]]
      ]] = { case Dsl.For.Yield.WithFilter(upstream, filter) =>
        FlatMap(
          upstreamToKeyword(upstream),
          { (upstreamCollection: collection.View[Element]) =>
            Pure(
              collection.View
                .Filter(upstreamCollection, filter, isFlipped = false)
            )
          }
        )
      }

      given [
          Upstream,
          Element
      ](using
          isUpstreamKeyword: Dsl.AsKeyword.IsKeyword[Upstream, Element]
      ): ToKeyword[Dsl.For.Yield.WithFilter[
        Upstream,
        Element
      ], FlatMap[Upstream, Element, Pure[collection.View[Element]]]] = {
        case Dsl.For.Yield.WithFilter(upstream, filter) =>
          FlatMap(
            upstream,
            { (element: Element) =>
              Pure(if (filter(element)) {
                collection.View.Single(element)
              } else {
                collection.View.Empty
              })
            }
          )
      }

      given [
          Upstream,
          UpstreamElement
      ](using
          isUpstreamKeyword: Dsl.AsKeyword.IsKeyword[Upstream, UpstreamElement]
      ): ToKeyword[Dsl.For.Do.Foreach[
        Upstream,
        UpstreamElement,
      ], FlatMap[
        Upstream,
        UpstreamElement,
        Pure[Unit]
      ]] = Pure.apply.liftCo[[X] =>> ToKeyword[Dsl.For.Do.Foreach[
        Upstream,
        UpstreamElement,
      ], FlatMap[
        Upstream,
        UpstreamElement,
        X
      ]]] { case Dsl.For.Do.Foreach(upstream, action) =>
        FlatMap(
          upstream,
          action
        )
      }

      given [
          Upstream,
          UpstreamElement,
          Element
      ](using
          isUpstreamKeyword: Dsl.AsKeyword.IsKeyword[Upstream, UpstreamElement]
      ): ToKeyword[Dsl.For.Yield.Map[
        Upstream,
        UpstreamElement,
        Element
      ], FlatMap[
        Upstream,
        UpstreamElement,
        Pure[collection.View[Element]]
      ]] = { case Dsl.For.Yield.Map(upstream, mapper) =>
        FlatMap(
          upstream,
          element => Pure(collection.View.Single(mapper(element)))
        )
      }

      given [
          Upstream,
          UpstreamElement,
          Nested <: Dsl.For.Do,
          NestedKeyword
      ](using
          isUpstreamKeyword: Dsl.AsKeyword.IsKeyword[Upstream, UpstreamElement],
          mappedToKeyword: ToKeyword[Nested, NestedKeyword]
      ): ToKeyword[Dsl.For.Do.FlatForeach[
        Upstream,
        UpstreamElement,
        Nested,
      ], FlatMap[
        Upstream,
        UpstreamElement,
        NestedKeyword
      ]] = { case Dsl.For.Do.FlatForeach(upstream, flatAction) =>
        FlatMap(upstream, flatAction.andThen(mappedToKeyword))
      }

      given [
          Upstream,
          UpstreamElement,
          Mapped <: Dsl.For.Yield[Element],
          Element,
          MappedKeyword
      ](using
          isUpstreamKeyword: Dsl.AsKeyword.IsKeyword[Upstream, UpstreamElement],
          mappedToKeyword: ToKeyword[Mapped, MappedKeyword]
      ): ToKeyword[Dsl.For.Yield.FlatMap[
        Upstream,
        UpstreamElement,
        Mapped,
        Element
      ], FlatMap[
        Upstream,
        UpstreamElement,
        MappedKeyword
      ]] = { case Dsl.For.Yield.FlatMap(upstream, flatMapper) =>
        FlatMap(upstream, flatMapper.andThen(mappedToKeyword))
      }

    }

    given [
        Comprehension,
        Keyword,
        Value
    ](using
        toKeyword: ToKeyword[Comprehension, Keyword],
        isKeyword: Dsl.AsKeyword.IsKeyword[
          Keyword,
          Value
        ]
    ): Dsl.AsKeyword.IsKeyword[Each.ToView[Comprehension], Value]
      with {}

    given [
        Comprehension,
        Keyword,
        Domain,
        Value
    ](using
        toKeyword: ToKeyword[Comprehension, Keyword],
        isKeyword: Dsl.AsKeyword.SearchIsKeywordFirst[
          Keyword,
          Keyword,
          Value
        ],
        polyCont: Dsl.PolyCont[
          Keyword,
          Domain,
          Value
        ]
    ): Dsl.PolyCont[Each.ToView[Comprehension], Domain, Value] = {
      (as, handler) =>
        polyCont.cpsApply(toKeyword(as), handler)
    }
  }

  given [Element]: AsKeyword.IsKeyword[Each[Element], Element] with {}

  extension [FA, A](inline fa: FA)(using
      inline notKeyword: util.NotGiven[
        FA <:< Dsl.Keyword
      ],
      inline asFA: FA <:< Traversable[A]
  )
    transparent inline def unary_! : A =
      Dsl.shift(Each(asFA(fa))): A

  private def toLinearSeq[Element](
      i: IterableOnce[Element]
  ): LinearSeq[Element] = {
    i match {
      case linearSeq: LinearSeq[Element] =>
        linearSeq
      case notSeq =>
        LazyList.from(notSeq)
    }
  }
  given [
      Element,
      MappedKeyword,
      MappedValue <: IterableOps[
        MappedElement,
        _,
        _
      ],
      MappedElement,
      Domain
  ](using
      isKeyword: AsKeyword.IsKeyword[
        MappedKeyword,
        MappedValue
      ],
      factory: Factory[MappedElement, MappedValue],
      blockDsl: Dsl.PolyCont[MappedKeyword, Domain, MappedValue]
  ): Dsl.PolyCont[
    FlatMap[Each[Element], Element, MappedKeyword],
    Domain,
    MappedValue
  ] = {
    case (
          FlatMap(Each(sourceCollection), flatMapper),
          handler
        ) =>
      @inline def loop(
          seqOps: LinearSeq[Element],
          viewHandler: View[MappedElement] => Domain
      ): Domain = {
        seqOps.headOption match {
          case Some(head) =>
            blockDsl.cpsApply(
              flatMapper(head),
              { mappedHead =>
                loop(
                  seqOps.tail.asInstanceOf[LinearSeq[Element]],
                  { mappedTail =>
                    viewHandler(View.Concat(mappedHead, mappedTail))
                  }
                )
              }
            )
          case None =>
            viewHandler(View.Empty)
        }
      }
      loop(
        toLinearSeq(sourceCollection),
        { view =>
          handler(factory.fromSpecific(view))
        }
      )
  }

  given [
      Element,
      MappedKeyword,
      Domain
  ](using
      blockDsl: Dsl.PolyCont[MappedKeyword, Domain, Unit]
  ): Dsl.PolyCont[
    FlatMap[Each[Element], Element, MappedKeyword],
    Domain,
    Unit
  ] = {
    case (
          FlatMap(Each(sourceCollection), flatMapper),
          handler
        ) =>
      @inline def loop(
          seqOps: LinearSeq[Element],
          viewHandler: () => Domain
      ): Domain = {
        seqOps.headOption match {
          case Some(head) =>
            blockDsl.cpsApply(
              flatMapper(head),
              { mappedHead =>
                loop(
                  seqOps.tail.asInstanceOf[LinearSeq[Element]],
                  { () =>
                    viewHandler()
                  }
                )
              }
            )
          case None =>
            viewHandler()
        }
      }
      loop(
        toLinearSeq(sourceCollection),
        { () =>
          handler(())
        }
      )
  }

}

package com.thoughtworks.dsl
package keywords

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.IsKeyword

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
  *           import com.thoughtworks.dsl.macros.Reset.Default.*
  *           def cartesianProduct = reset (List(!Each(Array(1, 2, 3)) * !Each(Vector(1, 10, 100, 1000))))
  *           cartesianProduct should be(List(1, 10, 100, 1000, 2, 20, 200, 2000, 3, 30, 300, 3000))
  * }}}
  * @see
  *   [[Dsl.For]] if you want to use traditional `for` comprehension instead of
  *   !-notation.
  */
opaque type Each[+Element] <: Dsl.Keyword.Opaque =
  Dsl.Keyword.Opaque.Of[Iterable[Element]]

def Each[Element](using
    dummyImplicit: DummyImplicit = DummyImplicit.dummyImplicit
): Iterable[Element] =:= Each[Element] =
  Dsl.Keyword.Opaque.Of
object Each {

  final case class To[
      +ForYield <: Dsl.For.Yield[Element],
      Element,
      +Collection
  ](
      factory: Factory[Element, Collection]
  )(val forYield: ForYield)
      extends Dsl.Keyword.Trait

  object To {
    given To[ForYield <: Dsl.For.Yield[Element], Element, Collection]
        : Dsl.IsKeyword[To[ForYield, Element, Collection], Collection] with {}
    given [
        ForYield <: Dsl.For.Yield[Element],
        Element,
        Collection,
        Domain
    ](using
        toViewDsl: Dsl.Searching[Each.ToView[ForYield], Domain, View[Element]]
    ): Dsl.Composed[
      Each.To[ForYield, Element, Collection],
      Domain,
      Collection
    ] = Dsl.Composed { (keyword, handler) =>
      val factory = keyword.factory
      toViewDsl(
        ToView(keyword.forYield),
        { view => handler(view.to(factory)) }
      )
    }
  }

  opaque type ToView[+Comprehension] <: Dsl.Keyword.Opaque =
    Dsl.Keyword.Opaque.Of[Comprehension]

  def ToView[Comprehension](using
      dummyImplicit: DummyImplicit = DummyImplicit.dummyImplicit
  ): Comprehension =:= Each.ToView[Comprehension] =
    Dsl.Keyword.Opaque.Of
  object ToView {

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
        Nested
      ], FlatMap[
        UpstreamKeyword,
        FlatMap[Each[
          UpstreamElement
        ], NestedKeyword]
      ]] = { case Dsl.For.Do.FlatForeach(upstream, flatAction) =>
        FlatMap(
          upstreamToKeyword(upstream),
          { (upstreamCollection: Iterable[UpstreamElement]) =>
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
        FlatMap[Each[
          UpstreamElement
        ], MappedKeyword]
      ]] = { case Dsl.For.Yield.FlatMap(upstream, flatMapper) =>
        FlatMap(
          upstreamToKeyword(upstream),
          { (upstreamCollection: collection.View[UpstreamElement]) =>
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
        UpstreamElement
      ], FlatMap[
        UpstreamKeyword,
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
          isUpstreamKeyword: Dsl.IsKeyword[Upstream, Element]
      ): ToKeyword[Dsl.For.Yield.WithFilter[
        Upstream,
        Element
      ], FlatMap[Upstream, Pure[collection.View[Element]]]] = {
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
          isUpstreamKeyword: Dsl.IsKeyword[Upstream, UpstreamElement]
      ): ToKeyword[Dsl.For.Do.Foreach[
        Upstream,
        UpstreamElement
      ], FlatMap[
        Upstream,
        Pure[Unit]
      ]] = Pure.liftCo[[X] =>> ToKeyword[Dsl.For.Do.Foreach[
        Upstream,
        UpstreamElement
      ], FlatMap[
        Upstream,
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
          isUpstreamKeyword: Dsl.IsKeyword[Upstream, UpstreamElement]
      ): ToKeyword[Dsl.For.Yield.Map[
        Upstream,
        UpstreamElement,
        Element
      ], FlatMap[
        Upstream,
        Pure[collection.View[Element]]
      ]] = { case Dsl.For.Yield.Map(upstream, mapper) =>
        FlatMap(
          upstream,
          (upstreamElement: UpstreamElement) =>
            Pure(collection.View.Single(mapper(upstreamElement)))
        )
      }

      given [
          Upstream,
          UpstreamElement,
          Nested <: Dsl.For.Do,
          NestedKeyword
      ](using
          isUpstreamKeyword: Dsl.IsKeyword[Upstream, UpstreamElement],
          mappedToKeyword: ToKeyword[Nested, NestedKeyword]
      ): ToKeyword[Dsl.For.Do.FlatForeach[
        Upstream,
        UpstreamElement,
        Nested
      ], FlatMap[
        Upstream,
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
          isUpstreamKeyword: Dsl.IsKeyword[Upstream, UpstreamElement],
          mappedToKeyword: ToKeyword[Mapped, MappedKeyword]
      ): ToKeyword[Dsl.For.Yield.FlatMap[
        Upstream,
        UpstreamElement,
        Mapped,
        Element
      ], FlatMap[
        Upstream,
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
        isKeyword: Dsl.IsKeyword[
          Keyword,
          Value
        ]
    ): Dsl.IsKeyword[Each.ToView[Comprehension], Value] with {}

    given [
        Comprehension,
        Keyword,
        Domain,
        Value
    ](using
        toKeyword: ToKeyword[Comprehension, Keyword],
        isKeyword: Dsl.IsKeyword[
          Keyword,
          Value
        ],
        polyCont: Dsl.Searching[
          Keyword,
          Domain,
          Value
        ]
    ): Dsl.Composed[Each.ToView[Comprehension], Domain, Value] = Dsl.Composed {
      (as, handler) =>
        polyCont(toKeyword(as), handler)
    }
  }

  given [Element]: IsKeyword[Each[Element], Element] with {}

  extension [FA, A](inline fa: FA)(using
      inline notKeyword: util.NotGiven[
        FA <:< Dsl.Keyword
      ],
      inline asFA: FA <:< Iterable[A]
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
      isKeyword: IsKeyword[
        MappedKeyword,
        MappedValue
      ],
      factory: Factory[MappedElement, MappedValue],
      blockDsl: Dsl.Searching[MappedKeyword, Domain, MappedValue]
  ): Dsl.Composed[
    FlatMap[Each[Element], MappedKeyword],
    Domain,
    MappedValue
  ] = Dsl.Composed {
    case (
          FlatMap(
            sourceCollection,
            flatMapper: (Element @unchecked => MappedKeyword)
          ),
          handler
        ) =>
      @inline def loop(
          seqOps: LinearSeq[Element],
          viewHandler: View[MappedElement] => Domain
      ): Domain = {
        seqOps.headOption match {
          case Some(head) =>
            blockDsl(
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
      blockDsl: Dsl.Searching[MappedKeyword, Domain, Unit]
  ): Dsl.Composed[
    FlatMap[Each[Element], MappedKeyword],
    Domain,
    Unit
  ] = Dsl.Composed {
    case (
          FlatMap(
            sourceCollection,
            flatMapper: (Element @unchecked => MappedKeyword)
          ),
          handler
        ) =>
      @inline def loop(
          seqOps: LinearSeq[Element],
          viewHandler: () => Domain
      ): Domain = {
        seqOps.headOption match {
          case Some(head) =>
            blockDsl(
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

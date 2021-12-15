package com.thoughtworks.dsl
package keywords

import scala.collection.Factory

opaque type ToView[Comprehension] <: Dsl.Keyword.Opaque =
  Dsl.Keyword.Opaque.Of[Comprehension]

object ToView {

  def apply[Comprehension]: Comprehension =:= ToView[Comprehension] =
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

    // TODO: Foreach

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
      FlatMap[Each[UpstreamElement], UpstreamElement, MappedKeyword]
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
  ): Dsl.AsKeyword.IsKeyword[ToView[Comprehension], Value] with {}

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
  ): Dsl.PolyCont[ToView[Comprehension], Domain, Value] = { (as, handler) =>
    polyCont.cpsApply(toKeyword(as), handler)
  }
}

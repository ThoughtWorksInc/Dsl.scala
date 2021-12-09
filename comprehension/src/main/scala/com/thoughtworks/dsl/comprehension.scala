package com.thoughtworks.dsl

import keywords._
import Dsl._
import scala.collection.WithFilter

object comprehension {
  extension [From, Keyword, Value](keyword: From)(using
      asKeyword: AsKeyword.SearchIsKeywordFirst[From, Keyword, Value]
  )
    @inline def to[Domain[_]](using
        run: Run[Keyword, Domain[Value], Value]
    ): Domain[Value] = {
      run(asKeyword(keyword))
    }

    @inline def as[Domain](using
        run: Run[Keyword, Domain, Value]
    ): Domain = {
      run(asKeyword(keyword))
    }

    @inline def map[MappedValue](
        mapper: Value => MappedValue
    ): FlatMap[Keyword, Value, Pure[MappedValue]] =
      FlatMap(asKeyword(keyword), Pure.apply.liftCo(mapper))

    @inline def flatMap[Mapped, MappedValue](
        flatMapper: Value => Mapped
    )(
        using /*erased*/ AsKeyword.IsKeyword[Mapped, MappedValue]
    ): FlatMap[Keyword, Value, Mapped] =
      FlatMap(asKeyword(keyword), flatMapper)

    @inline def withFilter[Mapped, MappedValue](
        filter: Value => Boolean
    ) =
      WithFilter(asKeyword(keyword), filter)
}

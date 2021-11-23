package com.thoughtworks.dsl

import keywords._
import Dsl._

given comprehension[Keyword, Value]: AnyRef {

  @inline def[Domain[?]](keyword: Keyword)to(
    given erased
    isKeyword: IsKeyword[Keyword, Value],
  )(
    given
      run: Run[Keyword, Domain[Value], Value]
  ): Domain[Value] = {
    run(keyword)
  }

  @inline def[Domain](keyword: Keyword)as(
    given erased
    isKeyword: IsKeyword[Keyword, Value],
  )(
    given
    run: Run[Keyword, Domain, Value]
  ): Domain = {
    run(keyword)
  }

  @inline def[MappedValue](
    upstream: Keyword
  )map(
    given erased IsKeyword[Keyword, Value]
  )(
    mapper: Value => MappedValue
  ) : FlatMap[Keyword, Value, Pure[MappedValue]] =
    FlatMap(upstream, Pure.cast.liftCo(mapper))

  @inline def[Keyword, Value, Mapped, MappedValue](
    upstream: Keyword
  )flatMap(
    given erased IsKeyword[Keyword, Value]
  )(
    flatMapper: Value => Mapped
  )(
    given erased IsKeyword[Mapped, MappedValue]
  ) : FlatMap[Keyword, Value, Mapped] =
    FlatMap(upstream, flatMapper)

}
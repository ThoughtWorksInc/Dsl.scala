package com.thoughtworks.dsl

import keywords._
import Dsl._
import scala.collection.WithFilter

object comprehension {
  extension [Keyword, Value](keyword: Keyword)
    @inline def to[Domain[_]](
        using /* erased */
        isKeyword: AsKeyword.IsKeyword[Keyword, Value]
    )(using
        run: Run[Keyword, Domain[Value], Value]
    ): Domain[Value] = {
      run(keyword)
    }

    @inline def as[Domain](
        using /* erased */
        isKeyword: AsKeyword.IsKeyword[Keyword, Value]
    )(using
        run: Run[Keyword, Domain, Value]
    ): Domain = {
      run(keyword)
    }
    @inline def map[MappedValue](
        using /*erased*/ AsKeyword.IsKeyword[Keyword, Value]
    )(
        mapper: Value => MappedValue
    ): FlatMap[Keyword, Value, Pure[MappedValue]] =
      FlatMap(keyword, Pure.apply.liftCo(mapper))

    @inline def flatMap[Mapped, MappedValue](
        using /*erased*/ AsKeyword.IsKeyword[Keyword, Value]
    )(
        flatMapper: Value => Mapped
    )(
        using /*erased*/ AsKeyword.IsKeyword[Mapped, MappedValue]
    ): FlatMap[Keyword, Value, Mapped] =
      FlatMap(keyword, flatMapper)

    @inline def withFilter[Mapped, MappedValue](
        using /*erased*/ AsKeyword.IsKeyword[Keyword, Value]
    )(
        filter: Value => Boolean
    ) =
      WithFilter(keyword, filter)
}

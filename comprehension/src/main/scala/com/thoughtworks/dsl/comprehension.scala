package com.thoughtworks.dsl

import keywords._
import Dsl._

extension [Keyword, Value](keyword: Keyword)
  @inline def to[Domain[_]](
      using /* erased */
      isKeyword: IsKeyword[Keyword, Value]
  )(using
      run: Run[Keyword, Domain[Value], Value]
  ): Domain[Value] = {
    run(keyword)
  }

  @inline def as[Domain](
      using /* erased */
      isKeyword: IsKeyword[Keyword, Value]
  )(using
      run: Run[Keyword, Domain, Value]
  ): Domain = {
    run(keyword)
  }
  @inline def map[MappedValue](
      using /*erased*/ IsKeyword[Keyword, Value]
  )(
      mapper: Value => MappedValue
  ): FlatMap[Keyword, Value, Pure[MappedValue]] =
    FlatMap(keyword, Pure.cast.liftCo(mapper))

  @inline def flatMap[Mapped, MappedValue](
      using /*erased*/ IsKeyword[Keyword, Value]
  )(
      flatMapper: Value => Mapped
  )(
      using /*erased*/ IsKeyword[Mapped, MappedValue]
  ): FlatMap[Keyword, Value, Mapped] =
    FlatMap(keyword, flatMapper)

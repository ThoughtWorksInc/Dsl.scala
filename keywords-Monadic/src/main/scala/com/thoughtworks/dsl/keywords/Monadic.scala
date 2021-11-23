package com.thoughtworks.dsl
package keywords
import Dsl.IsKeyword
import Dsl.Typed

opaque type Monadic[Functor[_], Value] <: Any = Functor[Value]
object Monadic {
  @inline def cast[Functor[_], Value]: Functor[Value] =:= Monadic[Functor, Value] = implicitly
  @inline def apply[Functor[_], Value](f: Functor[Value]): Monadic[Functor, Value] = f

  given [Functor[_], Value]: IsKeyword[Monadic[Functor, Value], Value] with {}
}

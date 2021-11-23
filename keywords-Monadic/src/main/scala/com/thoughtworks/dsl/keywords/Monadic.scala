package com.thoughtworks.dsl
package keywords
import Dsl.IsKeyword
import Dsl.Typed



opaque type Monadic[Functor[?], Value] <: Any = Functor[Value]
object Monadic {
  @inline def cast[Functor[?], Value]: Functor[Value] =:= Monadic[Functor, Value] = implicitly
  @inline def apply[Functor[?], Value](f: Functor[Value]): Monadic[Functor, Value] = f

  given[Functor[?], Value]: IsKeyword[Monadic[Functor, Value], Value]
}

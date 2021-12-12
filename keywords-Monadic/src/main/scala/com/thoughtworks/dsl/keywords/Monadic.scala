package com.thoughtworks.dsl
package keywords
import Dsl.AsKeyword
import Dsl.AsKeyword
import scala.language.higherKinds
import scala.language.implicitConversions

/** A keyword for extracting monadic value from the monadic expression [[fa]].
  *
  * @see
  *   [[com.thoughtworks.dsl.domains.cats]] for using this [[Monadic]] keyword with [[cats.Monad]].
  * @see
  *   [[com.thoughtworks.dsl.domains.scalaz]] for using this [[Monadic]] keyword with [[scalaz.Monad]].
  * @todo
  *   [[Monadic]] should be a [[scala.AnyVal]] after [[https://github.com/scala/bug/issues/10595]] is resolved.
  */
opaque type Monadic[Functor[_], Value] <: Dsl.OpaqueKeyword = Dsl.OpaqueKeyword.Of[Functor[Value]]

object Monadic {
  @inline def apply[Functor[_], Value]: Functor[Value] =:= Monadic[Functor, Value] = Dsl.OpaqueKeyword.Of.apply

  given [Functor[_], Value]: AsKeyword.IsKeyword[Monadic[Functor, Value], Value] with {}

  given implicitMonadic[Functor[_], Value]: AsKeyword[Functor[Value], Monadic[Functor, Value], Value] = Monadic(_)

}

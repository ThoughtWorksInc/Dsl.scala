package com.thoughtworks.dsl
package keywords

import com.thoughtworks.dsl.Dsl

import scala.language.implicitConversions

import Dsl.IsKeyword
import Dsl.Lift

/** A [[Dsl.Keyword]] to early return a lifted value from the enclosing
  * function.
  *
  * @author
  *   杨博 (Yang Bo)
  */
opaque type Return[+ReturnValue] <: Dsl.Keyword.Opaque =
  Dsl.Keyword.Opaque.Of[ReturnValue]
object Return {
  @inline def apply[ReturnValue]: ReturnValue =:= Return[ReturnValue] =
    Dsl.Keyword.Opaque.Of.apply

  given [ReturnValue]: IsKeyword[Return[ReturnValue], Nothing] with {}

  given [ReturnValue, Domain](using
      lift: Lift[ReturnValue, Domain]
  ): Dsl.Atomic[Return[ReturnValue], Domain, Nothing] = Dsl.Atomic {
    (keyword: Return[ReturnValue], handler: Nothing => Domain) =>
      lift(keyword)

  }

  given [ReturnValue, Domain >: ReturnValue]
      : Dsl.Atomic[Return[ReturnValue], Domain, Nothing] =
    Dsl.Atomic[Return[ReturnValue], Domain, Nothing] {
      (keyword: Return[ReturnValue], handler: Nothing => Domain) =>
        keyword
    }
}

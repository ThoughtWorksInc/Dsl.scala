package com.thoughtworks.dsl
package keywords

import com.thoughtworks.dsl.Dsl

import scala.language.implicitConversions

import Dsl.AsKeyword
import Dsl.Lift

/** A [[Dsl.Keyword]] to early return a lifted value from the enclosing function.
  *
  * @author
  *   杨博 (Yang Bo)
  */
opaque type Return[ReturnValue] <: Dsl.Keyword.Opaque = Dsl.Keyword.Opaque.Of[ReturnValue]
object Return {
  @inline def apply[ReturnValue]: ReturnValue =:= Return[ReturnValue] = Dsl.Keyword.Opaque.Of.apply

  given [ReturnValue]: AsKeyword.IsKeyword[Return[ReturnValue], Nothing] with {}

  given [ReturnValue, Domain](using
      lift: Lift[ReturnValue, Domain]
  ): Dsl[Return[ReturnValue], Domain, Nothing] with {
    def cpsApply(keyword: Return[ReturnValue], handler: Nothing => Domain): Domain = {
      lift(keyword)
    }
  }

  implicit def returnDsl[ReturnValue, Domain >: ReturnValue]: Dsl[Return[ReturnValue], Domain, Nothing] =
    new Dsl[Return[ReturnValue], Domain, Nothing] {
      def cpsApply(keyword: Return[ReturnValue], handler: Nothing => Domain): Domain = {
        keyword
      }
    }
}

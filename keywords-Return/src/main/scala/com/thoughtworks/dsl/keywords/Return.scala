package com.thoughtworks.dsl.keywords

import com.thoughtworks.dsl.Dsl

import scala.language.implicitConversions

import Dsl.IsKeyword
import Dsl.Lift
import Dsl.Typed

/** A [[Dsl.Keyword]] to early return a lifted value from the enclosing function.
  *
  * @author
  *   杨博 (Yang Bo)
  */
opaque type Return[ReturnValue] = ReturnValue
object Return {
  @inline def cast[ReturnValue]: ReturnValue <:< Return[ReturnValue] = implicitly

  @inline def apply[ReturnValue]: ReturnValue =:= Return[ReturnValue] = summon[ReturnValue =:= Return[ReturnValue]]
  given [ReturnValue]: IsKeyword[Return[ReturnValue], Nothing] with {}

  extension [ReturnValue](keyword: Return[ReturnValue]) def returnValue: ReturnValue = keyword

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
        keyword.returnValue
      }
    }
}

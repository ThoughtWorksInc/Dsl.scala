package com.thoughtworks.dsl
package keywords
import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.IsKeyword

/** @see
  *   [[Put]]
  * @author
  *   杨博 (Yang Bo)
  */
opaque type Get[S] <: Dsl.Keyword.Opaque = Dsl.Keyword.Opaque.Of[Unit]

object Get {
  given [S]: IsKeyword[Get[S], S] with {}
  def apply[S]: Get[S] = Dsl.Keyword.Opaque.Of(())

  implicit def getDsl[S0, S <: S0, A]: Dsl[Get[S0], S => A, S0] = new Dsl[Get[S0], S => A, S0] {
    def cpsApply(keyword: Get[S0], handler: S0 => S => A): S => A = { b =>
      handler(b)(b)
    }
  }

}

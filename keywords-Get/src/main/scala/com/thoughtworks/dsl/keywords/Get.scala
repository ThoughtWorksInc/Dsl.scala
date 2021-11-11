package com.thoughtworks.dsl.keywords
import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.Keyword

/** @see [[Put]]
  * @author 杨博 (Yang Bo)
  */
final case class Get[S]() extends Keyword[Get[S], S]

object Get {

  implicit def getDsl[S0, S <: S0, A] = new Dsl[Get[S0], S => A, S0] {
    def cpsApply(keyword: Get[S0], handler: S0 => S => A): S => A = { b =>
      handler(b)(b)
    }
  }

}

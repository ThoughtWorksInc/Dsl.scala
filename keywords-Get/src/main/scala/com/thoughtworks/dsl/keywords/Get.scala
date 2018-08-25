package com.thoughtworks.dsl.keywords
import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.Keyword

/**
  * @see [[Put]]
  * @author 杨博 (Yang Bo)
  */
final case class Get[A]() extends Keyword[Get[A], A]

object Get {

  implicit def getDsl[A, B <: A, C] = new Dsl[Get[A], B => C, A] {
    def cpsApply(keyword: Get[A], handler: A => B => C): B => C = { b =>
      handler(b)(b)
    }
  }

}

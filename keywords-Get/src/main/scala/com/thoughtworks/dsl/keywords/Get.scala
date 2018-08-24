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

//  implicit def getDsl[LeftDomain, Value, RightDomain]: Dsl[Get[Value], LeftDomain !! Value !! RightDomain, Value] =
//    new Dsl[Get[Value], LeftDomain !! Value !! RightDomain, Value] {
//      def cpsApply(keyword: Get[Value],
//                   handler: Value => !![!![LeftDomain, Value], RightDomain]): !![!![LeftDomain, Value], RightDomain] = {
//        (k0: RightDomain => !![LeftDomain, Value]) => (k1: Value => LeftDomain) =>
//
//          ???
//      }
//    }

//  implicit def getDsl[Domain, Value]: Dsl[Get[Value], Value !! Domain, Value] =
//    new Dsl[Get[Value], Value !! Domain, Value] {
//      def cpsApply(keyword: Get[Value], handler: Value => !![Value, Domain]): !![Value, Domain] = { (k: Domain => Value) =>
//        handler(k(???))(k)
////        handler { (k: Domain => Value) =>
////          ??? : Value
////        }(k)
//      }
//    }
//    new Dsl[Get[Value], Domain !! Value, Value] {
//      def cpsApply(keyword: Get[Value], handler: Value => (Domain !! Value)): Domain !! Value = { (k: Value => Domain) =>
//        ??? : Domain
//      }
//
//    }
}

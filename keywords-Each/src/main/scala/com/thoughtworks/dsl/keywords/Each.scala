package com.thoughtworks.dsl.keywords

import com.thoughtworks.dsl.bangnotation.{reset, unary_!}
import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.{!!, AsKeyword}

import scala.collection._
import scala.language.implicitConversions
import Shift.implicitShift

import scala.collection.mutable.Builder

/** Iterates though each element in [[elements]].
  * @author 杨博 (Yang Bo)
  *
  * @example [[Each]] keywords can be used to calculate cartesian product.
  *
  *          {{{
  *          import com.thoughtworks.dsl.bangnotation._
  *          def cartesianProduct = reset (List(!Each(Array(1, 2, 3)) * !Each(Vector(1, 10, 100, 1000))))
  *          cartesianProduct should be(List(1, 10, 100, 1000, 2, 20, 200, 2000, 3, 30, 300, 3000))
  *          }}}
  * @see [[comprehension]] if you want to use traditional `for` comprehension instead of !-notation.
  */
final case class Each[Element](elements: Traversable[Element])
object Each {
  given [Element]: AsKeyword.FromKeyword[Each[Element], Element] with {}
  private[Each] object Scala213 {

    @inline
    def flatMapBreakOut[Element, Domain, DomainElement](
        fa: Traversable[Element],
        f: Element => GenTraversableOnce[DomainElement]
    )(implicit factory: Factory[DomainElement, Domain]): Domain = {
      factory.fromSpecific(new View.FlatMap(fa, f))
    }

    @inline
    def newBuilder[A, C](implicit factory: Factory[A, C]): Builder[A, C] = {
      factory.newBuilder
    }

  }

  import Scala213._

  implicit def implicitEach[Element](elements: Traversable[Element]): Each[Element] = Each[Element](elements)

  implicit def eachDsl[Element, Domain, DomainElement](implicit
      thatIsTraversableOnce: (Element => Domain) => (Element => GenTraversableOnce[DomainElement]),
      factory: Factory[DomainElement, Domain]
  ): Dsl[Each[Element], Domain, Element] =
    new Dsl[Each[Element], Domain, Element] {
      def cpsApply(keyword: Each[Element], handler: Element => Domain): Domain = {
        flatMapBreakOut(keyword.elements, thatIsTraversableOnce(handler))
      }
    }

  private[dsl] def foreachDsl[Element]: Dsl[Each[Element], Unit, Element] =
    new Dsl[Each[Element], Unit, Element] {
      def cpsApply(keyword: Each[Element], handler: Element => Unit): Unit = {
        keyword.elements.foreach(handler)
      }
    }

  implicit def continuationEachDsl[Element, LeftDomain, RightDomain, DomainElement](implicit
      rightDomainIsTraversableOnce: (Element => LeftDomain !! RightDomain) => (
          Element => LeftDomain !! TraversableOnce[DomainElement]
      ),
      factory: Factory[DomainElement, RightDomain],
      shiftDsl: Dsl[Shift[LeftDomain, TraversableOnce[DomainElement]], LeftDomain, TraversableOnce[DomainElement]]
  ): Dsl[Each[Element], LeftDomain !! RightDomain, Element] = {
    new Dsl[Each[Element], LeftDomain !! RightDomain, Element] {
      def cpsApply(
          keyword: Each[Element],
          handler0: Element => LeftDomain !! RightDomain
      ): LeftDomain !! RightDomain = {
        val i = keyword.elements.iterator
        val builder = newBuilder[DomainElement, RightDomain]
        val handler = rightDomainIsTraversableOnce(handler0)
        @inline
        def loop(continue: RightDomain => LeftDomain): LeftDomain = reset {
          if (i.hasNext) {
            builder ++= !Shift(handler(i.next()))
            loop(continue)
          } else {
            continue(builder.result())
          }
        }
        loop
      }
    }
  }

}

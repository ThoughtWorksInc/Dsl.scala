package com.thoughtworks.dsl.keywords

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.{!!, Keyword}

import scala.collection.GenTraversableOnce
import scala.collection.generic.CanBuildFrom
import scala.language.implicitConversions
import Shift.implicitShift

/**
  * @author 杨博 (Yang Bo)
  */
final case class Each[Element](elements: Traversable[Element]) extends Keyword[Each[Element], Element]
object Each {

  implicit def implicitEach[Element](elements: Traversable[Element]): Each[Element] = Each[Element](elements)

  implicit def eachDsl[Element, Domain, DomainElement](
      implicit thatIsTraversableOnce: (Element => Domain) => (Element => GenTraversableOnce[DomainElement]),
      bf: CanBuildFrom[Nothing, DomainElement, Domain]
  ): Dsl[Each[Element], Domain, Element] =
    new Dsl[Each[Element], Domain, Element] {
      def cpsApply(keyword: Each[Element], handler: Element => Domain): Domain = {
        keyword.elements.flatMap(handler)(collection.breakOut(bf))
      }
    }

  @deprecated(message = "This is dangerous. Don't use it.", since = "1.0.0-RC5")
  implicit def foreachDsl[Element]: Dsl[Each[Element], Unit, Element] =
    new Dsl[Each[Element], Unit, Element] {
      def cpsApply(keyword: Each[Element], handler: Element => Unit): Unit = {
        keyword.elements.foreach(handler)
      }
    }

  implicit def continuationEachDsl[Element, LeftDomain, RightDomain, DomainElement](
      implicit rightDomainIsTraversableOnce: (Element => LeftDomain !! RightDomain) => (
          Element => LeftDomain !! TraversableOnce[DomainElement]),
      bf: CanBuildFrom[Nothing, DomainElement, RightDomain],
      shiftDsl: Dsl[Shift[LeftDomain, TraversableOnce[DomainElement]], LeftDomain, TraversableOnce[DomainElement]]
  ): Dsl[Each[Element], LeftDomain !! RightDomain, Element] = {
    new Dsl[Each[Element], LeftDomain !! RightDomain, Element] {
      def cpsApply(keyword: Each[Element],
                    handler0: Element => LeftDomain !! RightDomain): LeftDomain !! RightDomain = {
        val i = keyword.elements.toIterator
        val builder = bf()
        val handler = rightDomainIsTraversableOnce(handler0)
        @inline
        def loop(continue: RightDomain => LeftDomain): LeftDomain = {
          if (i.hasNext) {
            builder ++= !handler(i.next())
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

package com.thoughtworks.dsl.keywords

import com.thoughtworks.enableMembersIf
import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.{!!, Keyword}

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
  *          def cartesianProduct = List(!Each(Array(1, 2, 3)) * !Each(Vector(1, 10, 100, 1000)))
  *          cartesianProduct should be(List(1, 10, 100, 1000, 2, 20, 200, 2000, 3, 30, 300, 3000))
  *          }}}
  * @see [[comprehension]] if you want to use traditional `for` comprehension instead of !-notation.
  */
final case class Each[Element](elements: Traversable[Element]) extends Keyword[Each[Element], Element]
object Each {

  @enableMembersIf(scala.util.Properties.versionNumberString.matches("""^2\.1(1|2)\..*$"""))
  private[Each] object Scala211Or212 {
    type Factory[-A, +C] = scala.collection.generic.CanBuildFrom[Nothing, A, C]

    @inline
    def flatMapBreakOut[Element, Domain, DomainElement](
        fa: Traversable[Element],
        f: Element => GenTraversableOnce[DomainElement]
    )(implicit factory: Factory[DomainElement, Domain]): Domain = {
      fa.flatMap(f)(collection.breakOut(factory))
    }

    @inline
    def newBuilder[A, C](implicit factory: Factory[A, C]): Builder[A, C] = {
      factory()
    }

  }

  @enableMembersIf(scala.util.Properties.versionNumberString.matches("""^2\.13\..*$"""))
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

  import Scala211Or212._
  import Scala213._

  implicit def implicitEach[Element](elements: Traversable[Element]): Each[Element] = Each[Element](elements)

  implicit def eachDsl[Element, Domain, DomainElement](implicit
      thatIsTraversableOnce: (Element => Domain) => (Element => GenTraversableOnce[DomainElement]),
      factory: Factory[DomainElement, Domain]
  ): Dsl[Each[Element], Domain, Element] =
    new Dsl[Each[Element], Domain, Element] {
      def cpsApply(keyword: Each[Element], handler: Element => Domain): Domain = {
        flatMapBreakOut(keyword.elements, handler)
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
        val i = keyword.elements.toIterator
        val builder = newBuilder[DomainElement, RightDomain]
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

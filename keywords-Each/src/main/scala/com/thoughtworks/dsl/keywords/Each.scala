package com.thoughtworks.dsl.keywords

import com.thoughtworks.dsl.bangnotation.{reset, unary_!}
import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.{!!, AsKeyword}

import scala.collection._
import scala.language.implicitConversions

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
  given [Element]: AsKeyword.IsKeyword[Each[Element], Element] with {}

  @inline
  private def flatMapBreakOut[Element, Domain, DomainElement](
      fa: Traversable[Element],
      f: Element => GenTraversableOnce[DomainElement]
  )(implicit factory: Factory[DomainElement, Domain]): Domain = {
    factory.fromSpecific(new View.FlatMap(fa, f))
  }

  @inline
  private def newBuilder[A, C](implicit factory: Factory[A, C]): Builder[A, C] = {
    factory.newBuilder
  }

  given implicitEach[Element]: AsKeyword[Traversable[Element], Each[Element], Element] = Each(_)

  implicit def eachDsl[Element, Domain, DomainElement](implicit
      thatIsTraversableOnce: (Element => Domain) => (Element => GenTraversableOnce[DomainElement]),
      factory: Factory[DomainElement, Domain]
  ): Dsl[Each[Element], Domain, Element] =
    new Dsl[Each[Element], Domain, Element] {
      def cpsApply(keyword: Each[Element], handler: Element => Domain): Domain = {
        flatMapBreakOut(keyword.elements, thatIsTraversableOnce(handler))
      }
    }

  private def toLinearSeq[Element](
      i: IterableOnce[Element]
  ): LinearSeq[Element] = {
    i match {
      case linearSeq: LinearSeq[Element] =>
        linearSeq
      case notSeq =>
        LazyList.from(notSeq)
          }
        }
  given [
      Element,
      MappedKeyword,
      MappedValue <: IterableOps[
        MappedElement,
        _,
        _
      ],
      MappedElement,
      Domain
  ](using
      isKeyword: AsKeyword.IsKeyword[
        MappedKeyword,
        MappedValue
      ],
      factory: Factory[MappedElement, MappedValue],
      blockDsl: Dsl.PolyCont[MappedKeyword, Domain, MappedValue]
  ): Dsl.PolyCont[
    FlatMap[Each[Element], Element, MappedKeyword],
    Domain,
    MappedValue
  ] = { case (FlatMap(Each(sourceCollection), flatMapper), handler) =>
    @inline def loop(
        seqOps: LinearSeq[Element],
        viewHandler: View[MappedElement] => Domain
    ): Domain = {
      seqOps.headOption match {
        case Some(head) =>
          blockDsl.cpsApply(
            flatMapper(head),
            { mappedHead =>
              loop(
                seqOps.tail.asInstanceOf[LinearSeq[Element]],
                { mappedTail =>
                  viewHandler(View.Concat(mappedHead, mappedTail))
                }
              )
            }
          )
        case None =>
          viewHandler(Nil.view)
      }
    }
    loop(
      toLinearSeq(sourceCollection),
      { view =>
      handler(factory.fromSpecific(view))
      }
    )
  }

}

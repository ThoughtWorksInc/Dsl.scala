package com.thoughtworks.dsl.keywords

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.Keyword
import com.thoughtworks.dsl.keywords.Yield.From
import com.thoughtworks.enableMembersIf

import scala.collection._
import scala.collection.generic.CanBuildFrom
import scala.concurrent.Future
import scala.language.implicitConversions
import scala.language.higherKinds

/**
  * @author 杨博 (Yang Bo)
  * @example This `Yield` keyword must be put inside a function that returns `Seq[Element]` or `Seq[Element] !! ...`,
  *          or it will not compile.
  *
  *          {{{
  *          "def f(): Int = !Yield(1)" shouldNot compile
  *          }}}
  *
  */
final case class Yield[Element](element: Element) extends AnyVal with Keyword[Yield[Element], Unit]

private[keywords] trait LowPriorityYield1 {

  implicit def iteratorYieldFromDsl[A, B >: A, FromCollection[X] <: Iterator[X]]
    : Dsl[From[FromCollection, A], Iterator[B], Unit] =
    new Dsl[From[FromCollection, A], Iterator[B], Unit] {
      def cpsApply(keyword: From[FromCollection, A], generateTail: Unit => Iterator[B]): Iterator[B] = {
        keyword.elements ++ generateTail(())
      }
    }

  implicit def iteratorYieldDsl[A, B >: A]: Dsl[Yield[A], Iterator[B], Unit] =
    new Dsl[Yield[A], Iterator[B], Unit] {
      def cpsApply(keyword: Yield[A], generateTail: Unit => Iterator[B]): Iterator[B] = {
        Iterator.single(keyword.element) ++ generateTail(())
      }
    }

}

private[keywords] object YieldScalaVersions {

  @enableMembersIf(scala.util.Properties.versionNumberString.matches("""^2\.1(1|2)\..*$"""))
  object Scala211Or212 {

    trait LowPriorityYield0 extends LowPriorityYield1 {

      implicit def seqYieldFromDsl[A,
                                   B >: A,
                                   FromCollection[X] <: Iterable[X],
                                   Collection[X] <: SeqLike[X, Collection[X]]](
          implicit canBuildFrom: CanBuildFrom[Collection[B], B, Collection[B]])
        : Dsl[From[FromCollection, A], Collection[B], Unit] =
        new Dsl[From[FromCollection, A], Collection[B], Unit] {
          def cpsApply(keyword: From[FromCollection, A], generateTail: Unit => Collection[B]): Collection[B] = {
            keyword.elements ++: generateTail(())
          }
        }

      implicit def seqYieldDsl[A, B >: A, Collection[X] <: SeqLike[X, Collection[X]]](
          implicit canBuildFrom: CanBuildFrom[Collection[B], B, Collection[B]]): Dsl[Yield[A], Collection[B], Unit] =
        new Dsl[Yield[A], Collection[B], Unit] {
          def cpsApply(keyword: Yield[A], generateTail: Unit => Collection[B]): Collection[B] = {
            keyword.element +: generateTail(())
          }
        }

    }
  }

  @enableMembersIf(scala.util.Properties.versionNumberString.matches("""^2\.13\..*$"""))
  object Scala213 {
    trait LowPriorityYield0 extends LowPriorityYield1 {

      implicit def seqYieldFromDsl[A,
                                   B >: A,
                                   FromCollection[X] <: Iterable[X],
                                   Collection[X] <: Seq[X] with SeqOps[X, Collection, Collection[X]]]
        : Dsl[From[FromCollection, A], Collection[B], Unit] =
        new Dsl[From[FromCollection, A], Collection[B], Unit] {
          def cpsApply(keyword: From[FromCollection, A], generateTail: Unit => Collection[B]): Collection[B] = {
            keyword.elements ++: generateTail(())
          }
        }

      implicit def seqYieldDsl[A, B >: A, Collection[+X] <: Seq[X] with SeqOps[X, Collection, Collection[X]]]
        : Dsl[Yield[A], Collection[B], Unit] =
        new Dsl[Yield[A], Collection[B], Unit] {
          def cpsApply(keyword: Yield[A], generateTail: Unit => Collection[B]): Collection[B] = {
            keyword.element +: generateTail(())
          }
        }

    }
  }

}

import YieldScalaVersions.Scala211Or212._
import YieldScalaVersions.Scala213._

object Yield extends LowPriorityYield0 {

  final case class From[Collection[_], Element](elements: Collection[Element])
      extends AnyVal
      with Keyword[From[Collection, Element], Unit]

  def apply[Element](element0: Element, element1: Element, restElements: Element*) = {
    From(element0 +: element1 +: restElements)
  }

  implicit def implicitYieldFrom[FromCollection[_], Element](
      elements: FromCollection[Element]): From[FromCollection, Element] = From[FromCollection, Element](elements)

  implicit def streamYieldFromDsl[A, B >: A, FromCollection[X] <: Iterable[X]]
    : Dsl[From[FromCollection, A], Stream[B], Unit] =
    new Dsl[From[FromCollection, A], Stream[B], Unit] {
      def cpsApply(keyword: From[FromCollection, A], generateTail: Unit => Stream[B]): Stream[B] = {
        keyword.elements.toStream.append(generateTail(()))
      }
    }

  implicit def futureStreamYieldFromDsl[Element, That >: Element, FromCollection[X] <: Iterable[X]]
    : Dsl[From[FromCollection, Element], Stream[Future[That]], Unit] =
    new Dsl[From[FromCollection, Element], Stream[Future[That]], Unit] {
      def cpsApply(keyword: From[FromCollection, Element],
                   generateTail: Unit => Stream[Future[That]]): Stream[Future[That]] = {
        keyword.elements.toStream.map(Future.successful).append(generateTail(()))
      }
    }

  implicit def implicitYield[Element](element: Element): Yield[Element] = Yield[Element](element)

  implicit def streamYieldDsl[Element, That >: Element]: Dsl[Yield[Element], Stream[That], Unit] =
    new Dsl[Yield[Element], Stream[That], Unit] {
      def cpsApply(keyword: Yield[Element], generateTail: Unit => Stream[That]): Stream[That] = {
        new Stream.Cons(keyword.element, generateTail(()))
      }
    }

  implicit def futureStreamYieldDsl[Element, That >: Element]: Dsl[Yield[Element], Stream[Future[That]], Unit] =
    new Dsl[Yield[Element], Stream[Future[That]], Unit] {
      def cpsApply(keyword: Yield[Element], generateTail: Unit => Stream[Future[That]]): Stream[Future[That]] = {
        new Stream.Cons(Future.successful(keyword.element), generateTail(()))
      }
    }
}

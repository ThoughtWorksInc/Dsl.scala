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

private[keywords] trait LowPriorityYield2 {

  def apply[A](elements: A*) = {
    From(elements)
  }

  implicit def iteratorYieldFromDsl[A, B >: A, FromCollection <: TraversableOnce[A]]
    : Dsl[From[FromCollection], Iterator[B], Unit] =
    new Dsl[From[FromCollection], Iterator[B], Unit] {
      def cpsApply(keyword: From[FromCollection], generateTail: Unit => Iterator[B]): Iterator[B] = {
        keyword.elements.toIterator ++ generateTail(())
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

    trait LowPriorityYield1 extends LowPriorityYield2 {

      implicit def seqViewYieldFromDsl[A, B >: A, FromCollection <: Iterable[A], Coll1, Coll2](
          implicit canBuildFrom: CanBuildFrom[SeqView[B, Coll1], B, SeqView[B, Coll2]])
        : Dsl[From[FromCollection], SeqView[B, Coll1], Unit] =
        new Dsl[From[FromCollection], SeqView[B, Coll1], Unit] {
          def cpsApply(keyword: From[FromCollection], generateTail: Unit => SeqView[B, Coll1]): SeqView[B, Coll1] = {
            (keyword.elements ++: generateTail(()))(canBuildFrom).asInstanceOf[SeqView[B, Coll1]]
          }
        }

      implicit def seqViewYieldDsl[A, B >: A, Coll1, Coll2](
          implicit canBuildFrom: CanBuildFrom[scala.collection.SeqView[B, Coll1], B, SeqView[B, Coll2]])
        : Dsl[Yield[A], SeqView[B, Coll1], Unit] =
        new Dsl[Yield[A], SeqView[B, Coll1], Unit] {
          def cpsApply(keyword: Yield[A], generateTail: Unit => SeqView[B, Coll1]): SeqView[B, Coll1] = {
            (keyword.element +: generateTail(()))(canBuildFrom).asInstanceOf[SeqView[B, Coll1]]
          }
        }
    }

    trait LowPriorityYield0 extends LowPriorityYield1 {

      implicit def seqYieldFromDsl[A,
                                   B >: A,
                                   FromCollection <: Iterable[A],
                                   Collection[X] <: SeqLike[X, Collection[X]]](
          implicit canBuildFrom: CanBuildFrom[Collection[B], B, Collection[B]])
        : Dsl[From[FromCollection], Collection[B], Unit] =
        new Dsl[From[FromCollection], Collection[B], Unit] {
          def cpsApply(keyword: From[FromCollection], generateTail: Unit => Collection[B]): Collection[B] = {
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

    trait LowPriorityYield1 extends LowPriorityYield2 {

      implicit def viewYieldFromDsl[A, B >: A, FromCollection <: View.SomeIterableOps[A]]
        : Dsl[From[FromCollection], View[B], Unit] =
        new Dsl[From[FromCollection], View[B], Unit] {
          def cpsApply(keyword: From[FromCollection], generateTail: Unit => View[B]): View[B] = {
            new View.Concat(keyword.elements, generateTail(()))
          }
        }

      implicit def seqViewYieldDsl[A, B >: A]: Dsl[Yield[A], SeqView[B], Unit] =
        new Dsl[Yield[A], SeqView[B], Unit] {
          def cpsApply(keyword: Yield[A], generateTail: Unit => SeqView[B]): SeqView[B] = {
            generateTail(()).prepended(keyword.element)
          }
        }

      implicit def indexedSeqViewYieldDsl[A, B >: A]: Dsl[Yield[A], IndexedSeqView[B], Unit] =
        new Dsl[Yield[A], IndexedSeqView[B], Unit] {
          def cpsApply(keyword: Yield[A], generateTail: Unit => IndexedSeqView[B]): IndexedSeqView[B] = {
            generateTail(()).prepended(keyword.element)
          }
        }

      implicit def viewYieldDsl[A, B >: A]: Dsl[Yield[A], View[B], Unit] =
        new Dsl[Yield[A], View[B], Unit] {
          def cpsApply(keyword: Yield[A], generateTail: Unit => View[B]): View[B] = {
            new View.Concat[B](new View.Single(keyword.element), generateTail(()))
          }
        }
    }

    trait LowPriorityYield0 extends LowPriorityYield1 {
      implicit def seqYieldFromDsl[A,
                                   B >: A,
                                   FromCollection <: View.SomeIterableOps[A],
                                   Collection[X] <: SeqOps[X, Collection, Collection[X]]]
        : Dsl[From[FromCollection], Collection[B], Unit] =
        new Dsl[From[FromCollection], Collection[B], Unit] {
          def cpsApply(keyword: From[FromCollection], generateTail: Unit => Collection[B]): Collection[B] = {
            keyword.elements.toIterable ++: generateTail(())
          }
        }

      implicit def seqYieldDsl[A, B >: A, Collection[+X] <: SeqOps[X, Collection, Collection[X]]]
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

  final case class From[FromCollection <: TraversableOnce[_]](elements: FromCollection)
      extends AnyVal
      with Keyword[From[FromCollection], Unit]

  implicit def implicitYieldFrom[FromCollection <: TraversableOnce[_]](elements: FromCollection): From[FromCollection] =
    From(elements)

  implicit def streamYieldFromDsl[A, B >: A, FromCollection <: Iterable[A]]
    : Dsl[From[FromCollection], Stream[B], Unit] =
    new Dsl[From[FromCollection], Stream[B], Unit] {
      def cpsApply(keyword: From[FromCollection], generateTail: Unit => Stream[B]): Stream[B] = {
        keyword.elements.toStream.append(generateTail(()))
      }
    }

  implicit def futureStreamYieldFromDsl[A, B >: A, FromCollection <: Iterable[A]]
    : Dsl[From[FromCollection], Stream[Future[B]], Unit] =
    new Dsl[From[FromCollection], Stream[Future[B]], Unit] {
      def cpsApply(keyword: From[FromCollection], generateTail: Unit => Stream[Future[B]]): Stream[Future[B]] = {
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

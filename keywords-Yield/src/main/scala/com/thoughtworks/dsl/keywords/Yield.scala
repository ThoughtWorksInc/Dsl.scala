package com.thoughtworks.dsl.keywords

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.Keyword
import com.thoughtworks.dsl.keywords.Yield.From
import com.thoughtworks.enableIf
import com.thoughtworks.enableMembersIf

import scala.collection._
import scala.collection.generic.CanBuildFrom
import scala.concurrent.Future
import scala.language.implicitConversions
import scala.language.higherKinds

/** @author 杨博 (Yang Bo)
  * @example This `Yield` keyword must be put inside a function that returns `Seq[Element]` or `Seq[Element] !! ...`,
  *          or it will not compile.
  *
  *          {{{
  *          "def f(): Int = !Yield(1)" shouldNot compile
  *          }}}
  *
  * @example [[Yield]] keywords can be used together with other keywords.
  *          {{{
  *          def gccFlagBuilder(sourceFile: String, includes: String*): Stream[String] = {
  *            !Yield("gcc")
  *            !Yield("-c")
  *            !Yield(sourceFile)
  *            val include = !Each(includes)
  *            !Yield("-I")
  *            !Yield(include)
  *            !Continue
  *          }
  *
  *          gccFlagBuilder("main.c", "lib1/include", "lib2/include") should be(Stream("gcc", "-c", "main.c", "-I", "lib1/include", "-I", "lib2/include"))
  *          }}}
  * @see [[comprehension]] if you want to use traditional `for` comprehension instead of !-notation.
  */
final case class Yield[Element](element: Element) extends AnyVal with Keyword[Yield[Element], Unit]

private[keywords] trait LowPriorityYield3 {

  def apply[A](elements: A*) = {
    From(elements)
  }

  implicit def iteratorYieldFromDsl[A, FromCollection <: TraversableOnce[A]]
      : Dsl[From[FromCollection], Iterator[A], Unit] =
    new Dsl[From[FromCollection], Iterator[A], Unit] {
      def cpsApply(keyword: From[FromCollection], generateTail: Unit => Iterator[A]): Iterator[A] = {
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

    trait LowPriorityYield1 extends LowPriorityYield3 {

      @enableIf(scala.util.Properties.versionNumberString.matches("""^2\.11\..*$"""))
      implicit def seqViewYieldFromDsl[A, FromCollection <: Traversable[A], Coll1, Coll2](implicit
          canBuildFrom: CanBuildFrom[SeqView[A, Coll1], A, SeqView[A, Coll2]]
      ): Dsl[From[FromCollection], SeqView[A, Coll1], Unit] =
        new Dsl[From[FromCollection], SeqView[A, Coll1], Unit] {
          def cpsApply(keyword: From[FromCollection], generateTail: Unit => SeqView[A, Coll1]): SeqView[A, Coll1] = {
            (keyword.elements.toSeq.view ++: generateTail(()))(canBuildFrom).asInstanceOf[SeqView[A, Coll1]]
          }
        }

      @enableIf(scala.util.Properties.versionNumberString.matches("""^2\.12\..*$"""))
      implicit def seqViewYieldFromDsl[A, FromCollection <: Traversable[A], Coll1, Coll2](implicit
          canBuildFrom: CanBuildFrom[SeqView[A, Coll1], A, SeqView[A, Coll2]]
      ): Dsl[From[FromCollection], SeqView[A, Coll1], Unit] =
        new Dsl[From[FromCollection], SeqView[A, Coll1], Unit] {
          def cpsApply(keyword: From[FromCollection], generateTail: Unit => SeqView[A, Coll1]): SeqView[A, Coll1] = {
            (keyword.elements.toIterable ++: generateTail(()))(canBuildFrom).asInstanceOf[SeqView[A, Coll1]]
          }
        }

      implicit def seqViewYieldDsl[A, B >: A, Coll1, Coll2](implicit
          canBuildFrom: CanBuildFrom[scala.collection.SeqView[B, Coll1], B, SeqView[B, Coll2]]
      ): Dsl[Yield[A], SeqView[B, Coll1], Unit] =
        new Dsl[Yield[A], SeqView[B, Coll1], Unit] {
          def cpsApply(keyword: Yield[A], generateTail: Unit => SeqView[B, Coll1]): SeqView[B, Coll1] = {
            (keyword.element +: generateTail(()))(canBuildFrom).asInstanceOf[SeqView[B, Coll1]]
          }
        }
    }

    trait LowPriorityYield0 extends LowPriorityYield1 {

      implicit def seqYieldFromDsl[A, FromCollection <: Iterable[A], Collection[X] <: SeqLike[X, Collection[X]]](
          implicit canBuildFrom: CanBuildFrom[Collection[A], A, Collection[A]]
      ): Dsl[From[FromCollection], Collection[A], Unit] =
        new Dsl[From[FromCollection], Collection[A], Unit] {
          def cpsApply(keyword: From[FromCollection], generateTail: Unit => Collection[A]): Collection[A] = {
            keyword.elements ++: generateTail(())
          }
        }

      implicit def seqYieldDsl[A, B >: A, Collection[X] <: SeqLike[X, Collection[X]]](implicit
          canBuildFrom: CanBuildFrom[Collection[B], B, Collection[B]]
      ): Dsl[Yield[A], Collection[B], Unit] =
        new Dsl[Yield[A], Collection[B], Unit] {
          def cpsApply(keyword: Yield[A], generateTail: Unit => Collection[B]): Collection[B] = {
            keyword.element +: generateTail(())
          }
        }

    }
  }

  @enableMembersIf(scala.util.Properties.versionNumberString.matches("""^2\.13\..*$"""))
  object Scala213 {

    trait LowPriorityYield2 extends LowPriorityYield3 {

      implicit def viewYieldFromDsl[A, FromCollection <: View.SomeIterableOps[A]]
          : Dsl[From[FromCollection], View[A], Unit] =
        new Dsl[From[FromCollection], View[A], Unit] {
          def cpsApply(keyword: From[FromCollection], generateTail: Unit => View[A]): View[A] = {
            new View.Concat(keyword.elements, generateTail(()))
          }
        }

      implicit def indexedSeqViewYieldFromIterableDsl[A, FromCollection <: View.SomeIterableOps[A]]
          : Dsl[From[FromCollection], IndexedSeqView[A], Unit] =
        new Dsl[From[FromCollection], IndexedSeqView[A], Unit] {
          def cpsApply(keyword: From[FromCollection], generateTail: Unit => IndexedSeqView[A]): IndexedSeqView[A] = {
            new IndexedSeqView.Concat(collection.IndexedSeq.from(keyword.elements), generateTail(()))
          }
        }

      implicit def indexedSeqViewYieldFromDsl[A, FromCollection <: IndexedSeqOps[A, CC, C], CC[_], C]
          : Dsl[From[FromCollection], IndexedSeqView[A], Unit] =
        new Dsl[From[FromCollection], IndexedSeqView[A], Unit] {
          def cpsApply(keyword: From[FromCollection], generateTail: Unit => IndexedSeqView[A]): IndexedSeqView[A] = {
            new IndexedSeqView.Concat(keyword.elements, generateTail(()))
          }
        }

      implicit def seqViewYieldFromIterableDsl[A, FromCollection <: View.SomeIterableOps[A]]
          : Dsl[From[FromCollection], SeqView[A], Unit] =
        new Dsl[From[FromCollection], SeqView[A], Unit] {
          def cpsApply(keyword: From[FromCollection], generateTail: Unit => SeqView[A]): SeqView[A] = {
            new SeqView.Concat(collection.Seq.from(keyword.elements), generateTail(()))
          }
        }

      implicit def seqViewYieldFromDsl[A, FromCollection <: SeqOps[A, CC, C], CC[_], C]
          : Dsl[From[FromCollection], SeqView[A], Unit] =
        new Dsl[From[FromCollection], SeqView[A], Unit] {
          def cpsApply(keyword: From[FromCollection], generateTail: Unit => SeqView[A]): SeqView[A] = {
            new SeqView.Concat(keyword.elements, generateTail(()))
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

    trait LowPriorityYield1 extends LowPriorityYield2 {
      implicit def seqYieldFromDsl[A, FromCollection <: View.SomeIterableOps[A], Collection[X] <: SeqOps[
        X,
        Collection,
        Collection[X]
      ]]: Dsl[From[FromCollection], Collection[A], Unit] =
        new Dsl[From[FromCollection], Collection[A], Unit] {
          def cpsApply(keyword: From[FromCollection], generateTail: Unit => Collection[A]): Collection[A] = {
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

    trait LowPriorityYield0 extends LowPriorityYield1 {

      implicit def lazyListYieldFromDsl[A, FromCollection <: View.SomeIterableOps[A]]
          : Dsl[From[FromCollection], LazyList[A], Unit] =
        new Dsl[From[FromCollection], LazyList[A], Unit] {
          def cpsApply(keyword: From[FromCollection], generateTail: Unit => LazyList[A]): LazyList[A] = {
            keyword.elements.to(LazyList) #::: generateTail(())
          }
        }

      implicit def lazyListYieldDsl[Element, That >: Element]: Dsl[Yield[Element], LazyList[That], Unit] =
        new Dsl[Yield[Element], LazyList[That], Unit] {
          def cpsApply(keyword: Yield[Element], generateTail: Unit => LazyList[That]): LazyList[That] = {
            keyword.element #:: generateTail(())
          }
        }

      implicit def futureLazyListYieldDsl[Element, That >: Element]: Dsl[Yield[Element], LazyList[Future[That]], Unit] =
        new Dsl[Yield[Element], LazyList[Future[That]], Unit] {
          def cpsApply(
              keyword: Yield[Element],
              generateTail: Unit => LazyList[Future[That]]
          ): LazyList[Future[That]] = {
            Future.successful(keyword.element) #:: generateTail(())
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

  implicit def streamYieldFromDsl[A, FromCollection <: Iterable[A]]: Dsl[From[FromCollection], Stream[A], Unit] =
    new Dsl[From[FromCollection], Stream[A], Unit] {
      def cpsApply(keyword: From[FromCollection], generateTail: Unit => Stream[A]): Stream[A] = {
        keyword.elements.toStream.append(generateTail(()))
      }
    }

  implicit def futureStreamYieldFromDsl[A, FromCollection <: Iterable[A]]
      : Dsl[From[FromCollection], Stream[Future[A]], Unit] =
    new Dsl[From[FromCollection], Stream[Future[A]], Unit] {
      def cpsApply(keyword: From[FromCollection], generateTail: Unit => Stream[Future[A]]): Stream[Future[A]] = {
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

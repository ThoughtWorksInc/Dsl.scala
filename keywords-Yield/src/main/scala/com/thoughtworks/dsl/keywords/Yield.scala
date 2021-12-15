package com.thoughtworks.dsl
package keywords
import Dsl.AsKeyword
import scala.collection._
import scala.language.implicitConversions

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.keywords.Yield.From

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
  *          import com.thoughtworks.dsl.reset, reset._
  *          "def f(): Int = !Yield(1)" shouldNot compile
  *          }}}
  *
  * @example [[Yield]] keywords can be used together with other keywords.
  *          {{{
  *          import com.thoughtworks.dsl.reset, reset._
  *          def gccFlagBuilder(sourceFile: String, includes: String*) = reset[Stream[String]] {
  *            !Yield("gcc")
  *            !Yield("-c")
  *            !Yield(sourceFile)
  *            val include = !ToView.FromIterable(includes)
  *            !Yield("-I")
  *            !Yield(include)
  *            Stream.empty
  *          }
  *
  *          gccFlagBuilder("main.c", "lib1/include", "lib2/include") should be(Stream("gcc", "-c", "main.c", "-I", "lib1/include", "-I", "lib2/include"))
  *          }}}
  * @see [[comprehension]] if you want to use traditional `for` comprehension instead of !-notation.
  */
opaque type Yield[Element] <: Dsl.Keyword.Opaque = Dsl.Keyword.Opaque.Of[Element]

private[keywords] trait LowPriorityYield3 {

  def apply[A](elements: A*) = {
    From(elements)
  }

  implicit def iteratorYieldFromDsl[A, FromCollection <: TraversableOnce[A]]
      : Dsl[From[FromCollection], Iterator[A], Unit] =
    new Dsl[From[FromCollection], Iterator[A], Unit] {
      def cpsApply(keyword: From[FromCollection], generateTail: Unit => Iterator[A]): Iterator[A] = {
        From.apply.flip(keyword).toIterator ++ generateTail(())
      }
    }

  implicit def iteratorYieldDsl[A, B >: A]: Dsl[Yield[A], Iterator[B], Unit] =
    new Dsl[Yield[A], Iterator[B], Unit] {
      def cpsApply(keyword: Yield[A], generateTail: Unit => Iterator[B]): Iterator[B] = {
        Iterator.single(Yield.apply.flip(keyword)) ++ generateTail(())
      }
    }

}


private[keywords] trait LowPriorityYield1 extends LowPriorityYield3 {
  implicit def seqYieldFromDsl[A, FromCollection <: View.SomeIterableOps[A], Collection[X] <: SeqOps[
    X,
    Collection,
    Collection[X]
  ]]: Dsl[From[FromCollection], Collection[A], Unit] =
    new Dsl[From[FromCollection], Collection[A], Unit] {
      def cpsApply(keyword: From[FromCollection], generateTail: Unit => Collection[A]): Collection[A] = {
        From.apply.flip(keyword).toIterable ++: generateTail(())
      }
    }

  implicit def seqYieldDsl[A, B >: A, Collection[+X] <: SeqOps[X, Collection, Collection[X]]]
      : Dsl[Yield[A], Collection[B], Unit] =
    new Dsl[Yield[A], Collection[B], Unit] {
      def cpsApply(keyword: Yield[A], generateTail: Unit => Collection[B]): Collection[B] = {
        Yield.apply.flip(keyword) +: generateTail(())
      }
    }
}

private[keywords] trait LowPriorityYield0 extends LowPriorityYield1
object Yield extends LowPriorityYield0 {

  given [Element]: AsKeyword.IsKeyword[Yield[Element], Unit] with {}
  def apply[Element]: Element =:= Yield[Element] = Dsl.Keyword.Opaque.Of.apply
  def apply[A](element0: A, element1: A, elements: A*) = {
    From(element0 +: element1 +: elements)
  }

  opaque type From[FromCollection <: TraversableOnce[_]] <: Dsl.Keyword.Opaque = Dsl.Keyword.Opaque.Of[FromCollection]
  object From {
    given [FromCollection <: TraversableOnce[_]]: AsKeyword.IsKeyword[From[FromCollection], Unit] with {}

    def apply[FromCollection <: TraversableOnce[_]]: FromCollection =:= From[FromCollection] = Dsl.Keyword.Opaque.Of.apply

    extension [A, E](inline a: A)(using
        inline notKeyword: util.NotGiven[
          A <:< Dsl.Keyword
        ],
        inline isIterable: A <:< Iterable[E]
    )
      transparent inline def unary_! : Unit =
        !From[Iterable[E]](isIterable(a))
  }



  implicit def viewYieldFromDsl[A, FromCollection <: View.SomeIterableOps[A]]
      : Dsl[From[FromCollection], View[A], Unit] =
    new Dsl[From[FromCollection], View[A], Unit] {
      def cpsApply(keyword: From[FromCollection], generateTail: Unit => View[A]): View[A] = {
        new View.Concat(keyword, generateTail(()))
      }
    }

  implicit def indexedSeqViewYieldFromIterableDsl[A, FromCollection <: View.SomeIterableOps[A]]
      : Dsl[From[FromCollection], IndexedSeqView[A], Unit] =
    new Dsl[From[FromCollection], IndexedSeqView[A], Unit] {
      def cpsApply(keyword: From[FromCollection], generateTail: Unit => IndexedSeqView[A]): IndexedSeqView[A] = {
        new IndexedSeqView.Concat(collection.IndexedSeq.from(keyword), generateTail(()))
      }
    }

  implicit def indexedSeqViewYieldFromDsl[A, FromCollection <: IndexedSeqOps[A, CC, C], CC[_], C]
      : Dsl[From[FromCollection], IndexedSeqView[A], Unit] =
    new Dsl[From[FromCollection], IndexedSeqView[A], Unit] {
      def cpsApply(keyword: From[FromCollection], generateTail: Unit => IndexedSeqView[A]): IndexedSeqView[A] = {
        new IndexedSeqView.Concat(keyword, generateTail(()))
      }
    }

  implicit def seqViewYieldFromIterableDsl[A, FromCollection <: View.SomeIterableOps[A]]
      : Dsl[From[FromCollection], SeqView[A], Unit] =
    new Dsl[From[FromCollection], SeqView[A], Unit] {
      def cpsApply(keyword: From[FromCollection], generateTail: Unit => SeqView[A]): SeqView[A] = {
        new SeqView.Concat(collection.Seq.from(keyword), generateTail(()))
      }
    }

  implicit def seqViewYieldFromDsl[A, FromCollection <: SeqOps[A, CC, C], CC[_], C]
      : Dsl[From[FromCollection], SeqView[A], Unit] =
    new Dsl[From[FromCollection], SeqView[A], Unit] {
      def cpsApply(keyword: From[FromCollection], generateTail: Unit => SeqView[A]): SeqView[A] = {
        new SeqView.Concat(keyword, generateTail(()))
      }
    }

  implicit def seqViewYieldDsl[A, B >: A]: Dsl[Yield[A], SeqView[B], Unit] =
    new Dsl[Yield[A], SeqView[B], Unit] {
      def cpsApply(keyword: Yield[A], generateTail: Unit => SeqView[B]): SeqView[B] = {
        generateTail(()).prepended(keyword)
      }
    }

  implicit def indexedSeqViewYieldDsl[A, B >: A]: Dsl[Yield[A], IndexedSeqView[B], Unit] =
    new Dsl[Yield[A], IndexedSeqView[B], Unit] {
      def cpsApply(keyword: Yield[A], generateTail: Unit => IndexedSeqView[B]): IndexedSeqView[B] = {
        generateTail(()).prepended(keyword)
      }
    }

  implicit def viewYieldDsl[A, B >: A]: Dsl[Yield[A], View[B], Unit] =
    new Dsl[Yield[A], View[B], Unit] {
      def cpsApply(keyword: Yield[A], generateTail: Unit => View[B]): View[B] = {
        new View.Concat[B](new View.Single(keyword), generateTail(()))
      }
    }

  implicit def lazyListYieldFromDsl[A, FromCollection <: Iterable[A]]: Dsl[From[FromCollection], LazyList[A], Unit] =
    new Dsl[From[FromCollection], LazyList[A], Unit] {
      def cpsApply(keyword: From[FromCollection], generateTail: Unit => LazyList[A]): LazyList[A] = {
        keyword.to(LazyList) #::: generateTail(())
      }
    }

  implicit def lazyListYieldDsl[Element, That >: Element]: Dsl[Yield[Element], LazyList[That], Unit] =
    new Dsl[Yield[Element], LazyList[That], Unit] {
      def cpsApply(keyword: Yield[Element], generateTail: Unit => LazyList[That]): LazyList[That] = {
        keyword #:: generateTail(())
      }
    }


  implicit def streamYieldFromDsl[A, FromCollection <: Iterable[A]]: Dsl[From[FromCollection], Stream[A], Unit] =
    new Dsl[From[FromCollection], Stream[A], Unit] {
      def cpsApply(keyword: From[FromCollection], generateTail: Unit => Stream[A]): Stream[A] = {
        keyword.toStream #::: generateTail(())
      }
    }


  extension [A](inline a: A)(using
      inline notKeyword: util.NotGiven[
        A <:< Dsl.Keyword
      ]
  )
    transparent inline def unary_! : Unit =
      !Yield[A](a)

  given implicitYield[Element]: AsKeyword[Element, Yield[Element], Unit] = Yield(_)

  implicit def streamYieldDsl[Element, That >: Element]: Dsl[Yield[Element], Stream[That], Unit] =
    new Dsl[Yield[Element], Stream[That], Unit] {
      def cpsApply(keyword: Yield[Element], generateTail: Unit => Stream[That]): Stream[That] = {
        keyword #:: generateTail(())
      }
    }

}

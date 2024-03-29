package com.thoughtworks.dsl
package keywords
import Dsl.IsKeyword
import scala.collection._
import scala.language.implicitConversions

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.keywords.Yield.From

import scala.collection._
import scala.collection.generic.CanBuildFrom
import scala.concurrent.Future
import scala.language.implicitConversions
import scala.language.higherKinds

/** @author
  *   杨博 (Yang Bo)
  * @example
  *   This `Yield` keyword must be put inside a function that returns
  *   `Seq[Element]` or `Seq[Element] !! ...`, or it will not compile.
  *
  * {{{
  *           import com.thoughtworks.dsl.macros.Reset.Default.*
  *           "def f(): Int = !Yield(1)" shouldNot compile
  * }}}
  *
  * @example
  *   [[Yield]] keywords can be used together with other keywords.
  *   {{{
  *           import com.thoughtworks.dsl.macros.Reset.Default.*
  *           def gccFlagBuilder(sourceFile: String, includes: String*) = reset[Stream[String]] {
  *             !Yield("gcc")
  *             !Yield("-c")
  *             !Yield(sourceFile)
  *             val include = !Each(includes)
  *             !Yield("-I")
  *             !Yield(include)
  *             Stream.empty
  *           }
  *
  *           gccFlagBuilder("main.c", "lib1/include", "lib2/include") should be(Stream("gcc", "-c", "main.c", "-I", "lib1/include", "-I", "lib2/include"))
  *   }}}
  * @see
  *   [[comprehension]] if you want to use traditional `for` comprehension
  *   instead of !-notation.
  */
opaque type Yield[+Element] <: Dsl.Keyword.Opaque =
  Dsl.Keyword.Opaque.Of[Element]
def Yield[Element](using
    dummyImplicit: DummyImplicit = DummyImplicit.dummyImplicit
): Element =:= Yield[Element] = Dsl.Keyword.Opaque.Of

private[keywords] trait LowPriorityYield3 {

  def apply[A](elements: A*) = {
    From(elements)
  }

  given [A, FromCollection <: TraversableOnce[A]]
      : Dsl.Original[From[FromCollection], Iterator[A], Unit] =
    Dsl.Original[From[FromCollection], Iterator[A], Unit] {
      (keyword: From[FromCollection], generateTail: Unit => Iterator[A]) =>
        From.flip(keyword).toIterator ++ generateTail(())
    }

  given [A, B >: A]: Dsl.Original[Yield[A], Iterator[B], Unit] =
    Dsl.Original[Yield[A], Iterator[B], Unit] {
      (keyword: Yield[A], generateTail: Unit => Iterator[B]) =>
        Iterator.single(Yield.flip(keyword)) ++ generateTail(())
    }
}

private[keywords] trait LowPriorityYield1 extends LowPriorityYield3 {
  given [A, FromCollection <: View.SomeIterableOps[A], Collection[X] <: SeqOps[
    X,
    Collection,
    Collection[X]
  ]]: Dsl.Original[From[FromCollection], Collection[A], Unit] =
    Dsl.Original[From[FromCollection], Collection[A], Unit] {
      (keyword: From[FromCollection], generateTail: Unit => Collection[A]) =>
        From.flip(keyword).toIterable ++: generateTail(())
    }

  given [A, B >: A, Collection[+X] <: SeqOps[X, Collection, Collection[X]]]
      : Dsl.Original[Yield[A], Collection[B], Unit] =
    Dsl.Original[Yield[A], Collection[B], Unit] {
      (keyword: Yield[A], generateTail: Unit => Collection[B]) =>
        Yield.flip(keyword) +: generateTail(())
    }

}

private[keywords] trait LowPriorityYield0 extends LowPriorityYield1
object Yield extends LowPriorityYield0 {

  given [Element]: IsKeyword[Yield[Element], Unit] with {}
  def apply[A](element0: A, element1: A, elements: A*) = {
    From(element0 +: element1 +: elements)
  }

  opaque type From[FromCollection <: TraversableOnce[_]] <: Dsl.Keyword.Opaque =
    Dsl.Keyword.Opaque.Of[FromCollection]

  private[Yield] trait LowPriorityFrom0 { this: From.type =>

    given [A, FromSomeIterableOps <: View.SomeIterableOps[A]]
        : Dsl.Original[From[FromSomeIterableOps], SeqView[A], Unit] =
      Dsl.Original[From[FromSomeIterableOps], SeqView[A], Unit] {
        (
            keyword: From[FromSomeIterableOps],
            generateTail: Unit => SeqView[A]
        ) =>
          new SeqView.Concat(collection.Seq.from(keyword), generateTail(()))
      }

    given [A, FromSomeIterableOps <: View.SomeIterableOps[A]]
        : Dsl.Original[From[FromSomeIterableOps], IndexedSeqView[A], Unit] =
      Dsl.Original[From[FromSomeIterableOps], IndexedSeqView[A], Unit] {
        (
            keyword: From[FromSomeIterableOps],
            generateTail: Unit => IndexedSeqView[A]
        ) =>
          new IndexedSeqView.Concat(
            collection.IndexedSeq.from(keyword),
            generateTail(())
          )
      }
  }
  def From[FromCollection <: TraversableOnce[_]](using
      dummyImplicit: DummyImplicit = DummyImplicit.dummyImplicit
  ): FromCollection =:= From[FromCollection] = Dsl.Keyword.Opaque.Of
  object From extends LowPriorityFrom0 {
    given [FromCollection <: TraversableOnce[_]]
        : IsKeyword[From[FromCollection], Unit] with {}

    extension [A, E](inline a: A)(using
        inline notKeyword: util.NotGiven[
          A <:< Dsl.Keyword
        ],
        inline isIterable: A <:< Iterable[E]
    )
      transparent inline def unary_! : Unit =
        !From[Iterable[E]](isIterable(a))

    given [A, FromCollection <: View.SomeIterableOps[A]]
        : Dsl.Original[From[FromCollection], View[A], Unit] =
      Dsl.Original[From[FromCollection], View[A], Unit] {
        (keyword: From[FromCollection], generateTail: Unit => View[A]) =>
          new View.Concat(keyword, generateTail(()))
      }

    given indexedSeqViewYieldFromDsl[A, FromIndexedSeqOps <: IndexedSeqOps[
      A,
      CC,
      C
    ], CC[_], C]
        : Dsl.Original[From[FromIndexedSeqOps], IndexedSeqView[A], Unit] =
      Dsl.Original[From[FromIndexedSeqOps], IndexedSeqView[A], Unit] {
        (
            keyword: From[FromIndexedSeqOps],
            generateTail: Unit => IndexedSeqView[A]
        ) =>
          new IndexedSeqView.Concat(keyword, generateTail(()))
      }

    given seqViewYieldFromDsl[A, FromCollection <: SeqOps[A, CC, C], CC[_], C]
        : Dsl.Original[From[FromCollection], SeqView[A], Unit] =
      Dsl.Original[From[FromCollection], SeqView[A], Unit] {
        (keyword: From[FromCollection], generateTail: Unit => SeqView[A]) =>
          new SeqView.Concat(keyword, generateTail(()))
      }

    given [A, FromIterable <: Iterable[A]]
        : Dsl.Original[From[FromIterable], LazyList[A], Unit] =
      Dsl.Original[From[FromIterable], LazyList[A], Unit] {
        (keyword: From[FromIterable], generateTail: Unit => LazyList[A]) =>
          keyword.to(LazyList) #::: generateTail(())
      }

    given [A, FromIterable <: Iterable[A]]
        : Dsl.Original[From[FromIterable], Stream[A], Unit] =
      Dsl.Original[From[FromIterable], Stream[A], Unit] {
        (keyword: From[FromIterable], generateTail: Unit => Stream[A]) =>
          keyword.toStream #::: generateTail(())
      }

  }

  given [A, B >: A]: Dsl.Original[Yield[A], SeqView[B], Unit] =
    Dsl.Original[Yield[A], SeqView[B], Unit] {
      (keyword: Yield[A], generateTail: Unit => SeqView[B]) =>
        generateTail(()).prepended(keyword)
    }

  given [A, B >: A]: Dsl.Original[Yield[A], IndexedSeqView[B], Unit] =
    Dsl.Original[Yield[A], IndexedSeqView[B], Unit] {
      (keyword: Yield[A], generateTail: Unit => IndexedSeqView[B]) =>
        generateTail(()).prepended(keyword)
    }

  given [A, B >: A]: Dsl.Original[Yield[A], View[B], Unit] =
    Dsl.Original[Yield[A], View[B], Unit] {
      (keyword: Yield[A], generateTail: Unit => View[B]) =>
        new View.Concat[B](new View.Single(keyword), generateTail(()))
    }

  given [Element, That >: Element]
      : Dsl.Original[Yield[Element], LazyList[That], Unit] =
    Dsl.Original[Yield[Element], LazyList[That], Unit] {
      (keyword: Yield[Element], generateTail: Unit => LazyList[That]) =>
        keyword #:: generateTail(())
    }

  extension [A](inline a: A)(using
      inline notKeyword: util.NotGiven[
        A <:< Dsl.Keyword
      ]
  )
    transparent inline def unary_! : Unit =
      !Yield[A](a)

  implicit def streamYieldDsl[Element, That >: Element]
      : Dsl.Original[Yield[Element], Stream[That], Unit] =
    Dsl.Original[Yield[Element], Stream[That], Unit] {
      (keyword: Yield[Element], generateTail: Unit => Stream[That]) =>
        keyword #:: generateTail(())
    }
}

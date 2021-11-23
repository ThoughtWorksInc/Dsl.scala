package com.thoughtworks.dsl
package keywords
import Dsl.IsKeyword
import Dsl.Typed
import scala.collection._
import scala.language.implicitConversions

/** @author
  *   杨博 (Yang Bo)
  * @example
  *   This `Yield` keyword must be put inside a function that returns `Seq[Element]` or `Seq[Element] !! ...`, or it
  *   will not compile.
  *
  * {{{
  *           "def f(): Int = !Yield(1)" shouldNot compile
  * }}}
  *
  * @example
  *   [[Yield]] keywords can be used together with other keywords.
  * {{{
  *           def gccFlagBuilder(sourceFile: String, includes: String*): Stream[String] = {
  *             !Yield("gcc")
  *             !Yield("-c")
  *             !Yield(sourceFile)
  *             val include = !Each(includes)
  *             !Yield("-I")
  *             !Yield(include)
  *             !Continue
  *           }
  *
  *           gccFlagBuilder("main.c", "lib1/include", "lib2/include") should be(Stream("gcc", "-c", "main.c", "-I", "lib1/include", "-I", "lib2/include"))
  * }}}
  * @see
  *   [[comprehension]] if you want to use traditional `for` comprehension instead of !-notation.
  */
opaque type Yield[Element] = Element
object Yield {
  given [Element]: IsKeyword[Yield[Element], Unit] with {}
  def cast[Element]: Element <:< Yield[Element] = implicitly

  def apply[Element](element: Element): Yield[Element] = cast(element)

  def apply[A](element0: A, element1: A, elements: A*) = {
    From(element0 +: element1 +: elements)
  }

  opaque type From[FromCollection <: TraversableOnce[_]] = FromCollection
  object From {
    given [FromCollection <: TraversableOnce[_]]: IsKeyword[From[FromCollection], Unit] with {}

    def apply[FromCollection <: TraversableOnce[_]](elements: FromCollection): From[FromCollection] = elements
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

  implicit def streamYieldDsl[Element, That >: Element]: Dsl[Yield[Element], Stream[That], Unit] =
    new Dsl[Yield[Element], Stream[That], Unit] {
      def cpsApply(keyword: Yield[Element], generateTail: Unit => Stream[That]): Stream[That] = {
        keyword #:: generateTail(())
      }
    }
}

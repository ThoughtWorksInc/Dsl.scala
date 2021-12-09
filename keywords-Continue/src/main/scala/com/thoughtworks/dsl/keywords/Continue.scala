package com.thoughtworks.dsl.keywords

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.{!!, AsKeyword}

import scala.language.implicitConversions
import scala.language.higherKinds
import scala.util.control.TailCalls
import scala.util.control.TailCalls.TailRec
import scala.collection._

/** The base type of [[Continue$ Continue]] keyword.
  *
  * @see The [[Continue$ Continue]] object, which is the only instance of this [[Continue]] class.
  */
sealed class Continue

/** A keyword to skip the current iteration in a collection comprehension block.
  *
  * @note This [[Continue$ Continue]] keyword is usually used with [[Each]], to skip an element in the loop.
  * @see [[Each]] for creating collection comprehensions.
  * @example [[Each]] and [[Continue$ Continue]] can be used to calculate composite numbers and prime numbers.
  *
  *          {{{
  *          import com.thoughtworks.dsl.bangnotation._
  *          def compositeNumbersBelow(maxNumber: Int) = *[collection.immutable.HashSet] {
  *            val factor = !Each(2 until math.ceil(math.sqrt(maxNumber)).toInt)
  *            !Each(2 * factor until maxNumber by factor)
  *          }
  *
  *          compositeNumbersBelow(13) should be(Set(4, 6, 8, 9, 10, 12))
  *
  *          def primeNumbersBelow(maxNumber: Int) = *[Seq] {
  *            val compositeNumbers = compositeNumbersBelow(maxNumber)
  *            val i = !Each(2 until maxNumber)
  *            if (compositeNumbers(i)) !Continue
  *            i
  *          }
  *
  *          primeNumbersBelow(13) should be(Array(2, 3, 5, 7, 11))
  *          }}}
  * @author 杨博 (Yang Bo)
  */
case object Continue extends Continue {
  given AsKeyword.IsKeyword[Continue, Nothing] with {}
  given AsKeyword.IsKeyword[Continue.type, Nothing] with {}

  implicit def continueUnitDsl[Value]: Dsl[Continue, Unit, Value] = new Dsl[Continue, Unit, Value] {
    def cpsApply(keyword: Continue, handler: Value => Unit): Unit = ()
  }

  @inline
  private def empty[A, C](implicit factory: Factory[A, C]): C = {
    factory.newBuilder.result()
  }

  implicit def collectionContinueDsl[Value, Element, Collection[_]](implicit
      factory: Factory[Element, Collection[Element]]
  ): Dsl[Continue, Collection[Element], Value] =
    new Dsl[Continue, Collection[Element], Value] {
      def cpsApply(keyword: Continue, handler: Value => Collection[Element]): Collection[Element] = {
        empty[Element, Collection[Element]]
      }
    }

  implicit def OptionContinueDsl[Value, Element](implicit
      factory: Factory[Element, Option[Element]]
  ): Dsl[Continue, Option[Element], Value] =
    new Dsl[Continue, Option[Element], Value] {
      def cpsApply(keyword: Continue, handler: Value => Option[Element]): Option[Element] = {
        None
      }
    }

}

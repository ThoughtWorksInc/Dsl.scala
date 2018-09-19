package com.thoughtworks.dsl.keywords

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.{!!, Keyword}
import com.thoughtworks.enableMembersIf

import scala.language.implicitConversions
import scala.language.higherKinds
import scala.util.control.TailCalls
import scala.util.control.TailCalls.TailRec
import scala.collection._

/** The base type of [[Abort$ Abort]] keyword.
  *
  * @see The [[Abort$ Abort]] object, which is the only instance of this [[Abort]] class.
  */
sealed class Abort

/** A keyword to skip the current iteration in a collection comprehension block.
  *
  * @note This [[Abort$ Abort]] keyword is usually used with [[Each]], to skip an element in the loop.
  * @see [[Each]] for creating collection comprehensions.
  * @example [[Each]] and [[Abort$ Abort]] can be used to calculate composite numbers and prime numbers.
  *
  *          {{{
  *          def compositeNumbersBelow(maxNumber: Int) = collection.immutable.HashSet {
  *            val factor = !Each(2 until math.ceil(math.sqrt(maxNumber)).toInt)
  *            !Each(2 * factor until maxNumber by factor)
  *          }
  *
  *          compositeNumbersBelow(13) should be(Set(4, 6, 8, 9, 10, 12))
  *
  *          def primeNumbersBelow(maxNumber: Int) = Seq {
  *            val compositeNumbers = compositeNumbersBelow(maxNumber)
  *            val i = !Each(2 until maxNumber)
  *            if (compositeNumbers(i)) !Abort
  *            i
  *          }
  *
  *          primeNumbersBelow(13) should be(Array(2, 3, 5, 7, 11))
  *          }}}
  * @author 杨博 (Yang Bo)
  */
case object Abort extends Abort with Keyword[Abort, Nothing] {

  implicit def continueUnitDsl[Value]: Dsl[Abort, Unit, Value] = new Dsl[Abort, Unit, Value] {
    def cpsApply(keyword: Abort, handler: Value => Unit): Unit = ()
  }

  @enableMembersIf(scala.util.Properties.versionNumberString.matches("""^2\.1(1|2)\..*$"""))
  private[dsl] object Scala211Or212 {
    type Factory[-A, +C] = scala.collection.generic.CanBuildFrom[Nothing, A, C]

    @inline
    def empty[A, C](implicit factory: Factory[A, C]): C = {
      factory().result()
    }

  }

  @enableMembersIf(scala.util.Properties.versionNumberString.matches("""^2\.13\..*$"""))
  private[dsl] object Scala213 {

    @inline
    def empty[A, C](implicit factory: Factory[A, C]): C = {
      factory.newBuilder.result()
    }

  }

  import Scala211Or212._
  import Scala213._

  implicit def collectionAbortDsl[Value, Element, Collection[_]](
      implicit factory: Factory[Element, Collection[Element]]): Dsl[Abort, Collection[Element], Value] =
    new Dsl[Abort, Collection[Element], Value] {
      def cpsApply(keyword: Abort, handler: Value => Collection[Element]): Collection[Element] = {
        empty[Element, Collection[Element]]
      }
    }

}

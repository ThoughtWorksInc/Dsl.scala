package com.thoughtworks.dsl.keywords

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.{!!, Keyword}
import com.thoughtworks.enableMembersIf

import scala.language.implicitConversions
import scala.language.higherKinds
import scala.util.control.TailCalls
import scala.util.control.TailCalls.TailRec
import scala.collection._

sealed class Continue extends Keyword[Continue, Nothing]

/**
  *
  * @author 杨博 (Yang Bo)
  */
case object Continue extends Continue {

  implicit def continueUnitDsl[Value]: Dsl[Continue, Unit, Value] = new Dsl[Continue, Unit, Value] {
    def cpsApply(keyword: Continue, handler: Value => Unit): Unit = ()
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

  implicit def continueFactoryDsl[Value, Element, Collection[_]](
      implicit factory: Factory[Element, Collection[Element]]): Dsl[Continue, Collection[Element], Value] =
    new Dsl[Continue, Collection[Element], Value] {
      def cpsApply(keyword: Continue, handler: Value => Collection[Element]): Collection[Element] = {
        empty[Element, Collection[Element]]
      }
    }

}

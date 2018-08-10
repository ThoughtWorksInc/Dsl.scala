package com.thoughtworks.dsl.keywords

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.{!!, Keyword}

import scala.language.implicitConversions
import scala.util.control.TailCalls
import scala.util.control.TailCalls.TailRec

sealed class Break extends Keyword[Break, Nothing]

/**
  * @author 杨博 (Yang Bo)
  */
case object Break extends Break {

  implicit def breakUnitDsl[Value]: Dsl[Break, Unit, Value] = new Dsl[Break, Unit, Value] {
    def cpsApply(keyword: Break, handler: Value => Unit): Unit = ()
  }

  implicit def breakStreamDsl[Value, Element]: Dsl[Break, Stream[Element], Value] =
    new Dsl[Break, Stream[Element], Value] {
      def cpsApply(keyword: Break, handler: Value => Stream[Element]): Stream[Element] = Stream.empty
    }

}

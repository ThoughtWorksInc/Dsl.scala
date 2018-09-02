package com.thoughtworks.dsl.keywords

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.{!!, Keyword}

import scala.language.implicitConversions
import scala.util.control.TailCalls
import scala.util.control.TailCalls.TailRec

sealed class Continue extends Keyword[Continue, Nothing]

/**
  *
  * @author 杨博 (Yang Bo)
  */
case object Continue extends Continue {

  implicit def breakUnitDsl[Value]: Dsl[Continue, Unit, Value] = new Dsl[Continue, Unit, Value] {
    def cpsApply(keyword: Continue, handler: Value => Unit): Unit = ()
  }

  implicit def breakStreamDsl[Value, Element]: Dsl[Continue, Stream[Element], Value] =
    new Dsl[Continue, Stream[Element], Value] {
      def cpsApply(keyword: Continue, handler: Value => Stream[Element]): Stream[Element] = Stream.empty
    }

}

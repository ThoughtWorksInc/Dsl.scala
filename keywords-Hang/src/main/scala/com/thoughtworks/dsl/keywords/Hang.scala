package com.thoughtworks.dsl.keywords

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.{!!, Keyword}

import scala.language.implicitConversions
import scala.util.control.TailCalls
import scala.util.control.TailCalls.TailRec

sealed class Hang extends Keyword[Hang, Nothing]

/**
  * @author 杨博 (Yang Bo)
  */
case object Hang extends Hang {

  implicit def hangUnitDsl[Value]: Dsl[Hang, Unit, Value] = new Dsl[Hang, Unit, Value] {
    def cpsApply(keyword: Hang, handler: Value => Unit): Unit = ()
  }

  implicit def hangStreamDsl[Value, Element]: Dsl[Hang, Stream[Element], Value] =
    new Dsl[Hang, Stream[Element], Value] {
      def cpsApply(keyword: Hang, handler: Value => Stream[Element]): Stream[Element] = Stream.empty
    }

}

package com.thoughtworks.dsl.keywords

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.Keyword

import scala.language.implicitConversions
import scala.util.control.TailCalls
import scala.util.control.TailCalls.TailRec

/**
  * @author 杨博 (Yang Bo)
  */
sealed case class Hang[Value]() extends Keyword[Hang[Value], Value]

object Hang {

  implicit def hangUnitDsl[Value]: Dsl[Hang[Value], Unit, Value] = new Dsl[Hang[Value], Unit, Value] {
    def interpret(keyword: Hang[Value], handler: Value => Unit): Unit = ()
  }

  implicit def hangStreamDsl[Value, Element]: Dsl[Hang[Value], Stream[Element], Value] =
    new Dsl[Hang[Value], Stream[Element], Value] {
      def interpret(keyword: Hang[Value], handler: Value => Stream[Element]): Stream[Element] = Stream.empty
    }

}

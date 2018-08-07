package com.thoughtworks.dsl.keywords

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.{!!, Keyword}

import scala.language.implicitConversions
import scala.util.control.TailCalls
import scala.util.control.TailCalls.TailRec

/**
  * @author 杨博 (Yang Bo)
  */
final case class Return[ReturnValue, ExpressionValue](returnValue: ReturnValue)
    extends Keyword[Return[ReturnValue, ExpressionValue], ExpressionValue]

private[keywords] sealed trait LowPriorityReturn1 {
  implicit def returnDsl[ReturnValue, ExpressionValue]
    : Dsl[Return[ReturnValue, ExpressionValue], ReturnValue, ExpressionValue] =
    new Dsl[Return[ReturnValue, ExpressionValue], ReturnValue, ExpressionValue] {
      def cpsApply(keyword: Return[ReturnValue, ExpressionValue],
                   handler: ExpressionValue => ReturnValue): ReturnValue = {
        keyword.returnValue
      }
    }
}

private[keywords] sealed trait LowPriorityReturn0 extends LowPriorityReturn1 {

  implicit def returnContinuationDsl[ReturnValue, ExpressionValue, LeftDomain, RightDomain](
      implicit restReturnDsl: Dsl[Return[ReturnValue, RightDomain], RightDomain, RightDomain])
    : Dsl[Return[ReturnValue, ExpressionValue], LeftDomain !! RightDomain, ExpressionValue] =
    new Dsl[Return[ReturnValue, ExpressionValue], LeftDomain !! RightDomain, ExpressionValue] {
      def cpsApply(keyword: Return[ReturnValue, ExpressionValue],
                   handler: ExpressionValue => LeftDomain !! RightDomain): LeftDomain !! RightDomain =
        _(restReturnDsl.cpsApply(Return(keyword.returnValue), identity))
    }
}

object Return extends LowPriorityReturn0 {

  implicit def returnStreamDsl[ReturnValue, ExpressionValue, Domain](
      implicit restReturnDsl: Dsl[Return[ReturnValue, Domain], Domain, Domain])
    : Dsl[Return[ReturnValue, ExpressionValue], Stream[Domain], ExpressionValue] =
    new Dsl[Return[ReturnValue, ExpressionValue], Stream[Domain], ExpressionValue] {
      def cpsApply(keyword: Return[ReturnValue, ExpressionValue],
                   handler: ExpressionValue => Stream[Domain]): Stream[Domain] = {
        restReturnDsl.cpsApply(Return(keyword.returnValue), identity) #:: Stream.empty[Domain]
      }
    }

}

package com.thoughtworks.dsl.keywords

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.{!!, Keyword}

import scala.language.implicitConversions

/**
  * @author 杨博 (Yang Bo)
  */
final case class Return[ReturnValue](returnValue: ReturnValue) extends Keyword[Return[ReturnValue], Nothing]

private[keywords] sealed trait LowPriorityReturn1 {

  implicit def returnDsl[ReturnValue]: Dsl[Return[ReturnValue], ReturnValue, Nothing] =
    new Dsl[Return[ReturnValue], ReturnValue, Nothing] {
      def cpsApply(keyword: Return[ReturnValue], handler: Nothing => ReturnValue): ReturnValue = {
        keyword.returnValue
      }
    }
}

private[keywords] sealed trait LowPriorityReturn0 extends LowPriorityReturn1 {

  implicit def returnContinuationDsl[ReturnValue, LeftDomain, RightDomain](
      implicit restReturnDsl: Dsl[Return[ReturnValue], RightDomain, RightDomain])
    : Dsl[Return[ReturnValue], LeftDomain !! RightDomain, Nothing] =
    new Dsl[Return[ReturnValue], LeftDomain !! RightDomain, Nothing] {
      def cpsApply(keyword: Return[ReturnValue],
                   handler: Nothing => LeftDomain !! RightDomain): LeftDomain !! RightDomain =
        _(restReturnDsl.cpsApply(Return(keyword.returnValue), identity))
    }
}

object Return extends LowPriorityReturn0 {

  implicit def returnStreamDsl[ReturnValue, Domain](implicit restReturnDsl: Dsl[Return[ReturnValue], Domain, Domain])
    : Dsl[Return[ReturnValue], Stream[Domain], Nothing] =
    new Dsl[Return[ReturnValue], Stream[Domain], Nothing] {
      def cpsApply(keyword: Return[ReturnValue], handler: Nothing => Stream[Domain]): Stream[Domain] = {
        restReturnDsl.cpsApply(Return(keyword.returnValue), identity) #:: Stream.empty[Domain]
      }
    }

}

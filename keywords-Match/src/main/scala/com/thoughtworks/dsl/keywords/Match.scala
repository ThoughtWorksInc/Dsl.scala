package com.thoughtworks.dsl
package keywords
import Dsl.IsKeyword

type Match[+Keyword, +CaseSet] = keywords.FlatMap[Keyword, CaseSet]
object Match {
  export FlatMap.apply
  case class WithIndex[Index <: Int, +Keyword](index: Index, keyword: Keyword) extends Dsl.Keyword.Trait

  opaque type +:[+A, +B] = A | B

  object WithIndex:
    given [
        Index <: Int,
        Keyword,
        Domain,
        LastValue
    ](using
        dsl: Dsl[Keyword, Domain, LastValue],
        valueOfIndex: ValueOf[Index]
    ): Dsl.Composed[WithIndex[Index, Keyword] +: Nothing, Domain, LastValue] with {
      def cpsApply(keywordWithIndex: WithIndex[Index, Keyword] +: Nothing, handler: LastValue => Domain): Domain = {
        keywordWithIndex match {
          case WithIndex(valueOfIndex.value, keyword) =>
            dsl.cpsApply(keyword, handler)
          case _ =>
            throw new IllegalArgumentException("Invalid index")
        }
      }
    }

    given [
        Index <: Int,
        LeftKeyword,
        RestKeyword,
        Domain,
        Value
    ](using
        leftDsl: Dsl[LeftKeyword, Domain, Value],
        valueOfIndex: ValueOf[Index],
        restDsl: Dsl[RestKeyword, Domain, Value]
    ): Dsl.Composed[WithIndex[Index, LeftKeyword] +: RestKeyword, Domain, Value] with {
      def cpsApply(keywordUnion: WithIndex[Index, LeftKeyword] +: RestKeyword, handler: Value => Domain): Domain = {
        keywordUnion match {
          case WithIndex(valueOfIndex.value, leftKeyword) =>
            leftDsl.cpsApply(leftKeyword.asInstanceOf[LeftKeyword], handler)
          case _ =>
            restDsl.cpsApply(keywordUnion.asInstanceOf[RestKeyword], handler)
        }
      }
    }

}
// @inline def Match = keywords.FlatMap

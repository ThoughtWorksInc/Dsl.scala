package com.thoughtworks.dsl
package keywords
import Dsl.IsKeyword

  
type Match[Keyword, Value, CaseSet] = keywords.FlatMap[Keyword, Value, CaseSet]
object Match {
  export FlatMap.apply
  case class WithIndex[Index <: Int, Keyword](index: Index, keyword: Keyword)

  opaque type +:[A, B] >: A | B <: A | B = A | B

  given[
    Index <: Int, Keyword, Domain, LastValue
  ](
    given
    dsl: Dsl[Keyword, Domain, LastValue], 
    valueOfIndex: ValueOf[Index]
  ): Dsl[WithIndex[Index, Keyword] +: Nothing, Domain, LastValue] {
    def cpsApply(keywordWithIndex: WithIndex[Index, Keyword] +: Nothing, handler: LastValue => Domain): Domain = {
      keywordWithIndex match {
        case WithIndex(valueOfIndex.value, keyword) =>
          dsl.cpsApply(keyword, handler)
        case _ =>
          throw new IllegalArgumentException("Invalid index")
      }
    }
  }

  given[
    Index <: Int, LeftKeyword, RestKeyword, Domain, Value
  ](
    given
    leftDsl: Dsl[LeftKeyword, Domain, Value],
    valueOfIndex: ValueOf[Index],
    restDsl: Dsl[RestKeyword, Domain, Value]
  ): Dsl[WithIndex[Index, LeftKeyword] +: RestKeyword, Domain, Value]  {
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

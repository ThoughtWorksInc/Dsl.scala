package com.thoughtworks.dsl
package keywords
import Dsl.IsKeyword

type Match[+Keyword, Value, +CaseSet] = keywords.FlatMap[Keyword, Value, CaseSet]
object Match {
  export FlatMap.apply
  case class WithIndex[Index <: Int, +Keyword](index: Index, keyword: Keyword) extends Dsl.Keyword.Trait

  opaque type +:[A, B] = A | B

  given [
      Index <: Int,
      Keyword,
      Domain,
      LastValue
  ](using
      dsl: Dsl.PolyCont[Keyword, Domain, LastValue],
      valueOfIndex: ValueOf[Index]
  ): Dsl.PolyCont[WithIndex[Index, Keyword] +: Nothing, Domain, LastValue] with {
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
      leftDsl: Dsl.PolyCont[LeftKeyword, Domain, Value],
      valueOfIndex: ValueOf[Index],
      restDsl: Dsl.PolyCont[RestKeyword, Domain, Value]
  ): Dsl.PolyCont[WithIndex[Index, LeftKeyword] +: RestKeyword, Domain, Value] with {
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

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
        dsl: Dsl.Searching[Keyword, Domain, LastValue],
        valueOfIndex: ValueOf[Index]
    ): Dsl.Composed[WithIndex[Index, Keyword] +: Nothing, Domain, LastValue] = Dsl.Composed {
      (keywordWithIndex: WithIndex[Index, Keyword] +: Nothing, handler: LastValue => Domain) =>
        keywordWithIndex match {
          case WithIndex(valueOfIndex.value, keyword) =>
            dsl(keyword, handler)
          case _ =>
            throw new IllegalArgumentException("Invalid index")
        }
    }

    given [
        Index <: Int,
        LeftKeyword,
        RestKeyword,
        Domain,
        Value
    ](using
        leftDsl: Dsl.Searching[LeftKeyword, Domain, Value],
        valueOfIndex: ValueOf[Index],
        restDsl: Dsl.Searching[RestKeyword, Domain, Value]
    ): Dsl.Composed[WithIndex[Index, LeftKeyword] +: RestKeyword, Domain, Value] = Dsl.Composed {
      (keywordUnion: WithIndex[Index, LeftKeyword] +: RestKeyword, handler: Value => Domain) =>
        keywordUnion match {
          case WithIndex(valueOfIndex.value, leftKeyword) =>
            leftDsl(leftKeyword.asInstanceOf[LeftKeyword], handler)
          case _ =>
            restDsl(keywordUnion.asInstanceOf[RestKeyword], handler)
        }
    }

}
// @inline def Match = keywords.FlatMap

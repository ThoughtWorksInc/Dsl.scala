package com.thoughtworks.dsl
package keywords
import Dsl.AsKeyword
import Dsl.Typed




opaque type ForYield[Keyword, AsKeyword.IsKeyword, OutputValue] = Dsl.Typed[Keyword, AsKeyword.IsKeyword]
object ForYield {
  @inline def apply[Keyword, AsKeyword.IsKeyword, OutputValue]: Dsl.Typed[Keyword, AsKeyword.IsKeyword] =:= ForYield[Keyword, AsKeyword.IsKeyword, OutputValue] = summon
}

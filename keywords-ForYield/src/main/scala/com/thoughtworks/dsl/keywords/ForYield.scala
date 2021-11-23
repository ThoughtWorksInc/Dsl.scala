package com.thoughtworks.dsl
package keywords
import Dsl.IsKeyword
import Dsl.Typed




opaque type ForYield[Keyword, IsKeyword, OutputValue] = Dsl.Typed[Keyword, IsKeyword]
object ForYield {
  @inline def cast[Keyword, IsKeyword, OutputValue]: Dsl.Typed[Keyword, IsKeyword] <:< ForYield[Keyword, IsKeyword, OutputValue] = implicitly
}

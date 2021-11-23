package com.thoughtworks.dsl
package keywords
import Dsl.IsKeyword
import Dsl.Typed

opaque type Shift[R, A] = Dsl.Continuation[R, A]
object Shift {
  def apply[R, A](continuation: Dsl.Continuation[R, A]): Shift[R, A] = continuation
}

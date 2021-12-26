package com.thoughtworks.dsl
package keywords
import Dsl.IsKeyword
import Dsl.cpsApply
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scala.util.control.TailCalls.TailRec
import scala.util.control.TailCalls

object Fence extends Dsl.Keyword.Trait {

  given IsKeyword[Fence.type, Unit] with {}

  given [Domain](using
      util.NotGiven[Dsl.Derived.StackSafe[Fence.type, Domain, Unit]],
      util.NotGiven[Dsl.Derived.StackUnsafe[Fence.type, Domain, Unit]]
  ): Dsl.Original[Fence.type, Domain, Unit] = Dsl.Original {
    (keyword, handler) => handler(())
  }

}

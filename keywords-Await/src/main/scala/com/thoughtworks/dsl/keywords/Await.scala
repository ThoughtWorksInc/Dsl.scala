package com.thoughtworks.dsl
package keywords
import Dsl.IsKeyword
import Dsl.Typed




opaque type Await[Result] = concurrent.Future[Result]
object Await {
  def apply[Result](future: concurrent.Future[Result]): Await[Result] = future
  @inline def cast[Result]: concurrent.Future[Result] <:< Await[Result] = implicitly
  given[Result]: IsKeyword[Await[Result], Result] with {}
  given[
    Result,
    DomainElement,
    Value >: Result,
  ](
    using concurrent.ExecutionContext
  ): Dsl[Await[Result], concurrent.Future[DomainElement], Value] with {
    def cpsApply(keyword: Await[Result], handler: Value => concurrent.Future[DomainElement]): concurrent.Future[DomainElement] = {
      keyword.flatMap(handler)
    }
  }

}
package com.thoughtworks.dsl.keywords
import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.Keyword

/** A [[Keyword]] to put the [[value]] to the context.
  *
  * Purely functional programming languages usually do not support native first-class mutable variables.
  * In those languages, mutable states can be implemented in state monads.
  *
  * [[Put]] and [[Get]] are the [[Dsl]]-based replacements of state monads.
  *
  * @example The parameter of a [[scala.Function1]] can be read from [[Get]] keyword, and changed by [[Put]] keyword.
  *
  *          {{{
  *          def dslBasedState: String => Int = {
  *            !Get[String]() should be("initial value")
  *            !Put("changed value")
  *            !Get[String]() should be("changed value")
  *            !Return(0)
  *          }
  *
  *          dslBasedState("initial value") should be(0)
  *          }}}
  *
  *          The implementation of [[Get]] and [[Put]] keywords does not use native Scala `var`,
  *          though its behavior is similar to `var`.
  * @example The behavior of the above code is equivalent to the following code based on native Scala `var`:
  *
  *          {{{
  *          def varBasedState(initialValue: String): Int = {
  *            var v = initialValue
  *            v should be("initial value")
  *            v = "changed value"
  *            v should be("changed value")
  *            return 0
  *          }
  *
  *          varBasedState("initial value") should be(0)
  *          }}}
  *
  * @example [[Put]] and [[Get]] support multiple states.
  *
  *          The following code creates a formatter that [[Put]] parts of content into a `List[Any]` of string buffers.
  *
  *          {{{
  *          def format: Double => Int => List[Any] => String = {
  *            !Put("x=" :: !Get[List[Any]])
  *            !Put(!Get[Double] :: !Get[List[Any]])
  *            !Put(",y=" :: !Get[List[Any]])
  *            !Put(!Get[Int] :: !Get[List[Any]])
  *
  *            !Return((!Get[List[Any]]).reverse.mkString)
  *          }
  *
  *          format(0.5)(42)(Nil) should be("x=0.5,y=42")
  *          }}}
  * @see [[Get]]
  * @author 杨博 (Yang Bo)
  */
final case class Put[A](value: A) extends AnyVal with Keyword[Put[A], Unit]

object Put {

  implicit def putDsl[A, B >: A, C] = new Dsl[Put[A], B => C, Unit] {
    def cpsApply(keyword: Put[A], handler: Unit => B => C): B => C = {
      val newValue = keyword.value;
      { oldValue =>
        handler(())(newValue)
      }
    }
  }
}

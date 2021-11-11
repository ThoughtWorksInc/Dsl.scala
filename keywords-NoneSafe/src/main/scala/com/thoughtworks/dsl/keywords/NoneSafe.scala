package com.thoughtworks.dsl
package keywords
import com.thoughtworks.dsl.Dsl.Keyword
import scala.language.implicitConversions

final case class NoneSafe[A](option: Option[A]) extends AnyVal with Keyword[NoneSafe[A], A]

object NoneSafe {

  implicit def noneSafeDsl[A, Domain](implicit
      continueDsl: Dsl[Return[None.type], Domain, Nothing]
  ): Dsl[NoneSafe[A], Domain, A] =
    new Dsl[NoneSafe[A], Domain, A] {
      def cpsApply(keyword: NoneSafe[A], handler: A => Domain): Domain = {
        keyword.option match {
          case None =>
            continueDsl.cpsApply(Return(None), identity)
          case Some(a) =>
            handler(a)
        }
      }
    }

  implicit def implicitNoneSafe[A](option: Option[A]): NoneSafe[A] = NoneSafe(option)

}

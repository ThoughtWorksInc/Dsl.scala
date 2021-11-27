package com.thoughtworks.dsl.keywords
import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.IsKeyword
import com.thoughtworks.dsl.bangnotation.unary_!

import scala.language.implicitConversions
import scala.language.higherKinds
import scala.annotation.compileTimeOnly

/** [[NullSafe]] is a keyword to perform `null` check.
  *
  * @example
  *   You can use [[NullSafe$.? ?]] annotation to represent a nullable value.
  *
  * {{{
  *           import com.thoughtworks.dsl.keywords.NullSafe._
  *
  *           case class Tree(left: Tree @ $qmark = null, right: Tree @ $qmark = null, value: String @ $qmark = null)
  *
  *           val root: Tree @ $qmark = Tree(
  *             left = Tree(
  *               left = Tree(value = "left-left"),
  *               right = Tree(value = "left-right")
  *             ),
  *             right = Tree(value = "right")
  *           )
  * }}}
  *
  * A normal `.` is not null safe, when selecting `left`, `right` or `value` on a `null` value.
  *
  * {{{
  *           a[NullPointerException] should be thrownBy {
  *             root.right.left.right.value
  *           }
  * }}}
  *
  * The above code throws an exception because `root.right.left` is `null`.
  *
  * The exception can be avoided by using [[?]] on a nullable value:
  *
  * {{{
  *           root.?.right.?.left.?.right.?.value should be(null)
  * }}}
  *
  * The entire expression will be `null` if one of [[?]] is performed on a `null` value.
  *
  * <hr/>
  *
  * The boundary of a null safe operator [[?]] is the nearest enclosing expression whose type is annotated as `| Null`.
  *
  * {{{
  *           ("Hello " + ("world " + root.?.right.?.left.?.value)) should be("Hello world null")
  *           ("Hello " + (("world " + root.?.right.?.left.?.value.?): @ $qmark)) should be("Hello null")
  *           (("Hello " + ("world " + root.?.right.?.left.?.value.?)): @ $qmark) should be(null)
  * }}}
  *
  * @example
  *   The [[?]] operator usually works with Java libraries that may produce `null`.
  *
  * {{{
  *           import com.thoughtworks.dsl.keywords.NullSafe._
  *
  *           val myMap = new java.util.HashMap[String, String]();
  *           ((myMap.get("key1").? + myMap.get("key2").?): @ $qmark) should be(null)
  * }}}
  *
  * @note
  *   The [[?]] operator is only available on nullable values.
  *
  * A type is considered as nullable if it is a reference type, no matter it is annotated as `| Null` or not.
  *
  * {{{
  *       import com.thoughtworks.dsl.keywords.NullSafe._
  *
  *       val explicitNullable: String @ $qmark = null
  *       ((explicitNullable.? + " Doe") : @ $qmark) should be(null)
  * }}}
  *
  * {{{
  *       val implicitNullable: String = null
  *       ((implicitNullable.? + " Doe") : @ $qmark) should be(null)
  * }}}
  *
  * A type is considered as not nullable if it is a value type.
  *
  * {{{
  *       val implicitNotNullable: Int = 0
  *       "(implicitNotNullable.? + 42) : @ $qmark" shouldNot compile
  * }}}
  *
  * Alternatively, a type can be considered as not nullable by explicitly converting it to
  * [[com.thoughtworks.dsl.keywords.NullSafe.NotNull[A]* NotNull]].
  *
  * {{{
  *       val explicitNotNullable: NotNull[String] = NotNull("John")
  *       """(explicitNotNullable.? + " Doe") : @ $qmark""" shouldNot compile
  * }}}
  *
  * @see
  *   [[NoneSafe]] for similar checks on [[scala.Option]]s.
  * @author
  *   杨博 (Yang Bo)
  *
  * @define qmark
  *   ?
  */
opaque type NullSafe[A] = A | Null

object NullSafe {

  extension [A](keyword: NullSafe[A])
    transparent inline def ? : A = {
      !keyword
    }

  given [A]: IsKeyword[NullSafe[A], A] with {}

  implicit def implicitNullSafe[A](nullable: A | Null): NullSafe[A] = nullable

  given [A, Domain]: Dsl[NullSafe[A], Domain | Null, A] = { (keyword: NullSafe[A], handler: A => Domain | Null) =>
    keyword match {
      case null =>
        null
      case notNull: A =>
        handler(notNull)
    }
  }

}

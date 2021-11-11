package com.thoughtworks.dsl.keywords
import com.thoughtworks.dsl.Dsl.{reset, shift}
import com.thoughtworks.dsl.keywords.NullSafe.?
import com.thoughtworks.dsl.keywords.NullSafe.NotNull

import scala.language.implicitConversions
import scala.language.higherKinds
import scala.annotation.compileTimeOnly

/** [[NullSafe]] is a keyword to perform `null` check.
  *
  * @example You can use [[NullSafe$.? ?]] annotation to represent a nullable value.
  *
  *          {{{
  *          import com.thoughtworks.dsl.keywords.NullSafe._
  *
  *          case class Tree(left: Tree @ $qmark = null, right: Tree @ $qmark = null, value: String @ $qmark = null)
  *
  *          val root: Tree @ $qmark = Tree(
  *            left = Tree(
  *              left = Tree(value = "left-left"),
  *              right = Tree(value = "left-right")
  *            ),
  *            right = Tree(value = "right")
  *          )
  *          }}}
  *
  *          A normal `.` is not null safe, when selecting `left`, `right` or `value` on a `null` value.
  *
  *          {{{
  *          a[NullPointerException] should be thrownBy {
  *            root.right.left.right.value
  *          }
  *          }}}
  *
  *          The above code throws an exception because `root.right.left` is `null`.
  *
  *          The exception can be avoided by using [[?]] on a nullable value:
  *
  *          {{{
  *          root.?.right.?.left.?.right.?.value should be(null)
  *          }}}
  *
  *          The entire expression will be `null` if one of [[?]] is performed on a `null` value.
  *
  *          <hr/>
  *
  *          The boundary of a null safe operator [[?]] is the nearest enclosing expression
  *          whose type is annotated as `@ ?`.
  *
  *          {{{
  *          ("Hello " + ("world " + root.?.right.?.left.?.value)) should be("Hello world null")
  *          ("Hello " + (("world " + root.?.right.?.left.?.value.?): @ $qmark)) should be("Hello null")
  *          (("Hello " + ("world " + root.?.right.?.left.?.value.?)): @ $qmark) should be(null)
  *          }}}
  *
  * @example The [[?]] operator usually works with Java libraries that may produce `null`.
  *
  *          {{{
  *          import com.thoughtworks.dsl.keywords.NullSafe._
  *
  *          val myMap = new java.util.HashMap[String, String]();
  *          ((myMap.get("key1").? + myMap.get("key2").?): @ $qmark) should be(null)
  *          }}}
  *
  * @note The [[?]] operator is only available on nullable values.
  *
  *       A type is considered as nullable if it is a reference type,
  *       no matter it is annotated as `@ ?` or not.
  *
  *       {{{
  *       import com.thoughtworks.dsl.keywords.NullSafe._
  *
  *       val explicitNullable: String @ $qmark = null
  *       ((explicitNullable.? + " Doe") : @ $qmark) should be(null)
  *       }}}
  *
  *       {{{
  *       val implicitNullable: String = null
  *       ((implicitNullable.? + " Doe") : @ $qmark) should be(null)
  *       }}}
  *
  *       A type is considered as not nullable if it is a value type.
  *
  *       {{{
  *       val implicitNotNullable: Int = 0
  *       "(implicitNotNullable.? + 42) : @ $qmark" shouldNot compile
  *       }}}
  *
  *       Alternatively, a type can be considered as not nullable
  *       by explicitly converting it to [[com.thoughtworks.dsl.keywords.NullSafe.NotNull[A]* NotNull]].
  *
  *       {{{
  *       val explicitNotNullable: NotNull[String] = NotNull("John")
  *       """(explicitNotNullable.? + " Doe") : @ $qmark""" shouldNot compile
  *       }}}
  *
  * @see [[NoneSafe]] for similar checks on [[scala.Option]]s.
  * @author 杨博 (Yang Bo)
  *
  * @define qmark ?
  */
final case class NullSafe[A <: AnyRef](nullable: A @ ?) extends AnyVal {

  @inline
  final def cpsApply[Domain >: Null](handler: NotNull[A] => Domain @ ?): Domain @ ? = {
    if (nullable == null) {
      null
    } else {
      handler(NullSafe.OpaqueTypes.toNotNull(nullable))
    }
  }

  @shift
  @compileTimeOnly(
    """This method requires the compiler plugin: `addCompilerPlugin("com.thoughtworks.dsl" %% "compilerplugins-bangnotation" % "latest.release")` and must only be called inside a code block annotated as `@reset`."""
  )
  final def ? : NotNull[A] = {
    throw new IllegalAccessException(
      """This method requires the compiler plugin: `addCompilerPlugin("com.thoughtworks.dsl" %% "compilerplugins-bangnotation" % "latest.release")` and must only be called inside a code block annotated as `@reset`."""
    )
  }

}

object NullSafe {

  private[NullSafe] trait OpaqueTypes {
    type NotNull[+A] <: A

    private[NullSafe] def toNotNull[A](a: A): NotNull[A]
  }

  private[NullSafe] val OpaqueTypes: OpaqueTypes = new OpaqueTypes {
    type NotNull[+A] = A
    private[NullSafe] def toNotNull[A](a: A) = a
  }

  /** @usecase type NotNull[+A] <: A
    */
  type NotNull[+A] = OpaqueTypes.NotNull[A]

  /** Returns `a` if `a` is not `null`.
    *
    * @return `a` if `a` is not `null`.
    *
    *         {{{
    *         import com.thoughtworks.dsl.keywords.NullSafe._
    *
    *         val o = new AnyRef
    *         NotNull(o) should be(o)
    *         }}}
    *
    * @throws java.lang.NullPointerException if `a` is `null`.
    *
    *         {{{
    *         import com.thoughtworks.dsl.keywords.NullSafe._
    *         a[NullPointerException] should be thrownBy {
    *           NotNull(null)
    *         }
    *         }}}
    */
  def NotNull[A](a: A): NotNull[A] = {
    if (a == null) {
      throw new NullPointerException
    } else {
      OpaqueTypes.toNotNull(a)
    }
  }

  /** @template */
  type ? = reset

  implicit def implicitNullSafe[A <: AnyRef](nullable: A @ ?) = new NullSafe[A](nullable)

}

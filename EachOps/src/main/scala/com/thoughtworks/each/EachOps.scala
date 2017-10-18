package com.thoughtworks.each

import scala.annotation.{StaticAnnotation, TypeConstraint, compileTimeOnly}
import scala.reflect.macros.whitebox
import scala.language.experimental.macros
/**
  * @author 杨博 (Yang Bo)
  */
trait EachOps[+A] extends Any {
  @compileTimeOnly("""Magic call to `each` method requires compiler plugin: `addCompilerPlugin("com.thoughtworks.each" %% "compilerplugin" % "latest.release")` """)
  def each: A = ???

  /** Alias of `each` */
  @compileTimeOnly("""Magic call to `!` method requires compiler plugin: `addCompilerPlugin("com.thoughtworks.each" %% "compilerplugin" % "latest.release")` """)
  def unary_! : A = ???
}

object EachOps {
  /**
    * @note Used internally only.
    */
  final class reset extends StaticAnnotation with TypeConstraint

}

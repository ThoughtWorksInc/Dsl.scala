package com.thoughtworks.each

import scala.annotation.{StaticAnnotation, TypeConstraint, compileTimeOnly}

/**
  * @author 杨博 (Yang Bo)
  */
trait EachOps[+A] extends Any {
  @compileTimeOnly("Magic call to `!` method should be translated to map/flatMap at compile time")
  def ! : A = ???
}

object EachOps {

  /**
    * @note Used internally only.
    */
  final class reset extends StaticAnnotation with TypeConstraint

}

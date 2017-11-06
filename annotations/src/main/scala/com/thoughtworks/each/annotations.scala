package com.thoughtworks.each

import scala.annotation.{StaticAnnotation, TypeConstraint}

/**
  * @author 杨博 (Yang Bo)
  */
object annotations {

  /**
    * @note Used internally only.
    */
  final class reset extends StaticAnnotation with TypeConstraint

  /**
    * @note Used internally only.
    */
  final class shift extends StaticAnnotation

}

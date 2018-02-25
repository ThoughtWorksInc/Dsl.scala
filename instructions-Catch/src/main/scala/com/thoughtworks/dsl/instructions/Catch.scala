package com.thoughtworks.dsl.instructions

import com.thoughtworks.dsl.Dsl.Instruction

/**
  * @author 杨博 (Yang Bo)
  */
final case class Catch[Domain](onFailure: Throwable => Domain)
    extends AnyVal
    with Instruction[Catch[Domain], Domain => Domain]

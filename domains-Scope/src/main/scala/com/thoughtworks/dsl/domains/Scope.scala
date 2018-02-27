package com.thoughtworks.dsl.domains

import com.thoughtworks.dsl.Dsl

/**
  * @author 杨博 (Yang Bo)
  */
trait Scope[OtherDomain] extends ((OtherDomain => OtherDomain) => OtherDomain)

object Scope {

  def noop[OtherDomain](domain: OtherDomain): Scope[OtherDomain] = new Scope[OtherDomain] {
    def apply(handler: OtherDomain => OtherDomain): OtherDomain = handler(domain)
  }

  implicit def scopeDsl[Instruction, Domain, A](
      implicit restDsl: Dsl[Instruction, Domain, A]): Dsl[Instruction, Scope[Domain], A] = {
    new Dsl[Instruction, Scope[Domain], A] {
      def interpret(instruction: Instruction, handler: A => Scope[Domain]): Scope[Domain] = {
        new Scope[Domain] {
          def apply(continue: Domain => Domain): Domain =
            restDsl.interpret(instruction, { a =>
              handler(a)(continue)
            })
        }
      }
    }
  }

}

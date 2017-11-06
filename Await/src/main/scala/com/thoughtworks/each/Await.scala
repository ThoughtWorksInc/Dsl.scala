package com.thoughtworks.each

/**
  * @author 杨博 (Yang Bo)
  */
final case class Await[State, A](asyncFunction: Await.AsyncFunction[State, A])
    extends /* AnyVal with */ Continuation.InstructionOps[Await[State, A], A] {
  protected def self: Await[State, A] = this
}

object Await {
  type AsyncFunction[State, +A] = (A => State) => State

  implicit def awaitCps[State, A]: Continuation[Await[State, A], State, A] =
    new Continuation[Await[State, A], State, A] {
      def cpsApply(self: Await[State, A], mapper: A => State): State = self.asyncFunction(mapper)
    }

}

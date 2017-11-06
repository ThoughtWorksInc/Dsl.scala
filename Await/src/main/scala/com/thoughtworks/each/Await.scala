package com.thoughtworks.each

import com.thoughtworks.each.Await.AsyncFunction

/**
  * @author 杨博 (Yang Bo)
  */
final case class Await[State, A](f: AsyncFunction[State, A])
    extends /* AnyVal with */ Continuation.InstructionOps[Await[State, A], A] {
  override def self: Await[State, A] = this
}

object Await {
  type AsyncFunction[State, +A] = (A => State) => State

  implicit def continuationCps[State, A]: Continuation[Await[State, A], State, A] =
    new Continuation[Await[State, A], State, A] {
      def cpsApply(self: Await[State, A], mapper: A => State): State = self.f(mapper)
    }

}

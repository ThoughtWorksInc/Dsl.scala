package com.thoughtworks

/** This project, '''Dsl.scala''', is a framework to create embedded '''D'''omain-'''S'''pecific '''L'''anguages compatible with Scala control flows.
  *
  * We also provide some built-in DSLs for asynchronous programming, collection manipulation,
  * and any other classes that support [[scalaz.Monad]] or [[cats.Monad]].
  * Those built-in DSLs can be used as a replacement of
  * [[https://docs.scala-lang.org/tour/for-comprehensions.html `for` comprehension]] or monadic expression.
  *
  * = Introduction =
  *
  * Embedded DSLs usually consist of a set of domain-specific instructions,
  * which can be embedded in the their hosting languages.
  *
  * == Reinventing control flow in DSL ==
  *
  * Ideally, a domain-specific instruction should be an optional extension,
  * can be present everywhere in the ordinary control flow of the hosting language.
  * However, in practice, many of embedded DSLs badly interoperate with hosting language control flow.
  * Instead, they reinvent control flow in their own DSL.
  *
  * For example, the [[https://akka.io akka]] provides
  * [[https://doc.akka.io/docs/akka/2.5.10/fsm.html a DSL to create Finite State Machines]],
  * which consists of some domain-specific instructions like [[akka.actor.AbstractFSM.when when]],
  * [[akka.actor.AbstractFSM.goto goto]] and [[akka.actor.AbstractFSM.stay stay]].
  * Unfortunately, you cannot embedded those instructions into your ordinary `if` / `while` / `try` control flows,
  * because Akka's DSL is required to be split into small closures,
  * preventing ordinary control flows from crossing the boundary of those closures.
  *
  * TensorFlow's [[https://www.tensorflow.org/api_guides/python/control_flow_ops control flow operations]] and
  * Caolan's [[https://github.com/caolan/async async]] library are examples of reinventing control flow
  * in other languages.
  *
  * = Package Structure =
  *
  */
package object dsl

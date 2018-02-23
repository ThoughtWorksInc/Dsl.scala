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
  * which consists of some domain-specific instructions like [[akka.actor.AbstractFSM#when when]],
  * [[akka.actor.AbstractFSM#goto goto]] and [[akka.actor.AbstractFSM#stay stay]].
  * Unfortunately, you cannot embedded those instructions into your ordinary `if` / `while` / `try` control flows,
  * because Akka's DSL is required to be split into small closures,
  * preventing ordinary control flows from crossing the boundary of those closures.
  *
  * TensorFlow's [[https://www.tensorflow.org/api_guides/python/control_flow_ops control flow operations]] and
  * Caolan's [[https://github.com/caolan/async async]] library are examples of reinventing control flow
  * in languages other than Scala.
  *
  * == Monad: an interface of control flow ==
  *
  * It's too trivial to reinvent the whole set of control flow for each DSL.
  * A simpler approach is only implementing a minimal interface required for control flows for each domain,
  * while the syntax of other control flow operations are derived from the interface, shared between different domains.
  *
  * Since [[https://www.sciencedirect.com/science/article/pii/0890540191900524 computation can be represented as monads]],
  * some libraries use monad as the interface of control flow,
  * including [[scalaz.Monad]], [[cats.Monad]] and [[com.twitter.algebird.Monad]].
  * A DSL author only have to implement two abstract method in [[scalaz.Monad]],
  * and all the derived control flow operations
  * like [[scalaz.syntax.MonadOps.whileM whileM]], [[scalaz.syntax.BindOps.ifM ifM]] are available.
  * In addition, those monadic data type can be created and composed
  * from Scala's built-in [[https://docs.scala-lang.org/tour/for-comprehensions.html `for` comprehension]].
  *
  * For example, you can use the same [[scalaz.syntax syntax]] to create [[org.scalacheck.Gen random value generators]]
  * and [[com.thoughtworks.binding.Binding data-binding expressions]],
  * as long as there are [[scalaz.Monad Monad]] instances
  * for [[org.scalacheck.Gen]] and [[com.thoughtworks.binding.Binding]].
  *
  * Although the effort of creating a DSL is minimized with the help of monads,
  * the syntax is still unsatisfactory.
  * Methods in `MonadOps` still seem like reinventing control flow,
  * and `for` comprehension supports only a limited set of functionality in comparison to ordinary control flows.
  * `if` / `while` / `try` and other block expressions cannot appear in the enumerator clause of `for` comprehension.
  *
  * == Native control flows via monads ==
  *
  * == Continuation: The Mother of all Monads ==
  *
  * == Adaptive [[Dsl]] type class ==
  *
  * = Package Structure =
  *
  * @see [[https://www.schoolofhaskell.com/user/dpiponi/the-mother-of-all-monads The Mother of all Monads]]
  * @see [[https://docs.scala-lang.org/tour/for-comprehensions.html `for` comprehension]]
  * @see [[https://github.com/scala/scala-continuations scala-continuation]]
  * @see [[https://github.com/scala/scala-async scala-async]]
  * @see [[scalaz.syntax.MonadOps]],
  * @see [[cats.syntax.MonadOps]],
  * @see [[https://github.com/ThoughtWorksInc/each ThoughtWorks Each]]
  * @see [[http://monadless.io/ Monadless]]
  * @see [[https://github.com/pelotom/effectful effectful]]
  *
  */
package object dsl

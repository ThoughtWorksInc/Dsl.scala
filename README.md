# Dsl.scala <a href="http://thoughtworks.com/"><img align="right" src="https://www.thoughtworks.com/imgs/tw-logo.png" title="ThoughtWorks" height="15"/></a>

[![Build Status](https://travis-ci.org/ThoughtWorksInc/Dsl.scala.svg?branch=master)](https://travis-ci.org/ThoughtWorksInc/Dsl.scala)
[![Maven Central](https://img.shields.io/maven-central/v/com.thoughtworks.dsl/dsl_2.13.svg)](https://search.maven.org/search?q=g:com.thoughtworks.dsl)
[![Scaladoc](https://javadoc.io/badge/com.thoughtworks.dsl/dsl_2.12.svg?label=scaladoc)](https://javadoc.io/page/com.thoughtworks.dsl/dsl_2.12/latest/com/thoughtworks/dsl/index.html) [![Join the chat at https://gitter.im/ThoughtWorksInc/Dsl.scala](https://badges.gitter.im/ThoughtWorksInc/Dsl.scala.svg)](https://gitter.im/ThoughtWorksInc/Dsl.scala?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

**Dsl.scala** is a framework to create embedded **D**omain-**S**pecific **L**anguages in Scala. It can be considered as an alternative syntax to for comprehension, Scala Async and Scala Continuations. It unifies monads, generators, asynchronous functions, coroutines and continuations to a single universal syntax, and can be easily integrate to Scalaz, Cats, Scala Collections, Scala Futures, Akka HTTP, Java NIO, or your custom domains.

A DSL author is able to create language keywords by implementing the [Dsl](https://javadoc.io/page/com.thoughtworks.dsl/dsl_2.12/latest/com/thoughtworks/dsl/Dsl.html) trait, which contains only one abstract method to be implemented. No knowledge about Scala compiler or AST macros is required.

DSLs written in **Dsl.scala** are collaborative with others DSLs and Scala control flows. A DSL user can create functions that contains interleaved DSLs implemented by different vendors, along with ordinary Scala control flows.

We also provide some built-in keywords, including:

 * The `Await` keyword for creating memoized asynchronous values as Scala [Future](https://docs.scala-lang.org/overviews/core/futures.html)s, similar to the `await` / `async` keywords in C#, Python and JavaScript.
 * The `Shift` keyword for creating asynchronous tasks as delimited continuations, similar to the `shift` operator in [Scala Continuations](https://github.com/scala/scala-continuations).
 * The `AsynchronousIo.Connect`, `AsynchronousIo.Accept`, `AsynchronousIo.Read` and `AsynchronousIo.Write` keywords for performing I/O on an asynchronous channel.
 * The `Yield` keyword for generating lazy streams, similar to `yield` in C#, Python and JavaScript.
 * The `Each` keyword for iterating on a collection, similar to the list comprehension feature in Scala, Haskell, OCaml, Python and Lisp.
 * The `Continue` keyword LDK for skipping an element in an `Each` collection comprehension, similar to the native `continue` keyword in C/C++ or the `mzero` in Haskell.
 * The `Fork` keyword for duplicating current context, similar to the `fork` system call in POSIX.
 * The `Return` keyword for early returning, similar to the native `return` keyword in Scala.
 * The `Using` keyword to automatically close resources when exiting a scope, similar to the native `using` keyword in C#.
 * The `Monadic` keyword for creating Scalaz or Cats monadic control flow, similar to the !-notation in Idris.
 * The `NullSafe` keyword for the null safe operator, similar to the `?` operator in Kotlin and Groovy.
 * The `NoneSafe` keyword for the `None` safe operator, similar to the `Maybe` monad in Haskell.

All the above keywords can be used together with each others. For example you can perform list comprehension to manipulate native resources in an asynchronous task by using `Each`, `Using` and `Shift` together.

## Getting Started

Suppose you want to create a random number generator. The generated numbers should be stored in a lazily evaluated infinite stream, which can be built with the help of our built-in domain-specific keyword `Yield`.

So, you need to add the library that contains the implementation of the keyword `Yield`:

``` scala
// Add the "keywords-yield" library in your build.sbt, to use the `Yield` keyword
libraryDependencies += "com.thoughtworks.dsl" %% "keywords-yield" % "latest.release"

// Add other "keywords-xxx" libraries in your build.sbt, to use other keywords
// libraryDependencies += "com.thoughtworks.dsl" %% "keywords-xxx" % "latest.release"
```

And the Dsl.scala compiler plug-ins that are shared by all DSLs:   

``` scala
// Add the following settings in your build.sbt 
addCompilerPlugin("com.thoughtworks.dsl" %% "compilerplugins-bangnotation" % "latest.release")
addCompilerPlugin("com.thoughtworks.dsl" %% "compilerplugins-reseteverywhere" % "latest.release")
```

The random number generator can be implemented as a recursive function that produces the next random number in each iteration.

```scala
import com.thoughtworks.dsl.keywords.Yield
// Must not annotated with @tailrec
def xorshiftRandomGenerator(seed: Int): scala.collection.immutable.Stream[Int] = {
  val tmp1 = seed ^ (seed << 13)
  val tmp2 = tmp1 ^ (tmp1 >>> 17)
  val tmp3 = tmp2 ^ (tmp2 << 5)
  !Yield(tmp3)
  xorshiftRandomGenerator(tmp3)
}
```

Note that a keyword is a plain case class. You need a `!` prefix to the keyword to activate the DSL.

It's done. We can test it in ScalaTest:

```scala
val myGenerator = xorshiftRandomGenerator(seed = 123)
myGenerator(0) should be(31682556)
myGenerator(1) should be(-276305998)
myGenerator(2) should be(2101636938)
```

The call to `xorshiftRandomGenerator` does not throw a `StackOverflowError` because the execution of `xorshiftRandomGenerator` will be paused at the keyword `Yield`, and it will be resumed when the caller is looking for the next number.

## Showcases

* [sbt-ammonite-classpath](https://github.com/ThoughtWorksInc/sbt-ammonite-classpath) is an sbt plug-in that [uses `Each` keywords to iterate through configuations and keys](https://github.com/ThoughtWorksInc/sbt-ammonite-classpath/blob/793bc20/src/main/scala/com/thoughtworks/deeplearning/sbtammoniteclasspath/AmmoniteClasspath.scala#L23), as an alternative syntax of `for` comprehensions.
* [Dsl.scala-akka-actor](https://github.com/Atry/Dsl.scala-akka-actor) provides the [Akka](https://akka.io/) Actor support for Dsl.scala. It is an alternative to [Akka FSM](https://doc.akka.io/docs/akka/current/fsm.html), for building actors with complex states from simple native Scala control flows.
* [Dsl.scala-akka-http](https://github.com/ThoughtWorksInc/Dsl.scala-akka-http/) contains utilities to integrate Akka HTTP with Dsl.scala.
* [dsl-domains-cats](https://github.com/ThoughtWorksInc/dsl-domains-cats) contains utilities to integrate Cats with Dsl.scala. It provides the `!`-notation for creating Cats monadic expressions.

  
(Feel free to add your project here)

## Links and related works

* Check the [Documentation](https://javadoc.io/page/com.thoughtworks.dsl/dsl_2.12/latest/com/thoughtworks/dsl/index.html) to find examples about using or creating DSLs.
* See [MVNRepository](http://mvnrepository.com/artifact/com.thoughtworks.dsl?sort=newest) or [Scaladex](https://index.scala-lang.org/thoughtworksinc/dsl.scala) for the settings of each built-in DSLs for your build tools.
* [Benchmarks: Dsl.scala vs Monix vs Cats Effect vs Scalaz Concurrent vs Scala Async vs Scala Continuation](https://github.com/ThoughtWorksInc/Dsl.scala/wiki/Benchmarks:-Dsl.scala-vs-Monix-vs-Cats-Effect-vs-Scalaz-Concurrent-vs-Scala-Async-vs-Scala-Continuation)
* [Control.Dsl](https://github.com/Atry/Control.Dsl) is the Haskell port of this library.
* The syntax of our `BangNotation` compiler plugin is inspired by [Idris' !-notation](http://docs.idris-lang.org/en/latest/tutorial/interfaces.html#notation).

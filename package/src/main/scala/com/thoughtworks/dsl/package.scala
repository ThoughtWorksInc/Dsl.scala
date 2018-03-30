package com.thoughtworks

/** This project, '''Dsl.scala''', is a framework to create embedded '''D'''omain-'''S'''pecific '''L'''anguages.
  *
  * DSLs written in '''Dsl.scala''' are collaborative with others DSLs and Scala control flows.
  * DSL users can create functions that contains interleaved DSLs implemented by different vendors,
  * along with ordinary Scala control flows.
  *
  * We also provide some built-in DSLs for asynchronous programming, collection manipulation,
  * and adapters to [[scalaz.Monad]] or [[cats.Monad]].
  * Those built-in DSLs can be used as a replacement of
  * [[https://docs.scala-lang.org/tour/for-comprehensions.html `for` comprehension]],
  * [[https://github.com/scala/scala-continuations scala-continuations]],
  * [[https://github.com/scala/scala-async scala-async]],
  * [[http://monadless.io/ Monadless]],
  * [[https://github.com/pelotom/effectful effectful]]
  * and [[https://github.com/ThoughtWorksInc/each ThoughtWorks Each]].
  *
  * = Introduction =
  *
  * == Reinventing control flow in DSL ==
  *
  * Embedded DSLs usually consist of a set of domain-specific keywords,
  * which can be embedded in the their hosting languages.
  *
  * Ideally, a domain-specific keyword should be an optional extension,
  * which can be present everywhere in the ordinary control flow of the hosting language.
  * However, previous embedded DSLs usually badly interoperate with hosting language control flow.
  * Instead, they reinvent control flow in their own DSL.
  *
  * For example, the [[https://akka.io akka]] provides
  * [[https://doc.akka.io/docs/akka/2.5.10/fsm.html a DSL to create finite-state machines]],
  * which consists of some domain-specific keywords like [[akka.actor.AbstractFSM#when when]],
  * [[akka.actor.AbstractFSM#goto goto]] and [[akka.actor.AbstractFSM#stay stay]].
  * Unfortunately, you cannot embedded those keywords into your ordinary `if` / `while` / `try` control flows,
  * because Akka's DSL is required to be split into small closures,
  * preventing ordinary control flows from crossing the boundary of those closures.
  *
  * TensorFlow's [[https://www.tensorflow.org/api_guides/python/control_flow_ops control flow operations]] and
  * Caolan's [[https://github.com/caolan/async async]] library are examples of reinventing control flow
  * in languages other than Scala.
  *
  * == Monad: the generic interface of control flow ==
  *
  * It's too trivial to reinvent the whole set of control flows for each DSL.
  * A simpler approach is only implementing a minimal interface required for control flows for each domain,
  * while the syntax of other control flow operations are derived from the interface, shared between different domains.
  *
  * Since [[https://www.sciencedirect.com/science/article/pii/0890540191900524 computation can be represented as monads]],
  * some libraries use monad as the interface of control flow,
  * including [[scalaz.Monad]], [[cats.Monad]] and [[com.twitter.algebird.Monad]].
  * A DSL author only have to implement two abstract method in [[scalaz.Monad]],
  * and all the derived control flow operations
  * like [[scalaz.syntax.MonadOps.whileM]], [[scalaz.syntax.BindOps.ifM]] are available.
  * In addition, those monadic data type can be created and composed
  * from Scala's built-in [[https://docs.scala-lang.org/tour/for-comprehensions.html `for` comprehension]].
  *
  * For example, you can use the same [[scalaz.syntax syntax]] or `for` comprehension
  * to create [[org.scalacheck.Gen random value generators]]
  * and [[com.thoughtworks.binding.Binding data-binding expressions]],
  * as long as there are [[scalaz.Monad Monad]] instances
  * for [[org.scalacheck.Gen]] and [[com.thoughtworks.binding.Binding]] respectively.
  *
  * Although the effort of creating a DSL is minimized with the help of monads,
  * the syntax is still unsatisfactory.
  * Methods in `MonadOps` still seem like a duplicate of ordinary control flow,
  * and `for` comprehension supports only a limited set of functionality in comparison to ordinary control flows.
  * `if` / `while` / `try` and other block expressions cannot appear in the enumerator clause of `for` comprehension.
  *
  * == Enabling ordinary control flows in DSL via macros ==
  *
  * An idea to avoid inconsistency between domain-specific control flow and ordinary control flow is
  * converting ordinary control flow to domain-specific control flow at compiler time.
  *
  * For example, [[https://github.com/scala/scala-async scala.async]] provides a macro
  * to generate asynchronous control flow.
  * The users just wrap normal synchronous code in a [[scala.async]] block,
  * and it runs asynchronously.
  *
  * This approach can be generalized to any monadic data types.
  * [[https://github.com/ThoughtWorksInc/each ThoughtWorks Each]], [[http://monadless.io/ Monadless]]
  * and [[https://github.com/pelotom/effectful effectful]] are macros
  * that convert ordinary control flow to monadic control flow.
  *
  * For example, with the help of [[https://github.com/ThoughtWorksInc/each ThoughtWorks Each]],
  * [[https://github.com/ThoughtWorksInc/Binding.scala Binding.scala]] is used to create reactive HTML templating
  * from ordinary Scala control flow.
  *
  * == Delimited continuations ==
  *
  * Another generic interface of control flow is continuation,
  * which is known as
  * [[https://www.schoolofhaskell.com/user/dpiponi/the-mother-of-all-monads the mother of all monads]],
  * where control flows in specific domain can be supported by specific final result types of continuations.
  *
  * [[https://github.com/scala/scala-continuations scala-continuations]]
  * and [[https://github.com/qifun/stateless-future stateless-future]]
  * are two delimited continuation implementations.
  * Both projects can convert ordinary control flow to continuation-passing style closure chains at compiler time.
  *
  * For example, [[https://github.com/qifun/stateless-future-akka stateless-future-akka]],
  * based on `stateless-future`,
  * provides a special final result type for akka actors.
  * Unlike [[akka.actor.AbstractFSM]]'s inconsistent control flows, users can create complex finite-state machines
  * from simple ordinary control flows along with `stateless-future-akka`'s domain-specific keyword `nextMessage`.
  *
  * == Collaborative DSLs ==
  *
  * All the above approaches lack of the ability to collaborate with other DSLs.
  * Each of the above DSLs can be only exclusively enabled in a code block.
  * For example,
  * [[https://github.com/scala/scala-continuations scala-continuations]]
  * enables calls to `@cps` method in [[scala.util.continuations.reset]] blocks,
  * and [[https://github.com/ThoughtWorksInc/each ThoughtWorks Each]]
  * enables the magic `each` method for [[scalaz.Monad]] in [[com.thoughtworks.each.Monadic.monadic]] blocks.
  * It is impossible to enable both DSLs in one function.
  *
  * This [[https://github.com/ThoughtWorksInc/Dsl.scala Dsl.scala]] project resolves this problem.
  *
  * We also provide adapters to all the above kinds of DSLs.
  * Instead of switching different DSLs between different functions,
  * DSL users can use multiple DSLs together in one function,
  * by simply adding [[com.thoughtworks.dsl.compilerplugins.BangNotation our Scala compiler plug-in]].
  *
  * @example Suppose you want to create an [[https://en.wikipedia.org/wiki/Xorshift Xorshift]] random number generator.
  *
  *          The generated numbers should be stored in a lazily evaluated infinite [[scala.collection.immutable.Stream Stream]],
  *          which can be implemented as a recursive function that produce the next random number in each iteration,
  *          with the help of our built-in domain-specific keyword [[com.thoughtworks.dsl.keywords.Yield Yield]].
  *
  *          {{{
  *          import com.thoughtworks.dsl.Dsl.reset
  *          import com.thoughtworks.dsl.keywords.Yield
  *
  *          def xorshiftRandomGenerator(seed: Int): Stream[Int] = {
  *            val tmp1 = seed ^ (seed << 13)
  *            val tmp2 = tmp1 ^ (tmp1 >>> 17)
  *            val tmp3 = tmp2 ^ (tmp2 << 5)
  *            !Yield(tmp3)
  *            xorshiftRandomGenerator(tmp3)
  *          }: @reset
  *
  *          val myGenerator = xorshiftRandomGenerator(seed = 123)
  *
  *          myGenerator(0) should be(31682556)
  *          myGenerator(1) should be(-276305998)
  *          myGenerator(2) should be(2101636938)
  *          }}}
  *
  *          [[com.thoughtworks.dsl.keywords.Yield Yield]] is an keyword to produce a value
  *          for a lazily evaluated [[scala.collection.immutable.Stream Stream]].
  *          That is to say, [[scala.collection.immutable.Stream Stream]] is the domain
  *          where the DSL [[com.thoughtworks.dsl.keywords.Yield Yield]] can be used,
  *          which was interpreted like the `yield` keyword in C#, JavaScript or Python.
  *
  *          Note that the body of `xorshiftRandomGenerator` is annotated as `@reset`,
  *          which enables the [[Dsl.Keyword#unary_$bang !-notation]] in the code block.
  *
  *          Alternatively, you can also use the
  *          [[com.thoughtworks.dsl.compilerplugins.ResetEverywhere ResetEverywhere]] compiler plug-in,
  *          which enable [[Dsl.Keyword#unary_$bang !-notation]] for every methods and functions.
  *
  *          <hr/>
  *
  *          [[com.thoughtworks.dsl.keywords.Yield Yield]] and [[scala.collection.immutable.Stream Stream]]
  *          can be also used for logging.
  *
  *          Suppose you have a function to parse an JSON file,
  *          you can append log records to a [[scala.collection.immutable.Stream Stream]] during parsing.
  *
  *          {{{
  *          import com.thoughtworks.dsl.Dsl.!!
  *          import java.io._
  *          import scala.util.parsing.json._
  *
  *          def parseAndLog(jsonContent: String, defaultValue: JSONType): Stream[String] !! JSONType = _ {
  *            !Yield(s"I am going to parse the JSON text $jsonContent...")
  *            JSON.parseRaw(jsonContent) match {
  *              case Some(json) =>
  *                !Yield(s"Succeeded to parse $jsonContent")
  *                json
  *              case None =>
  *                !Yield(s"Failed to parse $jsonContent")
  *                defaultValue
  *            }
  *          }
  *          }}}
  *
  *          Since the function produces both a [[scala.util.parsing.json.JSONType JSONType]]
  *          and a [[scala.collection.immutable.Stream Stream]] of logs,
  *          the return type is now `Stream[String] !! JSONType`,
  *          where [[com.thoughtworks.dsl.Dsl.$bang$bang !!]] is
  *          an alias of continuation-passing style function marked as `@reset`,
  *          which enables the [[Dsl.Keyword#unary_$bang !-notation]] automatically.
  *
  *          {{{
  *          val logs = parseAndLog(""" { "key": "value" } """, JSONArray(Nil)) { json =>
  *            json should be(JSONObject(Map("key" -> "value")))
  *            Stream("done")
  *          }
  *
  *          logs should be(Stream("I am going to parse the JSON text  { \"key\": \"value\" } ...",
  *                                "Succeeded to parse  { \"key\": \"value\" } ",
  *                                "done"))
  *          }}}
  *
  *          <hr/>
  *
  *          [[com.thoughtworks.dsl.Dsl.$bang$bang !!]], or [[com.thoughtworks.dsl.Dsl.Continuation Continuation]],
  *          is the preferred approach to enable multiple domains in one function.
  *
  *          For example, you can create a function that
  *          lazily read each line of a [[java.io.BufferedReader BufferedReader]] to a [[Stream]],
  *          automatically close the [[java.io.BufferedReader BufferedReader]] after reading the last line,
  *          and finally return the total number of lines in the `Stream[String] !! Raii !! Int` domain.
  *
  *          {{{
  *          import com.thoughtworks.dsl.domains.Raii
  *          import com.thoughtworks.dsl.keywords.AutoClose
  *
  *          def readerToStream(createReader: () => BufferedReader): Stream[String] !! Raii !! Int = _ {
  *            val reader = !AutoClose(createReader())
  *
  *            def loop(lineNumber: Int): Stream[String] !! Raii !! Int = _ {
  *              reader.readLine() match {
  *                case null =>
  *                  lineNumber
  *                case line =>
  *                  !Yield(line)
  *                  !loop(lineNumber + 1)
  *              }
  *            }
  *
  *            !loop(0)
  *          }
  *          }}}
  *
  *          [[com.thoughtworks.dsl.domains.Raii Raii]] is a domain that supports many useful keywords:
  *           - [[com.thoughtworks.dsl.keywords.AutoClose AutoClose]] for resource management.
  *           - [[com.thoughtworks.dsl.keywords.Shift Shift]] for asynchronous programming.
  *           - [[com.thoughtworks.dsl.keywords.Fork Fork]] for creating multiple tasks in parallel.
  *
  *          `!loop(0)` is a shortcut of `!Shift(loop(0))`,
  *          because there is [[com.thoughtworks.dsl.domains.Raii#await an implicit conversion]]
  *          from `Stream[String] !! Raii !! Int` to [[com.thoughtworks.dsl.keywords.Shift Shift]] keyword,
  *          which is similar to the `await` keyword in JavaScript, Python or C#.
  *
  *          A type like `A !! B !! C` means a domain-specific value of type `C` in the domain of `A` and `B`.
  *          When `B` is [[com.thoughtworks.dsl.domains.Raii Raii]],
  *          a [[com.thoughtworks.dsl.domains.Raii.RaiiContinuationOps#run]] method is available,
  *          which can be used to register a callback function that handles the result of `Try[C]`.
  *
  *          {{{
  *          import scala.util.Success
  *
  *          var isClosed = false
  *          def createReader() = {
  *            new BufferedReader(new StringReader("line1\nline2\nline3")) {
  *              override def close() = {
  *                isClosed = true
  *              }
  *            }
  *          }
  *
  *          val stream = readerToStream(createReader _).run { result =>
  *            inside(result) {
  *              case Success(totalNumber) =>
  *                totalNumber should be(3)
  *            }
  *
  *            Stream.empty
  *          }
  *
  *          isClosed should be(false)
  *          stream should be(Stream("line1", "line2", "line3"))
  *          isClosed should be(true)
  *          }}}
  *
  *          If you don't need to collaborate to [[scala.collection.immutable.Stream Stream]] or other domains,
  *          you can use `TailRect[Unit] !! Raii !! A` or the alias [[com.thoughtworks.dsl.domains.Raii.Task]],
  *          as a higher-performance replacement of
  *          [[scala.concurrent.Future]], [[scalaz.concurrent.Task]] or [[monix.eval.Task]].
  *
  * @see [[Dsl]] for the guideline to create your custom DSL.
  * @see [[domains.scalaz]] for using [[Dsl.Keyword#unary_$bang !-notation]] with [[scalaz]].
  * @see [[domains.cats]] for using [[Dsl.Keyword#unary_$bang !-notation]] with [[cats]].
  *
  *
  */
package object dsl

package dsl {

  /** Contains built-in domain-specific [[com.thoughtworks.dsl.Dsl.Keyword Keyword]]s and their corresponding interpreters.
    *
    *
    */
  package object keywords
}

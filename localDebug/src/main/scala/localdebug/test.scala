package localdebug

import scala.concurrent.duration.Duration
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.given
import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.keywords.*, Match.+:
import com.thoughtworks.dsl.macros.Reset.Default.*

@main
def test = {
  val buffer = new StringBuffer
  def recoverFuture = Future {
    buffer.append("Oh")
  }
  def exceptionalFuture = Future[StringBuffer] {
    throw new IllegalStateException("No")
  }
  val reified = reify[Char => Future[StringBuffer]] {
    !Return(try {
      !Await(exceptionalFuture)
    } catch {
      case e: IllegalStateException =>
        !Await(recoverFuture)
        buffer.append(!Get[Char])
        buffer.append(e.getMessage)
    } finally {
      buffer.append("!")
    })
  }

  summon[
    reified.type <:<
      com.thoughtworks.dsl.keywords.Typed[com.thoughtworks.dsl.keywords.Suspend[
        com.thoughtworks.dsl.keywords.FlatMap[
          com.thoughtworks.dsl.keywords.TryCatchFinally[
            com.thoughtworks.dsl.keywords.Await[
              scala.concurrent.Future[StringBuffer]
            ],
            com.thoughtworks.dsl.keywords.Match.WithIndex[
              (0),
              com.thoughtworks.dsl.keywords.FlatMap[
                com.thoughtworks.dsl.keywords.Await[
                  scala.concurrent.Future[StringBuffer]
                ],
                com.thoughtworks.dsl.keywords.FlatMap[
                  com.thoughtworks.dsl.keywords.Get[Char],
                  com.thoughtworks.dsl.keywords.Pure[StringBuffer]
                ]
              ]
            ]
              +: Nothing,
            com.thoughtworks.dsl.keywords.Pure[Unit]
          ],
          com.thoughtworks.dsl.keywords.FlatMap[
            com.thoughtworks.dsl.keywords.Return[StringBuffer],
            com.thoughtworks.dsl.keywords.Pure[
              Char => concurrent.Future[StringBuffer]
            ]
          ]
        ]
      ], Char => concurrent.Future[StringBuffer]]
  ]

  def myFuture = summon[Dsl.Run[reified.type, Char => Future[
    StringBuffer
  ], Char => Future[StringBuffer]]](reified)

  // def myFuture = reset[Char => Future[StringBuffer]](!Return {
  //   try {
  //     !Await(exceptionalFuture)
  //   } catch {
  //     case e: IllegalStateException =>
  //       !Await(recoverFuture)
  //       buffer.append(!Get[Char])
  //       buffer.append(e.getMessage)
  //   } finally {
  //     buffer.append("!")
  //   }
  // })
  print(scala.concurrent.Await.result(myFuture(' '), Duration.Inf))
}

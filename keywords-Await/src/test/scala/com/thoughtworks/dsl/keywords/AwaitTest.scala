package com.thoughtworks.dsl
package keywords

import reset.{`*`, reify}
import Dsl.!!
import Dsl.Run
import Dsl.IsKeyword
import keywords._, Match._
import concurrent.ExecutionContext.Implicits.global
import concurrent.Future
import concurrent.duration._
import scala.language.dynamics
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.Inside
import scala.util.Failure
import scala.util.Success

class AwaitTest extends AsyncFreeSpec with Matchers with Inside {
  "div by zero" ignore {
    val future = *[Future] {
      !Pure(())
      !Suspend(() => Pure(0 / 0))
    }
    inside(future.value) {
      case Some(Failure(e)) =>
        e should be(an[ArithmeticException])
    }
  }
  "Suspend(div by zero)" ignore {
    val future = *[Future] {
      !Suspend(() => Pure(()))
      !Suspend(() => Pure(0 / 0))
    }
    inside(future.value) {
      case Some(Failure(e)) =>
        e should be(an[ArithmeticException])
    }
  }

  "Shift(div by zero)" ignore {
    val future = *[Future] {
      !Shift[Future[Int], Unit](_(()))
      !Suspend(() => Pure(0 / 0))
    }
    inside(future.value) {
      case Some(Failure(e)) =>
        e should be(an[ArithmeticException])
    }
  }

  "testReturnIf" in {
    val reified = reify {
      if (true) {
        !Return(!Await(Future(42)))
      }
      -1
    }
    reified.to[Future].map { _ should be(42) }
  }

  type Id[A] = A
  "testComprehension1" in {
    def inner1 = for {
      j <- Each(0 until 3)
    } yield 100 + j

    val ast1 = Await(Future(1)).flatMap { i =>
      inner1
    }
    summon[
      ast1.type
        <:<
          Dsl.For.Yield.FlatMap[
            Await[Future[Int]],
            Int,
            Dsl.For.Yield.Map[
              Each[Int],
              Int,
              Int
            ],
            Int
          ]
    ]

    *[Future] {
      (!Await(
        Each.ToView(ast1).to[Future]
      )).toVector should be(Vector(100, 101, 102))
    }
  }

  "testComprehension2" in {
    import Dsl._
    val inner2 = for {
      j <- Each(0 until 10)
    } yield 111
    summon[
      inner2.type
        <:<
          Dsl.For.Yield.Map[Each[Int], Int, Int]
    ]
    val ast2 = Await(Future(1)).flatMap { i =>
      inner2
    }
    summon[
      ast2.type
        <:<
          Dsl.For.Yield.FlatMap[
            Await[Future[Int]],
            Int,
            Dsl.For.Yield.Map[
              Each[Int],
              Int,
              Int
            ],
            Int
          ]
    ]
    succeed
  }

  "testComprehension3" in {
    import Dsl._
    val ast3 = Each.ToView.toKeyword(for {
      i <- Await(Future(1))
      j <- Each(0 until 10)
    } yield 111)
    summon[
      ast3.type
        <:<
          FlatMap[
            Await[Future[Int]],
            FlatMap[
              Each[Int],
              Pure[collection.View[Int]]
            ]
          ]
    ]
    succeed
  }

  "test1" in {
    val reified = reify[1](1)
    summon[reified.type <:< Typed[Pure[1], 1]]
    reified should be(1)
  }
  "test2" in {
    val reified = reify[1] { 1 }
    summon[reified.type <:< Typed[Pure[1], 1]]
    reified should be(1)

    val reified2 = reify[1](!reified)
    summon[reified2.type <:< Typed[_ <: Typed[Pure[1], 1], 1]]
    reified2 should be(1)
  }
  "test3" in {
    val reified = reify {}
    summon[reified.type <:< Typed[Pure[Unit], Unit]]
    reified should be(())
  }
  "test4" in {
    val reified = reify {
      !Await(Future(1L))
    }
    summon[reified.type <:< Typed[Await[Future[Long]], Long]]
    summon[Run[Typed[Await[Future[Long]], Long], Future[Long], Long]](reified)

    *[Future] {
      !Await(reified.to[Future]) should be(1L)
    }
  }

  "test5" in {
    val reified = reify {
      !Await(Future(!Await(Future(1L))))
    }
    summon[reified.type <:< Typed[FlatMap[Await[Future[Long]], Await[
      Future[Long]
    ]], Long]]
    summon[
      Run[
        FlatMap[Await[Future[Long]], Await[Future[Long]]],
        Future[Long],
        Long
      ]
    ]
    *[Future] {
      !Await(reified.to[Future]) should be(1L)
    }
  }

  "test6" in {
    val reified = reify {
      val i = !Await(Future(super.getClass.getSimpleName))
      !Await(Future(i))
    }
    summon[reified.type <:< Typed[
      FlatMap[Await[Future[String]], Await[Future[String]]],
      String
    ]]
    summon[
      Run[
        FlatMap[Await[Future[String]], Await[Future[String]]],
        Future[String],
        String
      ]
    ]
    *[Future] {
      !Await(reified.to[Future]) should be(super.getClass.getSimpleName)
    }
  }
  object Foo extends Dynamic {
    def applyDynamicNamed(method: String)(arg1: (String, Long)) = {
      arg1._2
    }
  }

  "test7" in {
    val reified = reify {
      val i = !Await(Future(body = this.Foo.d(j = 1L)))
      object A {
        def j = i + 200
      }
      !Await(Future(A.j))
    }
    summon[reified.type <:< Typed[FlatMap[Await[Future[Long]], Await[
      Future[Long]
    ]], Long]]
    summon[
      Run[
        FlatMap[Await[Future[Long]], Await[Future[Long]]],
        Future[Long],
        Long
      ]
    ]

    *[Future] {
      !Await(reified.to[Future]) should be(201L)
    }
  }

  "testReturn1" in {
    val reified = reify {
      (!Return(!Await(Future(())))): Unit
    }
    summon[
      reified.type <:<
        Typed[FlatMap[
          Await[Future[Unit]],
          FlatMap[Return[Unit], Pure[Unit]]
        ], Unit]
    ]

    *[Future] {
      !Await(reified.to[Future]) should be(())
    }
  }

  "testReturn2" in {
    val reified = reify {
      !Return(!Await(Future(())))
    }
    // summon[reified.type <:<
    //   Typed[
    //    FlatMap[
    //      Await[Future[Unit]]
    //     , Unit,Pure[Nothing]]
    //   , Nothing]
    // ]
    *[Future] {
      !Await(reified.to[Future]) should be(())
    }
  }

  inline val x = 42

  "testInline" in {
    val rr = reify {
      reify[x.type] {
        x
      }
    }

    summon[
      rr.type <:<
        Typed[
          Pure[
            Typed[Pure[42], 42]
          ],
          Typed[Pure[42], 42]
        ]
    ]

    rr.to[Id].to[Id] should be(42)

  }

  "testIf" in {
    val generator = reify {
      if (false) {
        !Yield(0)
      }
      if (true) {
        !Yield(1)
      }
      if { !Yield(2); false } then {
        !Yield(3)
      } else {
        !Yield(4)
      }
      LazyList.empty[Int]
    }

    generator.to[Id] should be(Seq(1, 2, 4))
  }

  "testMatch" in {
    def loop(i: Int): LazyList[Int] = {
      val reified = reify {
        (i: Int) match {
          case 100 =>
            LazyList.empty
          case _ =>
            !Yield(i)
            loop(i + 1)
        }
      }
      summon[
        reified.type <:<
          Typed[FlatMap[
            Pure[Int],
            Match.WithIndex[(0), Pure[LazyList[Nothing]]]
              +:
                Match.WithIndex[(1), FlatMap[Yield[Int], Pure[
                  LazyList[Int]
                ]]]
                +: Nothing
          ], LazyList[Int]]
      ]
      reified.as[LazyList[Int]]
    }

    loop(90) should be(LazyList(90, 91, 92, 93, 94, 95, 96, 97, 98, 99))
  }

  "testTryCatchFinally" in {
    def zeroDivZero = 0 / 0
    val reified = reify {
      try {
        zeroDivZero
        !Await(Future("unreachable code"))
      } catch {
        case _: ArithmeticException =>
          s"Cannot divide ${!Await(Future(0))} by ${!Await(Future(0))}"
      } finally {}
    }

    summon[
      reified.type <:<
        Typed[
          keywords.TryCatchFinally[Suspend[
            Await[Future[String]]
          ], Match.WithIndex[0, FlatMap[
            Await[Future[Int]],
            FlatMap[Await[
              Future[Int]
            ], Pure[String]]
          ]]
            +: Nothing, Suspend[Pure[Unit]]],
          String
        ]
    ]

    val fs = reified.as[Future[String]]
    fs.map(_ should be("Cannot divide 0 by 0"))
  }

  "testTryFinally" in {
    def zeroDivZero = 0 / 0
    val reified = reify {
      try {
        zeroDivZero
        !Await(Future(-41))
      } finally {}
    }

    summon[
      reified.type <:<
        Typed[keywords.TryFinally[Await[Future[Int]], Pure[Unit]], Int]
    ]

    reified.as[Future[Int]].transform { t =>
      inside(t) { case Failure(e) =>
        Success(e should be(an[ArithmeticException]))
      }
    }
  }

  "testTryCatch" in {
    val reified = reify {
      try {
        s"Division result: ${!Await(Future(3)) / !Await(Future(0))}"
      } catch {
        case e: ArithmeticException =>
          s"Cannot divide ${!Await(Future(3))} by ${!Await(Future(0))}"
      }
    }

    summon[
      reified.type <:<
        Typed[keywords.TryCatch[Suspend[
          FlatMap[
            Await[Future[Int]],
            FlatMap[Await[
              Future[Int]
            ], Pure[String]]
          ]
        ], Match.WithIndex[0, FlatMap[
          Await[Future[Int]],
          FlatMap[Await[
            Future[Int]
          ], Pure[String]]
        ]]
          +: Nothing], String]
    ]
    reified.to[Future].map {
      _ should be("Cannot divide 3 by 0")
    }
  }

  "testWhile" in {
    val reified = reify {
      var i = 0L
      while (!Await(Future(i < 3))) {
        i += !Await(Future(1))
      }
      i
    }
    summon[
      reified.type <:<
        Typed[FlatMap[While[
          Await[Future[Boolean]],
          FlatMap[Await[
            Future[Int]
          ], Pure[Unit]]
        ], Pure[Long]], Long]
    ]
    *[Future] {
      !Await(reified.to[Future]) should be(3L)
      !Await(reified.as[Double => Future[Long]].apply(1.0)) should be(3L)
    }
  }

  "testAssign" in {
    object A {
      var a = "a"
    }
    val reified = reify {
      (!Await(Future(A))).a = !Await(Future("x"))
      A.a.toUpperCase
    }
    summon[
      reified.type <:<
        com.thoughtworks.dsl.keywords.Typed[
          com.thoughtworks.dsl.keywords.FlatMap[
            com.thoughtworks.dsl.keywords.Await[scala.concurrent.Future[A.type]]
          , 
            com.thoughtworks.dsl.keywords.FlatMap[
              com.thoughtworks.dsl.keywords.Await[scala.concurrent.Future[String]]
            , com.thoughtworks.dsl.keywords.Pure[String]]
          ]
        , String]    ]
    reified.to[Future].map(_ should be("X"))
  }

  "testTyped" in {
    val reified = reify {
      val i = !Await(Future("x"): Future[CharSequence])
      val r = !Await(Future(i)): AnyRef
      r
    }
    summon[
      reified.type <:<
        com.thoughtworks.dsl.keywords.Typed[
          com.thoughtworks.dsl.keywords.FlatMap[
            com.thoughtworks.dsl.keywords.Await[concurrent.Future[CharSequence]]
          , 
            com.thoughtworks.dsl.keywords.FlatMap[
              com.thoughtworks.dsl.keywords.Await[
                scala.concurrent.Future[CharSequence]
              ]
            , com.thoughtworks.dsl.keywords.Pure[Object]]
          ],
        AnyRef
    ]]
    reified.to[Future].map(_ should be("x"))
  }

  "testPartialFunction" in {
    val reified = reify {
      Future[PartialFunction[Int, String]] {
        case 1          => "1"
        case x if x < 0 => "negative"
        case _          => "default"
      }
    }

    summon[
      reified.type <:<
        Typed[Pure[
          Future[PartialFunction[Int, String]]
        ], Future[PartialFunction[Int, String]]]
    ]
    type Id[A] = A
    reified.to[Id].map {
      _ should be(a[PartialFunction[_, _]])
    }
  }

  "testClosure" in {
    val reified = reify {
      val id = !Await(Future(identity[Int] _))
      val e = id.equals _
      !Await(Future(e))
    }
    summon[
      reified.type <:<
        com.thoughtworks.dsl.keywords.Typed[
          com.thoughtworks.dsl.keywords.FlatMap[
            com.thoughtworks.dsl.keywords.Await[scala.concurrent.Future[Int => Int]]
          , 
            com.thoughtworks.dsl.keywords.Await[
              scala.concurrent.Future[Any => Boolean]
            ]
          ]
        , Any => Boolean]    ]
    reified.to[Future].map {
      _ should be(a[Function1[_, _]])
    }
  }

  "testBangClosure" in {
    val reified = reify {
      val e = (!Await(Future(identity[Int] _))).equals _
      !Await(Future(e))
    }
    summon[
      reified.type <:<
        com.thoughtworks.dsl.keywords.Typed[
          com.thoughtworks.dsl.keywords.FlatMap[
            com.thoughtworks.dsl.keywords.FlatMap[
              com.thoughtworks.dsl.keywords.Await[scala.concurrent.Future[Int => Int]]
            , com.thoughtworks.dsl.keywords.Pure[Any => Boolean]]
          , 
            com.thoughtworks.dsl.keywords.Await[
              scala.concurrent.Future[Any => Boolean]
            ]
          ]
        , Any => Boolean]    ]
    reified.to[Future].map {
      _ should be(a[Function1[_, _]])
    }
  }

  "testClosure2" in {
    val reified = reify {
      val e = (!Await(Future((i: Int) => i))).equals _
      !Await(Future(e))
    }
    summon[
      reified.type <:<
         com.thoughtworks.dsl.keywords.Typed[
            com.thoughtworks.dsl.keywords.FlatMap[
              com.thoughtworks.dsl.keywords.FlatMap[
                com.thoughtworks.dsl.keywords.Await[scala.concurrent.Future[Int => Int]]
              , com.thoughtworks.dsl.keywords.Pure[Any => Boolean]]
            , 
              com.thoughtworks.dsl.keywords.Await[
                scala.concurrent.Future[Any => Boolean]
              ]
            ]
          , Any => Boolean]
        
    ]
    reified.to[Future].map {
      _ should be(a[Function1[_, _]])
    }
  }

}

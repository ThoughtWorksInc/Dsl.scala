package com.thoughtworks.dsl

import bangnotation._
import Dsl.Run
import Dsl.!!
import Dsl.AsKeyword
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
      j <- In(0 until 3)
    } yield 100 + j
    summon[Run[
      FlatMap[
        In[Int],
        Int,
        Pure[Int]
      ],
      Vector[Int],
      Int
    ]]

    summon[
      AsKeyword.FromKeyword[
        FlatMap[
          In[Int],
          Int,
          Pure[Int]
        ],
        Int
      ]
    ]

    val ast1 = Await(Future(1)).flatMap { i =>
      inner1
    }
    summon[
      ast1.type
        <:<
          FlatMap[
            Await[Int],
            Int,
            FlatMap[
              In[Int],
              Int,
              Pure[Int]
            ]
          ]
    ]

    def forall[A] = {
      summon[Run[
        FlatMap[
          Await[Int],
          Int,
          FlatMap[
            In[Int],
            Int,
            Pure[Int]
          ]
        ],
        Future[A] !! Vector[Int],
        Int
      ]]
    }

    *[Future] {
      !Await(ast1.as[Future[Vector[Int]] !! Vector[Int]].apply(Future.successful)) should be(Vector(100, 101, 102))
      !Await(ast1.as[Future[Array[Int]] !! Array[Int]].apply(Future.successful)) should be(Array(100, 101, 102))
    }
  }

  "testComprehension2" in {
    val inner2 = for {
      j <- In(0 until 10)
    } yield 111
    summon[
      inner2.type
        <:<
          FlatMap[
            In[Int],
            Int,
            Pure[Int]
          ]
    ]
    val ast2 = Await(Future(1)).flatMap { i =>
      inner2
    }
    summon[
      ast2.type
        <:<
          FlatMap[
            Await[Int],
            Int,
            FlatMap[
              In[Int],
              Int,
              Pure[Int]
            ]
          ]
    ]
    succeed
  }

  "testComprehension3" in {
    val ast3 = for {
      i <- Await(Future(1))
      j <- In(0 until 10)
    } yield 111
    summon[
      ast3.type
        <:<
          FlatMap[
            Await[Int],
            Int,
            FlatMap[
              In[Int],
              Int,
              Pure[Int]
            ]
          ]
    ]
    succeed
  }

  "test1" in {
    val refied = reify[1](1)
    summon[refied.type <:< Typed[Pure[1], 1]]
    refied should be(1)
  }
  "test2" in {
    val refied = reify[1] { 1 }
    summon[refied.type <:< Typed[Pure[1], 1]]
    refied should be(1)

    val refied2 = reify[1](!refied)
    summon[refied2.type <:< Typed[Typed[Pure[1], 1], 1]]
    refied2 should be(1)
  }
  "test3" in {
    val refied = reify {}
    summon[refied.type <:< Typed[Pure[Unit], Unit]]
    refied should be(())
  }
  "test4" in {
    val refied = reify {
      !Await(Future(1L))
    }
    summon[refied.type <:< Typed[Await[Long], Long]]
    summon[Run[Typed[Await[Long], Long], Future[Long], Long]](refied)

    *[Future] {
      !Await(refied.to[Future]) should be(1L)
    }
  }

  "test5" in {
    val refied = reify {
      !Await(Future(!Await(Future(1L))))
    }
    summon[refied.type <:< Typed[FlatMap[Await[Long], Long, Await[Long]], Long]]
    summon[
      Run[
        FlatMap[Await[Long], Long, Await[Long]],
        Future[Long],
        Long
      ]
    ]
    *[Future] {
      !Await(refied.to[Future]) should be(1L)
    }
  }

  "test6" in {
    val refied = reify {
      val i = !Await(Future(super.getClass.getSimpleName))
      !Await(Future(i))
    }
    summon[refied.type <:< Typed[FlatMap[Await[String], String, Await[String]], String]]
    summon[
      Run[
        FlatMap[Await[String], String, Await[String]],
        Future[String],
        String
      ]
    ]
    *[Future] {
      !Await(refied.to[Future]) should be(super.getClass.getSimpleName)
    }
  }
  object Foo extends Dynamic {
    def applyDynamicNamed(method: String)(arg1: (String, Long)) = {
      arg1._2
    }
  }

  "test7" in {
    val refied = reify {
      val i = !Await(Future(body = this.Foo.d(j = 1L)))
      object A {
        def j = i + 200
      }
      !Await(Future(A.j))
    }
    summon[refied.type <:< Typed[FlatMap[Await[Long], Long, Await[Long]], Long]]
    summon[
      Run[
        FlatMap[Await[Long], Long, Await[Long]],
        Future[Long],
        Long
      ]
    ]

    *[Future] {
      !Await(refied.to[Future]) should be(201L)
    }
  }

  "testReturn1" in {
    val refied = reify {
      (!Return(!Await(Future(())))): Unit
    }
    summon[
      refied.type <:<
        com.thoughtworks.dsl.keywords.Typed[FlatMap[
          Await[Unit],
          Unit,
          FlatMap[Return[Unit], Nothing, Pure[Unit]]
        ], Unit]
    ]

    *[Future] {
      !Await(refied.to[Future]) should be(())
    }
  }

  "testReturn2" in {
    val refied = reify {
      !Return(!Await(Future(())))
    }
    // summon[refied.type <:<
    //   com.thoughtworks.dsl.keywords.Typed[
    //    FlatMap[
    //      Await[Unit]
    //     , Unit,Pure[Nothing]]
    //   , Nothing]
    // ]
    *[Future] {
      !Await(refied.to[Future]) should be(())
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
        com.thoughtworks.dsl.keywords.Typed[
          Pure[
            com.thoughtworks.dsl.keywords.Typed[Pure[42], 42]
          ],
          com.thoughtworks.dsl.keywords.Typed[Pure[42], 42]
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
          com.thoughtworks.dsl.keywords.Typed[FlatMap[
            Pure[Int],
            Int,
            Match.WithIndex[(0), Pure[LazyList[Nothing]]]
              +:
                Match.WithIndex[(1), FlatMap[Yield[Int], Unit, Pure[LazyList[Int]]]]
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
        com.thoughtworks.dsl.keywords.Typed[
          TryCatchFinally[Suspend[
            Await[String]
          ], Match.WithIndex[0, FlatMap[
            Await[Int],
            Int,
            FlatMap[Await[
              Int
            ], Int, Pure[String]]
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
        com.thoughtworks.dsl.keywords.Typed[TryFinally[Suspend[
          Await[Int]
        ], Suspend[Pure[Unit]]], Int]
    ]

    reified.as[Future[Int]].transform { t =>
      inside(t) { case Failure(e) =>
        Success(e should be(an[ArithmeticException]))
      }
    }
  }

  "testTryCatch" in {
    val refied = reify {
      try {
        s"Division result: ${!Await(Future(3)) / !Await(Future(0))}"
      } catch {
        case e: ArithmeticException =>
          s"Cannot divide ${!Await(Future(3))} by ${!Await(Future(0))}"
      }
    }

    summon[
      refied.type <:<
        com.thoughtworks.dsl.keywords.Typed[TryCatch[Suspend[
          FlatMap[
            Await[Int],
            Int,
            FlatMap[Await[
              Int
            ], Int, Pure[String]]
          ]
        ], Match.WithIndex[0, FlatMap[
          Await[Int],
          Int,
          FlatMap[Await[
            Int
          ], Int, Pure[String]]
        ]]
          +: Nothing], String]
    ]
    refied.to[Future].map {
      _ should be("Cannot divide 3 by 0")
    }
  }

  "testWhile" in {
    val refied = reify {
      var i = 0L
      while (!Await(Future(i < 3))) {
        i += !Await(Future(1))
      }
      i
    }
    summon[
      refied.type <:<
        com.thoughtworks.dsl.keywords.Typed[FlatMap[While[
          Suspend[
            Await[Boolean]
          ],
          Suspend[
            FlatMap[Await[
              Int
            ], Int, Pure[Unit]]
          ]
        ], Unit, Pure[Long]], Long]
    ]
    *[Future] {
      !Await(refied.to[Future]) should be(3L)
      !Await(refied.as[Double => Future[Long]].apply(1.0)) should be(3L)
    }
  }

  "testAssign" in {
    object A {
      var a = "a"
    }
    val refied = reify {
      (!Await(Future(A))).a = !Await(Future("x"))
      A.a.toUpperCase
    }
    summon[
      refied.type <:<
        com.thoughtworks.dsl.keywords.Typed[FlatMap[
          Await[A.type],
          A.type,
          FlatMap[
            Await[String],
            String,
            Pure[String]
          ]
        ], String]
    ]
    refied.to[Future].map(_ should be("X"))
  }

  "testTyped" in {
    val refied = reify {
      val i = !Await(Future("x"): Future[CharSequence])
      val r = !Await(Future(i)): AnyRef
      r
    }
    summon[
      refied.type <:<
        com.thoughtworks.dsl.keywords.Typed[FlatMap[
          Await[CharSequence],
          CharSequence,
          FlatMap[Await[
            CharSequence
          ], CharSequence, Pure[Object]]
        ], Object]
    ]
    refied.to[Future].map(_ should be("x"))
  }

  "testPartialFunction" in {
    val refied = reify {
      Future[PartialFunction[Int, String]] {
        case 1          => "1"
        case x if x < 0 => "negative"
        case _          => "default"
      }
    }

    summon[
      refied.type <:<
        com.thoughtworks.dsl.keywords.Typed[Pure[
          Future[PartialFunction[Int, String]]
        ], Future[PartialFunction[Int, String]]]
    ]
    type Id[A] = A
    refied.to[Id].map {
      _ should be(a[PartialFunction[_, _]])
    }
  }

  "testClosure" in {
    val refied = reify {
      val id = !Await(Future(identity[Int] _))
      val e = id.equals _
      !Await(Future(e))
    }
    summon[
      refied.type <:<
        com.thoughtworks.dsl.keywords.Typed[FlatMap[
          Await[Int => Int],
          Int => Int,
          Await[Any => Boolean]
        ], Any => Boolean]
    ]
    refied.to[Future].map {
      _ should be(a[Function1[_, _]])
    }
  }

  "testBangClosure" in {
    val refied = reify {
      val e = (!Await(Future(identity[Int] _))).equals _
      !Await(Future(e))
    }
    summon[
      refied.type <:<
        com.thoughtworks.dsl.keywords.Typed[FlatMap[
          FlatMap[Await[Int => Int], Int => Int, Pure[Any => Boolean]],
          Any => Boolean,
          Await[Any => Boolean]
        ], Any => Boolean]
    ]
    refied.to[Future].map {
      _ should be(a[Function1[_, _]])
    }
  }

  "testClosure2" in {
    val refied = reify {
      val e = (!Await(Future((i: Int) => i))).equals _
      !Await(Future(e))
    }
    summon[
      refied.type <:<
        com.thoughtworks.dsl.keywords.Typed[FlatMap[
          FlatMap[Await[Int => Int], Int => Int, Pure[Any => Boolean]],
          Any => Boolean,
          Await[Any => Boolean]
        ], Any => Boolean]
    ]
    refied.to[Future].map {
      _ should be(a[Function1[_, _]])
    }
  }

}

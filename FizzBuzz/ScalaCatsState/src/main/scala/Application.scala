import cats.data.State
import scala.annotation.tailrec

object Application {

    val fizz: State[Int, Option[String]] = {
        State.inspect(s => if (s%3==0) Some("Fizz") else None)
    }

    val buzz: State[Int, Option[String]] = {
        State.inspect(s => if (s%5==0) Some("Buzz") else None)
    }

    val number: State[Int, Option[String]] = {
        State.inspect(s => Some(s.toString))
    }

    val increment: State[Int, Unit] = {
        State.modify[Int](s => s + 1)
    }

    val fizzbuzz: State[Int, Option[String]] = for {
        f <- fizz
        b <- buzz
        n <- number
        _ <- increment
    } yield {
        (f, b, n) match {
            case (None, None, None) => None
            case (None, None, Some(x)) => Some(x)
            case (Some(x), None, _) => Some(x)
            case (None, Some(x), _) => Some(x)
            case (Some(x), Some(y), _) => Some(x + y)

        }
    }

    def repeat[S](m: State[S, Option[String]], count: Int): State[S, Option[String]] = {
        @tailrec
        def inner(m: State[S, Option[String]], count: Int, acc: State[S, Option[String]]): State[S, Option[String]] = {
            if (count <= 0) acc
            else {
                val next = for {
                    a <- acc
                    b <- m
                }   yield a.flatMap(x => b.map(y => s"${x}, ${y}"))
                inner(m, count-1, next)
            }
        }

        inner(m, count-1, m)
    }


    def double[S](m: State[S, Option[String]]):State[S, Option[String]] = {
        for {
            a <- m
            b <- m
        } yield a.flatMap(x => b.map(y => s"${x}, ${y}"))
    }

    def main(args: Array[String]): Unit = {
        val stuff = repeat(fizzbuzz, 15)
        val thing = double(stuff)

        stuff.runA(1).value.foreach(println)
        println("=============")
        thing.runA(1).value.foreach(println)

    }
}
package scala2Advanced.lectures.afp

import scala.util.Random

object PartialFunctions extends App {
  val aFunction = (x: Int) => x + 1

  val aFussyFunction = (x: Int) => x match {
    case 1 => 42
    case 2 => 3
    case 5 => 0
  }
  val aPartialFunction: PartialFunction[Int, Int] = {
    case 1 => 42
    case 2 => 3
    case 5 => 0
  }
//  println(aPartialFunction.isDefinedAt(55))
//  println(aFussyFunction(2))

  val lifted = aPartialFunction.lift // Int => Option[Int]
//  println(lifted(2))
//  println(lifted(55))

  val chain = aPartialFunction.orElse[Int, Int] {
    case 67 => 2
  }
//  println(chain(67))

  val aMappedlist = List(1, 2, 3).map {
    case 1 => 5
    case 2 => 7
    case 3 => 1000 - 7
  }

  // construct a PF instance (anonymous class)
  trait MyPartialFunction[A, B] {
    def apply(x: A): B

  }

  val myPartialFunction: MyPartialFunction[Int, Int] = {
    case 2 => 5
    case 4 => 7
  }
//  println(myPartialFunction(2))

//  scala.io.Source.stdin.getLines().foreach(_ => {
//    val random = new Random(System.nanoTime())
//    val randomInt = random.nextInt(3)
//    val botResponse: PartialFunction[Int, String] = {
//      case 0 => "Hi"
//      case 1 => "Yes"
//      case 2 => "Thanks!"
//    }
//    println(botResponse(randomInt))
//  }
//  )
  val set = Set(1, 2, 3)
  println(set(2))
}

package scala2Essentials.lectures.part1basics.p3func

object PatternMatching extends App {
  case class Person(name: String, age: Int)

  val bob = Person("Bob", 20)

  def greeting(person: Person): String = {
    person match {
      case Person(n, a) if a < 21 => s"Hi, my name is $n"
      case Person(n, a) => s"my name is $n, and i am $a years old"
      case _ => "I don't know who I am"
    }
  }

  trait Expr {

    override def toString: String = {
      this match {
        case Sum(e1, e2) => s"$e1 + $e2"
        case Prod(Sum(e1, e2), e3) => s"(${Sum(e1, e2)}) * $e3"
        case Prod(e3, Sum(e1, e2)) => s"$e3 * (${Sum(e1, e2)})"
        case Prod(Sum(e1, e2), Sum(e3, e4)) => s"(${Sum(e1, e2)}) * (${Sum(e3, e4)})"
        case Prod(e1, e2) => s"$e1 * $e2"
        case Number(n) => s"$n"

      }
    }
  }

  case class Number(n: Int) extends Expr

  case class Sum(e1: Expr, e2: Expr) extends Expr

  case class Prod(e1: Expr, e2: Expr) extends Expr

  val hui = Prod(Number(2), Sum(Number(3), Number(6)))
  println(hui)


}

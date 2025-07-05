package scala2Advanced.lectures

object PatternMatchingAdvanced extends App {
  val numbers = List(1)
  val description = numbers match {
    case head :: Nil => println(s"the only element is $head")
    case _ =>
  }


class Person(val name: String, val age: Int,val gender: Boolean)

object Person {
  def unapply(person: Person): Option[(String, Int, Boolean)] = Some((person.name, person.age, person.gender))
  def unapply(age: Int): Option[String] = Some(if (age < 21) "minor" else "major")
}
  val bob = new Person("bob", 22, true)
  val greeting = bob match {
    case Person(n, a, g) => s"Hi, my name is $n and I am $a yo"
  }
  val legalStatus = 22 match {
    case Person(status) => s"My legal status is $status"
  }

  println(greeting)
  println(legalStatus)

//  def unapply(t: (Boolean, Boolean)):
  val n: Int = 45
  val mathProperty = (n < 10, n % 2 == 0) match {
    case (true, false) => "single digit"
    case (false, true) => "even number"
    case (true, true) => "sd and even"
    case _ => "no property"
  }

}

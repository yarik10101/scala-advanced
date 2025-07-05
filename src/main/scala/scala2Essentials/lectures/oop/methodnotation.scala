package scala2Essentials.lectures.oop

object methodnotation extends App {
  class Person(val name: String, favoriteMovie: String, val age: Int = 0) {
    def likes(movie: String): Boolean = movie == favoriteMovie


    def +(person: Person): String = s"${this.name} is hanging out with ${person.name}"

    def +(str: String): Person = new Person(s"$name ($str)", favoriteMovie)

    def unary_! : Person = this

    def isAlive(): Boolean = true

    def apply(): String = s"$name $favoriteMovie"

    def unary_+ : Person = new Person(s"$name", favoriteMovie, age + 1)

    def learns(string: String): String = s"$name learns $string"
    def learnsScala(): String = mary learns "scala"
  }

  val mary = new Person("Mary", "Inception")
  println(mary.likes("Inception"))
  println(mary likes "Inception")

  val tom = new Person("Tom", "Fight Club")
  println(mary.+(tom))

  val x = -1
  val y = 1.unary_-

  !mary
  mary.unary_!


  mary.apply()
  mary()

  println(mary + "huj")
  val negro = new Person("Negro", "Jango Unchained")
  println((+(+negro)).age)

  println(mary.learnsScala)

}



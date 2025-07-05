package scala2Essentials.lectures.part2oop

object traits extends App {

  abstract class Animal {
    val creatureType: String
    def eat: Unit
  }
  class Dog extends Animal {
    val creatureType: String = "Dog"
    def eat: Unit = println("crunch")
  }

}

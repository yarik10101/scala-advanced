package scala2Essentials.lections

object Generics extends App {

  class MyList[+A] {
    def add[B >: A](element: B): MyList[B] = ???
    /*
    A = Cat
    B = Animal
     */
  }
  class MyMap[key, Value]
  val listOfIntegers = new MyList[Int]
  val listOfStrings = new MyList[String]

  object MyList {
    def empty[A]: MyList[A] = ???
  }
  val emptyListOfIntegers = MyList.empty[Int]

  class Animal
  class Cat extends Animal
  class Dog extends Animal

  class CovariantList[+A]
  val animal: Animal = new Cat
  val animalList: CovariantList[Animal] = new CovariantList[Cat]

  class InvariantList[A]
  val invariantAnimalList: InvariantList[Animal] = new InvariantList[Animal]

  class Trainer[-A]
  val trainer: Trainer[Cat] = new Trainer[Animal]

  class Cage[A <: Animal](animal: A)
  val cage = new Cage(new Dog)

//  class Car
//  val newCage = new Cage(new Car)

}

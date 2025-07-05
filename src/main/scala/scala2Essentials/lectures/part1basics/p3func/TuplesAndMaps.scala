package scala2Essentials.lectures.part1basics.p3func

//import exercises.{MyList, MyListNotEmpty}

import scala.annotation.tailrec


case class Person(name: String, friends: List[String]) {


  def friendFunction(newFriend: String): Person = {
    Person(name, newFriend :: friends)
  }

  def unfriendFunction(notFriend: String): Person = {
    Person(name, remove(friends, notFriend))
  }

  def friendCount(): Int = {
    friends.length
  }

  def remove(list: List[String], elem: String): List[String] = {
    if (elem == list.head) list.tail
    else list.head :: remove(list.tail, elem)
  }
}

object TuplesAndMaps extends App {
  val Sasha = Person("Sasha", List("Yarik", "Vanya", "Ilya", "Egor", "Billy", "Yrik"))
  val Yarik = Person("Yarik", List("Sasha", "Vanya", "Yrik", "Ilya", "BoSin"))
  val Vanya = Person("Vanya", List("Sasha", "Yarik"))
  val Ilya = Person("Ilya", List("Sasha", "Yarik"))
  val BoSin = Person("BoSin", List("Yarik"))
  val Egor = Person("Egor", List("Sasha", "Yrik"))
  val Alik = Person("Alik", List())
  val Billy = Person("Billy", List("Sasha"))
  val Yrik = Person("Yrik", List("Yarik", "Egor", "Sasha"))

  val socialNetwork: Map[String, Person] = Map("Sasha" -> Sasha, "Yarik" -> Yarik, "Ilya" -> Ilya, "Vanya" -> Vanya, "BoSin" -> BoSin, "Egor" -> Egor, "Alik" -> Alik, "Billy" -> Billy, "Yrik" -> Yrik)

  def add(network: Map[String, Person], person: Person): Map[String, Person] = {
    val newPerson = person.name -> person
    network + newPerson
  }

  def remove(network: Map[String, Person], person: Person): Map[String, Person] = {
    network.map(x => x._1 -> Person(x._2.name, x._2.friends.filter(x => x != person.name))).filter(x => x._1 != person.name)
  }

  def friendMap(network: Map[String, Person], person: String, friend: String): Map[String, Person] = {
    val p1 = network(person)
    val p2 = network(friend)
    val p1updated = p1.friendFunction(friend)
    val p2updated = p2.friendFunction(person)
    network + (person -> p1updated) + (friend -> p2updated)
  }

  def unfriendMap(network: Map[String, Person], person: String, notFriend: String): Map[String, Person] = {
    val p1 = network(person)
    val p2 = network(notFriend)
    val p1updated = p1.unfriendFunction(notFriend)
    val p2updated = p2.unfriendFunction(person)
    network + (person -> p1updated) + (notFriend -> p2updated)
  }

  def mostFriends(network: Map[String, Person]): List[Person] = {
    //    network.map(x => x._1 -> x._2.friends.length)
    @tailrec
    def theMost(network: Map[String, Person], acc: Int = 0): Int = {
      if (network.head._2.friends.length > acc && network.tail.nonEmpty) theMost(network.tail, network.toList.head._2.friends.length)
      else if (network.head._2.friends.length < acc && network.tail.nonEmpty) theMost(network.tail, acc)
      else if (network.head._2.friends.length > acc && network.tail.isEmpty) network.head._2.friends.length
      else acc
    }

    val theMostVal = theMost(network)
    network.filter(x => x._2.friends.length == theMostVal).values.toList
  }

  def noFriendsCount(network: Map[String, Person], acc: Int = 0): Int = {
    if (network.isEmpty) acc
    else if (network.head._2.friends.isEmpty) noFriendsCount(network.tail, acc + 1)
    else  noFriendsCount(network.tail, acc)
  }
  def socialConnection(p1: Person, p2: Person): Boolean = {
      @tailrec
      def inDirectHelper(p1: Person, acc: Set[String] = p1.friends.toSet): Set[String] = {
        println(acc)
        if (acc != acc.flatMap(x => socialNetwork(x).friends.toSet[String])) inDirectHelper(p1, acc.flatMap(x => socialNetwork(x).friends.toSet[String]))
        else acc
      }
      inDirectHelper(p1).contains(p2.name)
  }

//  println(noFriendsCount(add(socialNetwork, Vanya)))
//  println(Sasha.friends.toSet[String].flatMap(x => socialNetwork(x).friends.toSet[String]))
  println(socialConnection(BoSin, Billy))
  println(remove(socialNetwork, Sasha))
}
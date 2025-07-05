package scala2Essentials.lectures.part1basics.p3func

import java.util

trait Maybe[+T] {
  def map[B >: T](f: T => B): Maybe[B]

  def flatMap[B >: T](f: T => Maybe[B]): Maybe[B]

  def filter(f: T => Boolean): Maybe[T]
}

case object MaybeNot extends Maybe[Nothing] {
  override def map[B >: Nothing](f: Nothing => B): Maybe[B] = this.map(f)

  override def flatMap[B >: Nothing](f: Nothing => Maybe[B]): Maybe[B] = this.flatMap(f)

  override def filter(f: Nothing => Boolean): Maybe[Nothing] = this
}

case class Just[+T](head: T) extends Maybe[T] {
  override def map[B >: T](f: T => B): Maybe[B] = new Just[B](f(head))

  override def flatMap[B >: T](f: T => Maybe[B]): Maybe[B] = f(head)

  override def filter(f: T => Boolean): Maybe[T] = {
    if (f(head)) this
    else MaybeNot
  }
}

object MapFlatmapFilterFor extends App {

  val numbers: List[Int] = List(1, 2, 3, 4)
  val chars: List[String] = List("a", "b", "c", "d")

  //  def combine[A,B](list1: List[A], list2: List[B], acc: List[String] = List()):List[String] = {
  //    if (list2.isEmpty)  acc
  //    else combine(list1, list2.tail, acc ++ list1.map(x => x + s"${list2.head}"))
  //  }
  //  println(combine(numbers, chars))
  def combineNoRec[A, B](list1: List[A], list2: List[B]): List[String] = {
    list2.flatMap(x => list1.map(y => y + s"$x"))
  }

  println(combineNoRec(numbers, chars))


}

package scala2Advanced.exercises

import scala.annotation.tailrec

trait MySet[A] extends (A => Boolean) {

  def contains(elem: A): Boolean

  def +(elem: A): MySet[A]

  def ++(anotherSet: MySet[A]): MySet[A]

  def map[B](f: A => B): MySet[B]

  def flatMap[B](f: A => MySet[B]): MySet[B]

  def filter(predicate: A => Boolean): MySet[A]

  def foreach(f: A => Unit): Unit

  def head: A

  def tail: MySet[A]

  def remove(elem: A, acc: MySet[A]): MySet[A]

  def intersectionWith(anotherSet: MySet[A]): MySet[A]

  def differenceWith(anotherSet: MySet[A]): MySet[A]

  def isEmpty: Boolean

}

case class MySetEmpty[A]() extends MySet[A] {

  override def apply(v1: A): Boolean = false

  override def contains(elem: A): Boolean = false

  override def +(elem: A): MySet[A] = MySetNonEmpty(elem, this)

  override def ++(anotherSet: MySet[A]): MySet[A] = anotherSet

  override def map[B](f: A => B): MySet[B] = MySetEmpty()

  override def flatMap[B](f: A => MySet[B]): MySet[B] = MySetEmpty()

  override def filter(predicate: A => Boolean): MySet[A] = MySetEmpty()

  override def foreach(f: A => Unit): Unit = {}

  override def head: A = throw new Exception("nope")

  override def tail: MySet[A] = this

  override def remove(elem: A, acc: MySet[A]): MySet[A] = this

  override def intersectionWith(anotherSet: MySet[A]): MySet[A] = this

  override def differenceWith(anotherSet: MySet[A]): MySet[A] = anotherSet

  override def isEmpty: Boolean = true
}

case class MySetNonEmpty[A](head: A, tail: MySet[A]) extends MySet[A] {

  override def isEmpty: Boolean = true

  override def apply(v1: A): Boolean = head match {
    case h if v1 == h => true
    case _ => tail(v1)
  }

  override def contains(elem: A): Boolean = this (elem)

  override def +(elem: A): MySet[A] =
    if (this contains elem) this
    else MySetNonEmpty(elem, this)

  override def ++(anotherSet: MySet[A]): MySet[A] = tail ++ anotherSet + head

  override def map[B](f: A => B): MySet[B] = MySetNonEmpty(f(head), tail.map(f))

  override def flatMap[B](f: A => MySet[B]): MySet[B] = f(head) ++ tail.flatMap(f)

  override def filter(predicate: A => Boolean): MySet[A] = {
    if (predicate(head)) MySetNonEmpty(head, tail.filter(predicate))
    else tail.filter(predicate)
  }

  override def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }

  override def remove(elem: A, acc: MySet[A]): MySet[A] = {
    if (elem == head) tail
    else tail.remove(elem, acc + head)
  }
  override def intersectionWith(anotherSet: MySet[A]): MySet[A] = {
    flatMap { x =>
      if (anotherSet.isEmpty) MySetEmpty()
      else if (x == anotherSet.head) MySetEmpty() + x
      else intersectionWith(anotherSet.tail)
    }
  }

  override def differenceWith(anotherSet: MySet[A]): MySet[A] = ???
}
object MySet {
  def apply[A](x: A*): MySet[A] = {
    @tailrec
    def build(seq: Seq[A], acc: MySet[A]): MySet[A] = {
      if (seq.isEmpty) acc
      else build(seq.tail, acc + seq.head)

    }

    build(x, new MySetEmpty[A])
  }
}
object set extends App {



  val mySet = MySet(1, 2, 3)
  val myNewSet = MySet(2, 3, 5)
  //  println(mySet)
  //  println(mySet(4))
  //  mySet + 3 flatMap (x => MySet(x, x * 10)) foreach println
  val myIntercestedSet = mySet intersectionWith myNewSet
  myIntercestedSet foreach println
}
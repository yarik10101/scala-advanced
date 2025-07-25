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

  def -(elem: A): MySet[A]

  def &(anotherSet: MySet[A]): MySet[A]

  def --(anotherSet: MySet[A]): MySet[A]

  def isEmpty: Boolean

  def unary_! : MySet[A]

}

case class EmptySet[A]() extends MySet[A] {

  override def apply(v1: A): Boolean = false

  override def contains(elem: A): Boolean = false

  override def +(elem: A): MySet[A] = NonEmptySet(elem, this)

  override def ++(anotherSet: MySet[A]): MySet[A] = anotherSet

  override def map[B](f: A => B): MySet[B] = EmptySet()

  override def flatMap[B](f: A => MySet[B]): MySet[B] = EmptySet()

  override def filter(predicate: A => Boolean): MySet[A] = EmptySet()

  override def foreach(f: A => Unit): Unit = {}


  override def -(elem: A): MySet[A] = this

  override def &(anotherSet: MySet[A]): MySet[A] = this

  override def --(anotherSet: MySet[A]): MySet[A] = anotherSet

  override def isEmpty: Boolean = true

  override def unary_! : MySet[A] = new PropertyBasedSet[A](_ => true)

}
class PropertyBasedSet[A](property: A => Boolean) extends MySet[A] {
  override def contains(x: A): Boolean = property(x)

  override def +(y: A): PropertyBasedSet[A] = new PropertyBasedSet[A](x => property(x) || x == y)

  override def ++(anotherSet: MySet[A]): PropertyBasedSet[A] = new PropertyBasedSet[A](x => this.contains(x) || anotherSet.contains(x))

  override def map[B](f: A => B): PropertyBasedSet[B] = throw new Exception("no map for u")

  override def flatMap[B](f: A => MySet[B]): PropertyBasedSet[B] = throw new Exception("no fMap for u")

  override def filter(predicate: A => Boolean): PropertyBasedSet[A] = new PropertyBasedSet[A](x => predicate(x) && property(x))

  override def foreach(f: A => Unit): Unit = throw new Exception("go foreach yourself")

  override def -(elem: A): MySet[A] = new PropertyBasedSet[A](x => property(x) && x != elem)

  override def &(anotherSet: MySet[A]): MySet[A] = new PropertyBasedSet[A](x => contains(x) && anotherSet.contains(x))

  override def --(anotherSet: MySet[A]): MySet[A] = new PropertyBasedSet[A](x => contains(x) && !anotherSet.contains(x))

  override def isEmpty: Boolean = throw new Exception("vpadlupizdec")

  override def unary_! : MySet[A] = new PropertyBasedSet[A](x => !contains(x))

  override def apply(v1: A): Boolean = property(v1)
}


case class NonEmptySet[A](head: A, tail: MySet[A]) extends MySet[A] {

  override def isEmpty: Boolean = false

  override def apply(v1: A): Boolean = head match {
    case h if v1 == h => true
    case _ => tail(v1)
  }

  override def contains(elem: A): Boolean = this.apply(elem)

  override def +(elem: A): MySet[A] =
    if (this contains elem) this
    else NonEmptySet(elem, this)

  override def ++(anotherSet: MySet[A]): MySet[A] = tail ++ anotherSet + head

  override def map[B](f: A => B): MySet[B] = NonEmptySet(f(head), tail.map(f))

  override def flatMap[B](f: A => MySet[B]): MySet[B] = f(head) ++ tail.flatMap(f)

  override def filter(predicate: A => Boolean): MySet[A] = {
    if (predicate(head)) NonEmptySet(head, tail.filter(predicate))
    else tail.filter(predicate)
  }

  override def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }

  override def -(elem: A): MySet[A] = {
    if (elem == head) tail
    else tail - elem + head
  }

  override def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet)

  override def --(anotherSet: MySet[A]): MySet[A] = filter(!anotherSet)

  //  filter(x => !anotherSet.contains(x))
  def unary_! : MySet[A] = new PropertyBasedSet[A](x => !contains(x))
}
  object MySet {
    def apply[A](x: A*): MySet[A] = {
      @tailrec
      def build(seq: Seq[A], acc: MySet[A]): MySet[A] = {
        if (seq.isEmpty) acc
        else build(seq.tail, acc + seq.head)

      }

      build(x, new EmptySet[A])
    }
  }

  object set extends App {


    val mySet = MySet(1, 2, 3, 4)
    val myNewSet = MySet(2, 3, 5)
    //  println(mySet)
    //  println(mySet(4))
    //  mySet + 3 flatMap (x => MySet(x, x * 10)) foreach println
    val myIntercestedSet = mySet & myNewSet
    //  myIntercestedSet foreach println
    val myNonCommonSet = mySet -- myNewSet
    myNonCommonSet foreach println

  }



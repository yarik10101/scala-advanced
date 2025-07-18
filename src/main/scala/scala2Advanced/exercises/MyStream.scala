package scala2Advanced.exercises

abstract class MyStream[+A]  {
  def isEmpty: Boolean
  def head: A
  def tail: MyStream[A]

  def #::[B >: A](element: B): MyStream[B] // prepend
  def ++[B >: A](anotherStream: MyStream[B]): MyStream[B]

  def foreach(f: A => Unit): Unit
  def map[B](f: A => B): MyStream[B]
  def flatMap[B](f: A => MyStream[B]): MyStream[B]
  def filter(predicate: A => Boolean): MyStream[A]

  def take(n: Int): MyStream[A] // takes the first n elements out of this stream
  def takeAsList(n: Int): List[A]
  def toList: List[A]
}

object EmptyStream extends MyStream[Nothing] {


  override def isEmpty: Boolean = true

  override def head: Nothing = throw new Exception("h")

  override def tail: MyStream[Nothing] = throw new Exception("t")

  override def #::[B >: Nothing](element: B): MyStream[B] = new NonEmptyStream(element, EmptyStream)

  override def ++[B >: Nothing](anotherStream: MyStream[B]): MyStream[B] = anotherStream


  override def foreach(f: Nothing => Unit): Unit = {}

  override def map[B](f: Nothing => B): MyStream[B] = this

  override def flatMap[B](f: Nothing => MyStream[B]): MyStream[B] = this

  override def filter(predicate: Nothing => Boolean): MyStream[Nothing] = this

  override def take(n: Int): MyStream[Nothing] = this

  override def takeAsList(n: Int): List[Nothing] = Nil

  def toList: List[Nothing] = Nil
}

class NonEmptyStream[+A](val head: A, lazyTail: => MyStream[A]) extends MyStream[A] {

  override def tail: MyStream[A] = lazyTail

  override def isEmpty: Boolean = false

  override def #::[B >: A](element: B): MyStream[B] = new NonEmptyStream[B](element, this)

  override def ++[B >: A](anotherStream: MyStream[B]): MyStream[B] = new NonEmptyStream[B](head, tail ++ anotherStream)


  override def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }

  override def map[B](f: A => B): MyStream[B] = new NonEmptyStream[B](f(head), tail.map(f))
// f(head) #:: tail.map(f)
  override def flatMap[B](f: A => MyStream[B]): MyStream[B] =
//f(head) ++ tail.flatMap(f)
  override def filter(predicate: A => Boolean): MyStream[A] = {
    if (predicate(head)) head #:: tail.filter(predicate)
    else tail.filter(predicate)
  }

  override def take(n: Int): MyStream[A] = {
    if (n != 0) new NonEmptyStream(head, tail.take(n - 1))
    else EmptyStream
  }

  override def takeAsList(n: Int): List[A] = {
    take(n).toList
  }

  def toList: List[A] = head :: tail.toList

}

object MyStream {
  def from[A](start: A)(generator: A => A): MyStream[A] = new NonEmptyStream[A](start, from(generator(start))(generator))

}
// MyStream.from(1)(x => x + 1)
object MyStreamObject extends App {
  val test = MyStream.from(1)(x => x + 1)
  val test1 = new NonEmptyStream(1, new NonEmptyStream(2, EmptyStream))
  val test2 = MyStream.from(3)(x => x * 2)

//  test.map(_ * 2).take(100).foreach(println)
//  test.flatMap(x => new NonEmptyStream(x, new NonEmptyStream(x + 1, EmptyStream))).take(10).foreach(println)
//  (3.5 #:: test).take(2).foreach(println)
  val t = test1 ++ test2
//  t.take(10).foreach(println)
}
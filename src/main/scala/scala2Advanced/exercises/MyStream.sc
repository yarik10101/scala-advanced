abstract class MyStream[+A] {
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
}

class EmptyStream[A] extends MyStream {
  override def isEmpty: Boolean = true

  override def head: Nothing = throw new Exception("head of an EmptyStream")

  override def tail: MyStream[Nothing] = throw new Exception("tail of an EmptyStream")

  override def #::[B >: Nothing](element: B): MyStream[B] = NonEmptyStream(element, new EmptyStream)

  override def ++[B >: Nothing](anotherStream: MyStream[B]): MyStream[B] =

  override def foreach(f: Nothing => Unit): Unit = ???

  override def map[B](f: Nothing => B): MyStream[B] = ???

  override def flatMap[B](f: Nothing => MyStream[B]): MyStream[B] = ???

  override def filter(predicate: Nothing => Boolean): MyStream[Nothing] = ???

  override def take(n: Int): MyStream[Nothing] = ???

  override def takeAsList(n: Int): List[Nothing] = ???
}

case class NonEmptyStream[+A](head: A, tail: MyStream[A]) extends MyStream[A] {
  override def isEmpty: Boolean = ???

  override def #::[B >: A](element: B): MyStream[B] = ???

  override def ++[B >: A](anotherStream: MyStream[B]): MyStream[B] = ???

  override def foreach(f: A => Unit): Unit = ???

  override def map[B](f: A => B): MyStream[B] = ???

  override def flatMap[B](f: A => MyStream[B]): MyStream[B] = ???

  override def filter(predicate: A => Boolean): MyStream[A] = ???

  override def take(n: Int): MyStream[A] = ???

  override def takeAsList(n: Int): List[A] = ???
}

object MyStream {
  def from[A](start: A)(generator: A => A): MyStream[A] =
}
// MyStream.from(1)(x => x + 1)
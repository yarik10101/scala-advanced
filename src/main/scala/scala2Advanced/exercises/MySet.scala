package scala2Advanced.exercises

trait MySet[A] extends (A => Boolean) {
  def contains(elem: A): Boolean

  def +(elem: A): MySet[A]

  def ++(anotherSet: MySet[A]): MySet[A]

  def map[B](f: A => B): MySet[B]

  def flatMap[B](f: A => MySet[B]): MySet[B]

  def filter(predicate: A => Boolean): MySet[A]

  def foreach(f: A => Unit): Unit
}

case class MySetEmpty[A]() extends MySet[A] {
  override def contains(elem: A): Boolean = false

  override def +(elem: A): MySet[A] = MySetNonEmpty(elem, this)

  override def ++(anotherSet: MySet[A]): MySet[A] = anotherSet

  override def map[B](f: A => B): MySet[B] = MySetEmpty()

  override def flatMap[B](f: A => MySet[B]): MySet[B] = MySetEmpty()

  override def filter(predicate: A => Boolean): MySet[A] = MySetEmpty()

  override def foreach(f: A => Unit): Unit = {}
}

case class MySetNonEmpty[A](head: A, tail: MySet[A]) extends MySet[A] {
  override def contains(elem: A): Boolean = apply(elem)

  override def +(elem: A): MySet[A] = MySetNonEmpty(elem, this)

  override def ++(anotherSet: MySet[A]): MySet[A] = MySetNonEmpty(head, tail + anotherSet)

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

}
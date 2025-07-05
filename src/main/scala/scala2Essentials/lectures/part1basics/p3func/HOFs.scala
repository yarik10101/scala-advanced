package scala2Essentials.lectures.part1basics.p3func

object HOFs extends App {
  val superFunction: (Int, (String, Int => Boolean) => Int) => Int => Int = ???

  def nTimes(f: Int => Int, n: Int, x: Int): Int =
    if (n <= 0) x
    else nTimes(f, n - 1, f(x))

  def toCurry[B](func: (B, B) => B): B => B => B = {
    x => y => func(x, y)
  }

  def fromCurry[B](func: B => B => B): (B, B) => B = {
    (x, y) => func(x)(y)
  }

  def compose[A](f: A => A, g: A => A): A => A = {
    x => f(g(x))
  }

  def andThen[A](f: A => A, g: A => A): A => A = {
    x => g(f(x))
  }

  def quickSort[A](list: List[A], predicate: (A, A) => Boolean): List[A] =
    if (list.tail.isEmpty) list
    else {
      val (left, right) = list.tail.partition(predicate(list.head, _))
      quickSort(left, predicate).appendedAll(quickSort(right, predicate).prepended(list.head))
    }


}



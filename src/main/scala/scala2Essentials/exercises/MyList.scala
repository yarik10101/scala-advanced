package scala2Essentials.exercises

import java.security.KeyStore.TrustedCertificateEntry
import scala.Byte.MaxValue

abstract class MyList[+A] {
  def head: A

  def tail: MyList[A]

  def isEmpty: Boolean

  def add[B >: A](x: B): MyList[B]

  def toString: String

  def map[B >: A](trans: A => B): MyList[B]

  def filter(pedic: A => Boolean): MyList[A]

  def flatMap[B >: A](fMap: A => MyList[B]): MyList[B]

  def +:[B >: A](xList: MyList[B]): MyList[B]

  def :+[B >: A](xList: MyList[B]): MyList[B]

  def remove[B >: A](index: B): MyList[B]

  def sort[B >: A](func: (B, B) => Boolean, acc: MyList[B] = MyListEmpty): MyList[B]

  def minValue[B >: A](func: (B, B) => Boolean, acc: B = head): B

  def foreach[B >: A](func: A => B): B

  def zipWith[B >: A](xList: MyList[B], func: (A, B) => B, acc: MyList[B] = MyListEmpty): MyList[B]

  def reverse[B >: A](acc: MyList[B] = MyListEmpty): MyList[B]

  def fold[B >: A](start: B, funcFold: (B, B) => B): B

}

case object MyListEmpty extends MyList[Nothing] {
  override def head: Nothing = throw new Exception("go fuck yourself")

  override def tail: MyList[Nothing] = throw new Exception("go fuck yourself")

  override def isEmpty: Boolean = true

  override def add[B >: Nothing](x: B): MyList[B] = MyListNotEmpty(x, this)

  override def toString: String = "List()"

  override def map[B >: Nothing](trans: Nothing => B): MyList[B] =
    this

  override def filter(pedic: Nothing => Boolean): MyList[Nothing] =
    this

  override def flatMap[B >: Nothing](fMap: Nothing => MyList[B]): MyList[B] =
    this

  override def +:[B >: Nothing](xList: MyList[B]): MyList[B] = xList

  override def :+[B >: Nothing](xList: MyList[B]): MyList[B] = xList

  override def remove[B >: Nothing](index: B): MyList[B] = this

  override def sort[B >: Nothing](func: (B, B) => Boolean, acc: MyList[B] = MyListEmpty): MyList[B] = acc

  override def minValue[B >: Nothing](func: (B, B) => Boolean, acc: B = head): Nothing = throw new Exception("gg")

  override def foreach[B >: Nothing](func: Nothing => B): Nothing = throw new Exception("GGG")

  override def zipWith[B >: Nothing](xList: MyList[B], func: (Nothing, B) => B, acc: MyList[B] = MyListEmpty): MyList[B] = {
    if (!xList.isEmpty) throw new Exception("nea")
    else acc.reverse()
  }

  override def reverse[B >: Nothing](acc: MyList[B] = MyListEmpty): MyList[B] = acc

  override def fold[B >: Nothing](start: B, funcFold: (B, B) => B): B = start
}

case class MyListNotEmpty[+A](head: A, tail: MyList[A]) extends MyList[A] {

  override def isEmpty: Boolean = false

  override def add[B >: A](x: B): MyList[B] = MyListNotEmpty(x, this)

  override def toString: String = {
    def toStringHelp(list: MyList[A]): String = {
      if (list.isEmpty) ""
      else if (list.tail.isEmpty) s"${list.head}"
      else list.head.toString + ", " + toStringHelp(list.tail)
    }

    s"MyList(${toStringHelp(this)})"
  }

  override def map[B >: A](trans: A => B): MyList[B] = {
    MyListNotEmpty(trans(head), tail.map(trans))
  }

  override def filter(pedic: A => Boolean): MyList[A] =
    if (pedic(head)) MyListNotEmpty(head, tail.filter(pedic))
    else tail.filter(pedic)

  override def flatMap[B >: A](fMap: A => MyList[B]): MyList[B] =
    fMap(head) :+ tail.flatMap(fMap)

  override def +:[B >: A](xList: MyList[B]): MyList[B] =
    if (!xList.isEmpty) MyListNotEmpty(xList.head, this +: xList.tail)
    else this

  override def :+[B >: A](xList: MyList[B]): MyList[B] = {
    MyListNotEmpty(head, tail :+ xList)
  }

  override def remove[B >: A](elem: B): MyList[B] = {
    if (elem == head) tail
    else new MyListNotEmpty[B](head, tail.remove(elem))
  }

  override def minValue[B >: A](func: (B, B) => Boolean, acc: B = head): B = {
    if (tail.isEmpty) acc
    else if (func(tail.head, acc)) tail.minValue(func, tail.head)
    else tail.minValue(func, acc)
  }

  override def sort[B >: A](func: (B, B) => Boolean, acc: MyList[B] = MyListEmpty): MyList[B] = {
    val small = minValue(func)
    remove(small).sort(func, acc.add(small))
  }

  override def foreach[B >: A](func: A => B): B = {
    if (tail.isEmpty) func(head)
    else {
      func(head)
      tail.foreach(func)
    }
  }

  override def zipWith[B >: A](xList: MyList[B], funcZip: (A, B) => B, acc: MyList[B] = MyListEmpty): MyList[B] = {
    tail.zipWith(xList.tail, funcZip, acc.add(funcZip(head, xList.head)))
  }

  override def reverse[B >: A](acc: MyList[B] = MyListEmpty): MyList[B] = {
    tail.reverse(acc.add(head))
  }

  override def fold[B >: A](start: B, funcFold: (B, B) => B): B = {
    tail.fold(funcFold(start, head), funcFold)
  }

}
object MyList {
  def apply[A](x: A*): MyList[A] = {
    def loop(xs: List[A]): MyList[A] = xs match {
      case Nil => MyListEmpty
      case head +: tail => MyListNotEmpty(head, loop(tail))
    }

    loop(x.toList)
  }

}

object GanDon extends App {

  val mylist: MyList[Int] = MyListNotEmpty(1, MyListNotEmpty(2, MyListNotEmpty(3, MyListNotEmpty(4, MyListEmpty))))
  val mylist1: MyList[Int] = MyListNotEmpty(2, MyListNotEmpty(3, MyListNotEmpty(4, MyListNotEmpty(5, MyListEmpty))))
  val transformerInstance: Int => Int = _ * 2
  val newTransformerInstance: Int => MyList[Int] = X => MyListNotEmpty(X, MyListNotEmpty(X + 1, MyListEmpty))

  //  println(mylist.map(transformerInstance))

  val predicateInstance: Int => Boolean = _ % 2 == 0

  //  println(mylist.filter(predicateInstance))
  //  println(mylist.flatMap(newTransformerInstance))
  //  println(List(1, 2, 3).map(n => n * 2))
//  println(mylist.sort((x: Int, y: Int) => x < y))
//  mylist.foreach(x => println(x * 2))
//  println(mylist.zipWith(mylist1, (x: Int, y: Int) => x * y))
//  println(mylist.reverse())
//  println(mylist.fold(100, (x: Int, y: Int) => x + y))
//  println(for {
//    n <- mylist
//    k <- mylist1
//  } yield n + k + 1)
  val myNewList = MyList(1, 2, 3, 55)
  println(myNewList)


}
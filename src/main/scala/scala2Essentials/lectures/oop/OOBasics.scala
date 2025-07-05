package scala2Essentials.lectures.oop

object OOBasics extends App {
  val writer = new Writer("sania", "suslov", 19)
  val novel = new Novel("tinker", 2007, writer)
  println(writer.fullName())
  println(novel.authorAge())
}

class Writer(fName: String, sName: String, val age: Int) {
  def fullName(): String = s"Full name is $fName $sName "
}

class Novel(name: String, releaseYear: Int, author: Writer) {
  def authorAge(): Int = {
    val releaseAge = author.age - 2025 + releaseYear
    releaseAge
  }

  def isWrittenBy(): String = author.fullName()

  def copy(newReleaseYear: Int): Novel =
    new Novel(name, newReleaseYear, author)


}
package scala2Essentials.lectures.part1basics.p3func

import scala.util.{Failure, Random, Success, Try}

object HandlingFailure extends App {
  val hostname = "localost"
  val port = "8080"
  def renderHTML(page: String):Unit = println(page)

  class Connection {
    def get(url: String): String = {
      val random = new Random(System.nanoTime())
      if (random.nextBoolean()) "<html>...</html>"
      else throw new RuntimeException("Connection interrupted")
    }
  }

  object HttpService {
    val random = new Random(System.nanoTime())

    def getConnection(host: String, port: String): Connection =
      if (random.nextBoolean()) new Connection
      else throw new RuntimeException("Someone else took the port")
  }

  val tryGetConnection = for {
    x <- Try(HttpService.getConnection(hostname, port))
    y <- Try(x.get("/homo"))

  } yield renderHTML(y)
println(tryGetConnection)
  val newFailure = Failure(new RuntimeException("sosi"))
  println(newFailure.flatMap(x => Failure(new RuntimeException("sosiii"))))




}

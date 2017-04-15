package lectures.exceptions

import scala.util.Try

/**
  * Created by pavel on 05.04.17.
  */
object PrintGreetings {
  case class Greeting(msg: String)
  private val data = Array(Greeting("Hi"), Greeting("Hello"),
    Greeting("Good morning"), Greeting("Good afternoon"),
    null, null)
  def printGreetings() = {
    for (i <- 0 to 10) {
      Try {
        println(data(i).msg)
      } recover {
        case e: NullPointerException => println(e)
        case e: ArrayIndexOutOfBoundsException => println(e)
      }
    }
  }
}

object GreetingsApp extends App {
  PrintGreetings.printGreetings()
}

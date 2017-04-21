package lectures.concurrent

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.util._

/**
  * Smooth - это своебразный функциональный кэш, предназначенный для исключения повторных вызовов кода
  * до того, как получен результат первого вызова.
  * Он работает следующим образом:
  * * * * в объект Smooth в метод apply передается код, который может выполняться какое-то время, и возвращает какое-то значение
  * * * * apply создаст инстанс Smooth
  * * * * созданный инстанс при вызове apply возвращает Future
  * * * * * и запускает код, если код еще не запущен
  * * * * * и не запускает код, если код еще не завершился с момента предыдущего запуска
  *
  * Подсказка: можно использовать AtomicReference
  *
  */
object Smooth{
  def apply[T](thunk: => T): Smooth[T] = new Smooth(thunk)
}

class Smooth[T](thunk: => T) {

   var is_running = new AtomicBoolean(false)
   var resut: AtomicReference[Future[T]] = new AtomicReference[Future[T]](null)

   def apply(): Future[T] = if (is_running.getAndSet(true)) resut.get() else {
      val f = Future {
         thunk
      }
      resut.set(f)
      f onComplete {
         case _ => is_running.set(false)
      }
      f
   }
}

object SmoothExample extends App {
   val executeCode1 = Smooth({
      Thread.sleep(1000)
      (Random.alphanumeric take 10 toList) mkString
   })
   var res1 = executeCode1()
   res1 onSuccess {
      case result => println(s"First res = $result")
   }
   val res2 = executeCode1()
   res2 onSuccess {
      case result => println(s"Second res = $result")
   }
   println("Calls during second are equal: " + (res1 eq res2))
   Thread.sleep(1100)
   val res3 = executeCode1()
   res3 onSuccess {
      case result => println(s"Third res = $result")
   }
   println("Calls after a second are equal: " + (res1 eq res3))
   Await.result(res3, Duration.Inf)
}
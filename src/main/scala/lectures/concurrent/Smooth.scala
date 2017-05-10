package lectures.concurrent

import java.util.concurrent.atomic.{AtomicBoolean}

import scala.concurrent.{Await, Future, Promise}
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

   @volatile private var res = Promise[T]()
   private val is_running = new AtomicBoolean(false)

   def apply(): Future[T] = {
      if (is_running.getAndSet(true)) res.future
      else {
         val f = Future {
            thunk
         }
         f onComplete {
            case _ => is_running.set(false)
         }
         res.tryCompleteWith(f)                 //for cases, when some thread managed to enter then,
                                                // before Promise was completed with future
         res = Promise[T]().tryCompleteWith(f)
         f
      }
   }

   def then_apply() = res.future
}

object SmoothExample extends App {
   val executeCode1 = Smooth({
      Thread.sleep(1000)
      (Random.alphanumeric take 10 toList) mkString
   })
   //Emulate Situation when thread enters "then" block (res.future) before
   // "else" block is finished by the first thread, that called apply
   var fut0 = executeCode1.then_apply()
   var fut1 = executeCode1()
   Thread.sleep(100)
   val fut2 = executeCode1()
   for {
      res0 <- fut0
      res1 <- fut1
      res2 <- fut2
   } {
      println(s"Zero res = $res0")
      println(s"First res = $res1")
      println(s"Second res = $res2")
      println(s"First eq Second ${res1 == res2}")
      println(s"First eq Zero ${res1 == res0}")
      /* Ex.:
Zero res = uQMKcd0NCE
First res = uQMKcd0NCE
Second res = uQMKcd0NCE
First eq Second true
First eq Zero true
       */
   }
   Thread.sleep(1100)
   val fut3 = executeCode1()
   Thread.sleep(100)
   val fut4 = executeCode1()
   for {
      res1 <- fut1
      res3 <- fut3
      res4 <- fut4
   } {
      println(s"Third res = $res3")
      println(s"Forth res = $res4")
      println(s"First eq Third ${res1 == res3}")
      println(s"Third eq Forth ${res3 == res4}")
   }
   /* Ex.:
Third res = WsCpjLjoco
Forth res = WsCpjLjoco
First eq Third false
Third eq Forth true
 */
   Await.result(fut4, Duration.Inf)
   Thread.sleep(10)
}
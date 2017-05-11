package lectures.eval

import java.time.Clock

import scala.collection.SeqView

/**
  * В этом задании, ваша задча реализовать своеобразный View с таймером
  *
  * Он должен предаставлять из себя стандартный SeqView c ограничением по времени
  * Т.е. этот view ведет себя как обычно пока не истечет таймаут, предеданный при создании
  * Как только таймаут истекает view должен начать вести себя так, как буд-то коллекция пуста
  *
  * Для решения задачи подставьте на место вопросительныйх знаков реализацию view
  * Раскомментируйте и выполните тесты а lectures.eval.LazySchedulerTest
  */

object LazySchedulerView {

//      "it already has expired" in {
//        val g = Gen.listOfN(100, Gen.alphaStr)
//        forAll(g) { l =>
//          val r = l.lazySchedule(-1).map {
//            identity
//          }
//          r should have size 0
//        }
//      }
//    }
  implicit class SeqViewConverter[A](f: Seq[A]) {
    val c = Clock.systemDefaultZone()
    val emptySeq = Seq()

    /**
      *
      * @param expirationTimeout - таймаут после которого view становиться пустым в милисекундах
      * @return - view
      */
    def lazySchedule(expirationTimeout: Long): SeqView[A, Seq[_]]  = {
      val i = c.instant().plusMillis(expirationTimeout)

      new SeqView[A, Seq[_]] {
        override def iterator: Iterator[A] = if(i.isAfter(c.instant())) f.iterator else emptySeq.iterator

        override protected def underlying: Seq[_] = if(i.isAfter(c.instant())) f else emptySeq

        override def length: Int = if(i.isAfter(c.instant())) f.length else emptySeq.length

        override def apply(idx: Int): A = if(i.isAfter(c.instant())) f(idx) else emptySeq(idx)
      }
    }
  }
}

object LazySchedulerViewExample extends App {

  import LazySchedulerView._

  val v = List(1, 2, 3, 56)
  val d = v.lazySchedule(1300)

  print(d.length)
  Thread.sleep(1500)
  print(d.length)
}



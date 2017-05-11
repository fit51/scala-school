package lectures.collections

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
  * Представим, что по какой-то причине Вам понадобилась своя обертка над списком целых чисел List[Int]
  *
  * Вы приняли решение, что будет достаточно реализовать 4 метода:
  * * * * * def flatMap(f: (Int => MyList)) -  реализуете на основе соответствующего метода из List
  * * * * * метод map(f: (Int) => Int) - с помощью только что полученного метода flatMap класса MyList
  * * * * * filter(???) - через метод flatMap класса MyList
  * * * * * foldLeft(acc: Int)(???) - через декомпозицию на head и tail
  *
  * Для того, чтобы выполнить задание:
  * * * * * раскомментируйте код
  * * * * * замените знаки вопроса на сигнатуры и тела методов
  * * * * * не используйте var и мутабильные коллекции
  *
  */
object MyListImpl extends App {

  case class MyList[A, I <: Iterable[A]](data: Iterable[A]) {//I type is useless

    def flatMap[B](f: A => Iterable[B]): MyList[B, Iterable[B]] =
      MyList(data.flatMap(inp => f(inp)))

    def map[B](f: A => B) = flatMap(f andThen { x => Iterable(x) })

    def foldLeft(acc: A)(f: ((A, A)) => A): A = data match {//I've got no idea how to make acc - [B] (test do not work)
      case Nil => acc
      case x :: xs => MyList(xs).foldLeft(f((acc, x)))(f)
    }

    def filter(f: (A) => Boolean) = flatMap(x => {
      if (f(x))
        Iterable(x)
      else
        Iterable()
    })
  }

  object MyListBuffer {
    def apply[A](data: ListBuffer[A]): MyListBuffer[A] = new MyListBuffer(data)
  }
  class MyListBuffer[A](data: ListBuffer[A]) extends MyList(data)

  object MyIndexedList {
    def apply[A](data: IndexedSeq[A]): MyIndexedList[A] = new MyIndexedList(data)
  }
  class MyIndexedList[A](data: IndexedSeq[A]) extends MyList(data)

  require(MyList(List(1, 2, 3, 4, 5, 6)).map(_ * 2).data == List(2, 4, 6, 8, 10, 12))
  require(MyList(List(1, 2, 3, 4, 5, 6)).filter(_ % 2 == 0).data == List(2, 4, 6))
  require(MyList(List(1, 2, 3, 4, 5, 6)).foldLeft(0)((tpl) => tpl._1 + tpl._2) == 21)
  require(MyList(Nil).foldLeft(0)((tpl) => tpl._1 + tpl._2) == 0)

}
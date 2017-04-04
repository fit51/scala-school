package lectures.operators

import lectures.functions.{Computation, CurriedComputation, Data, FunctionalComputation}
import org.scalameter._

/**
  * В задачке из lectures.functions.Computations мы реализовали
  * один и тот же метод 3-мя разными способами
  *
  * Пришло время оценить, насколько разные имплементации
  * отличаются друг от друга по производительности
  *
  * Для этого
  *   * в классах CurriedComputation и FunctionalComputation уберите extends App, оставьте extends Data
  *   * раскомментируйте код, выполните в циклах вызов 3-х имплементаций,
  *   * оцените разницу во времени выполнения и объясните ее происхожение
  *
  * Каррсированная и функциональная версия выполняются в 10 раз быстрее, чем computation, это происходит потому, что
  * в computation каждый раз пересчитывается filterArray = filterData.split(" "), а в каррированной и функциональной -
  * только один раз при инициализации объекта.
  * Время выполнения карированной и функциональной версии практически равны, хотя у время выполнения карированной
  * функции чуть дольше (1-2мс) функциональной: думаю это связано с тем, что в
  * карированной функции тратится время на создание PatiallyAppliedFunction
  */
object EvaluateOptimization extends App with Data {


  // ВЫПОЛНИТЬ В ЦИКЛЕ ОТ 1 ДО 100 Computation.computation
  val timeWarmComputation = withWarmer(new Warmer.Default) measure {
    for (i <- 1 until 100) {
      Computation.computation(filterData, dataArray)
    }
  }

  println("Elapsed time in computation(): " + timeWarmComputation)




  // ВЫПОЛНИТЬ В ЦИКЛЕ ОТ 1 ДО 100 CurriedComputation.partiallyAppliedCurriedFunction
  val timeWarmPartiallyApplied = withWarmer(new Warmer.Default) measure {
    val curriedFunc = CurriedComputation.curriedComputation(filterData)_
    for (i <- 1 until 100) {
      curriedFunc(dataArray)
    }
  }

  println("Elapsed time in partiallyAppliedCurriedFunction(): " + timeWarmPartiallyApplied)




  // ВЫПОЛНИТЬ В ЦИКЛЕ ОТ 1 ДО 100 FunctionalComputation.filterApplied
  val timeWarmFilterApplied = withWarmer(new Warmer.Default) measure {
    val filterFunc = FunctionalComputation.functionalComputation(filterData)
    for (i <- 1 until 100) {
      filterFunc(dataArray)
    }
  }

  println("Elapsed time in filterApplied():" + timeWarmFilterApplied)

  // ВЫВЕСТИ РАЗНИЦУ В ПРОДОЛЖИТЕЛЬНОСТИ ВЫПОЛНЕНИЯ МЕЖДУ КАРРИРОВАННОЙ ВЕРСИЕЙ
  // И ФУНКЦИОНАЛЬНОЙ

  val diff = timeWarmPartiallyApplied - timeWarmFilterApplied

  println(s"Difference is about $diff milliseconds")
}


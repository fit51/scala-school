package lectures.operators

import lectures.functions.{Computation, CurriedComputation, Data, FunctionalComputation}

/**
  * В задачке из lectures.functions.Computations мы реализовали
  * один и тот же метод 3-мя разными способами
  *
  * Пришло время оценить, насколько разные имплементации
  * отличаются друг от друга по производительности
  *
  * Для этого раскомментируйте код, выполните в циклах вызов 3-х имплементаций,
  * оцените разницу во времени выполнения и объясните ее происхожение
  *
  * Каррсированная и функциональная версия выполняются в 10 раз быстрее, чем computation, это происходит потому, что
  * в computation каждый раз пересчитывается filterArray = filterData.split(" "), а в каррированной и функциональной -
  * только один раз при инициализации объекта.
  * Время выполнения карированной и функциональной версии практически равны, хотя у время выполнения карированной
  * функции чуть дольше (1-2мс) функциональной: думаю это связано с тем, что в
  * карированной функции тратится время на создание PatiallyAppliedFunction
  */
object EvaluateOptimization extends App with Data {

  val computationStartTimestamp = System.currentTimeMillis()

  // ВЫПОЛНИТЬ В ЦИКЛЕ ОТ 1 ДО 100 Computation.computation
  for (i <- 1 until 100) {
    Computation.computation(filterData, dataArray)
  }

  println("Elapsed time in computation(): " + (System.currentTimeMillis() - computationStartTimestamp))



  val partiallyAppliedStartTimestamp = System.currentTimeMillis()

  // ВЫПОЛНИТЬ В ЦИКЛЕ ОТ 1 ДО 100 CurriedComputation.partiallyAppliedCurriedFunction
  for (i <- 1 until 100) {
    CurriedComputation.partiallyAppliedCurriedFunction(dataArray)
  }

  val partiallyAppliedDuration = System.currentTimeMillis() - partiallyAppliedStartTimestamp
  println("Elapsed time in partiallyAppliedCurriedFunction(): " + partiallyAppliedDuration)



  val filterAppliedStartTimestamp = System.currentTimeMillis()

  // ВЫПОЛНИТЬ В ЦИКЛЕ ОТ 1 ДО 100 FunctionalComputation.filterApplied
  for (i <- 1 until 100) {
    FunctionalComputation.functionalComputation(filterData)(dataArray)
  }

  val filterAppliedDuration = System.currentTimeMillis() - filterAppliedStartTimestamp
  println("Elapsed time in filterApplied():" + filterAppliedDuration)

  // ВЫВЕСТИ РАЗНИЦУ В ПРОДОЛЖИТЕЛЬНОСТИ ВЫПОЛНЕНИЯ МЕЖДУ КАРРИРОВАННОЙ ВЕРСИЕЙ
  // И ФУНКЦИОНАЛЬНОЙ

  val diff = partiallyAppliedDuration - filterAppliedDuration

  println(s"Difference is about $diff milliseconds")
}


package lectures.oop.types

import lectures.matching.SortingStuff.Watches

import scala.util.Random

/**
  * Модифицируйте реализацию BSTImpl из предыдущего задания.
  * Используя тайп параметры и паттерн Type Class, реализуйте GeneralBSTImpl таким образом,
  * чтобы дерево могло работать с произвольным типом данных.
  *
  * Наследников GeneralBSTImpl определять нельзя.
  *
  * Создайте генератор для деревьев 3-х типов данных:
  * * * * float
  * * * * String
  * * * * Watches из задачи SortStuff. Большими считаются часы с большей стоимостью
  */

trait Appliable[T] {
  def compare(a: T, b: T): Int
  def generateRandom(): T
}

trait GeneralBST[T] {
  val value: T
  val left: Option[GeneralBST[T]]
  val right: Option[GeneralBST[T]]

  def add(newValue: T): GeneralBST[T]

  def find(value: T): Option[GeneralBST[T]]
}

object GeneralBSTImpl {
  def generate[T](root: GeneralBST[T], nodesCount: Int, maxValue: Int)(implicit appl: Appliable[T]): GeneralBST[T] = {
    var tempRoot = root
    for (i <- 1 until nodesCount)
      tempRoot = tempRoot.add(appl.generateRandom())
    tempRoot
  }
}

case class GeneralBSTImpl[T: Appliable](override val value: T,
                                        override val left: Option[GeneralBST[T]] = None,
                                        override val right: Option[GeneralBST[T]] = None) extends GeneralBST[T] {
  val applicator = implicitly[Appliable[T]]

  override def find(newValue: T): Option[GeneralBST[T]] = {
    if (applicator.compare(newValue, value) > 0)
      right flatMap { _.find(newValue) }
    else if (applicator.compare(newValue, value) < 0)
      left flatMap { _.find(newValue) }
    else
      Some(this)
  }

  override def add(newValue: T): GeneralBST[T] = {
    if (applicator.compare(newValue, value) > 0)
      right match {
        case Some(tree) => GeneralBSTImpl(value, left, Some(tree.add(newValue)))
        case None => GeneralBSTImpl(value, left, Some(GeneralBSTImpl(newValue)))
      } else if (applicator.compare(newValue, value) < 0)
      left match {
        case Some(tree) => GeneralBSTImpl(value, Some(tree.add(newValue)), right)
        case None => GeneralBSTImpl(value, Some(GeneralBSTImpl(newValue)), right)
      } else
      this
  }

  override def toString(): String = {
    val curValue = value.toString
    (left, right) match {
      case (None, None) => curValue
      case _ => {
        val leftBlock = (left match {
          case Some(tree) => tree.toString
          case None => ""
        }) split('\n') toList
        val rightBlock = (right match {
          case Some(tree) => tree.toString
          case None => ""
        }) split('\n') toList
        val leftWidth = (0 until leftBlock.head.length).map(_ => " ").mkString
        val rightWidth = (0 until rightBlock.head.length).map(_ => " ").mkString
        val valWidth = (0 until curValue.length).map(_ => " ").mkString
        val currentBlock = leftWidth + curValue + rightWidth ::
          (leftBlock.zipAll(rightBlock, leftWidth, rightWidth) map { (x) => { x._1 + valWidth + x._2} })
        currentBlock mkString ("\n")
      }
    }
  }
}

object GeneralTreeTest extends App {

  implicit val apInt: Appliable[Int] = new Appliable[Int] {
    override def compare(a: Int, b: Int): Int = a - b
    override def generateRandom(): Int = (Math.random() * maxValue) toInt
  }
  implicit val apString: Appliable[String] = new Appliable[String] {
    override def compare(a: String, b: String): Int = a.compare(b)
    override def generateRandom(): String = Random.alphanumeric.take(
      (Math.random() * 8) toInt
    ).mkString
  }
  implicit val apWatch: Appliable[Watches] = new Appliable[Watches] {
    override def compare(a: Watches, b: Watches): Int = (a.cost - b.cost) toInt
    override def generateRandom(): Watches = new Watches(apString.generateRandom(), apInt.generateRandom())
  }

  val sc = new java.util.Scanner(System.in)
  val maxValue = 110000
  val nodesCount = sc.nextInt()

  val markerItem = (Math.random() * maxValue).toInt
  val markerItem2 = (Math.random() * maxValue).toInt
  val markerItem3 = (Math.random() * maxValue).toInt

  // Generate huge Int tree
  val root: GeneralBST[Int] = GeneralBSTImpl(maxValue / 2)
  val tree: GeneralBST[Int] = GeneralBSTImpl.generate(root, nodesCount, maxValue)

  // Generate huge String tree
  val rootString: GeneralBST[String] = GeneralBSTImpl("Mroot")
  val treeString: GeneralBST[String] = GeneralBSTImpl.generate(rootString, nodesCount, maxValue)

  // Generate huge Watches tree
  val rootWatches: GeneralBST[Watches] = GeneralBSTImpl(new Watches("Mroot", maxValue/2))
  val treeWatches: GeneralBST[Watches] = GeneralBSTImpl.generate(rootWatches, nodesCount, maxValue)

  // add marker items
  val testTree = tree.add(markerItem).add(markerItem2).add(markerItem3)

  // check that search is correct
  require(testTree.find(markerItem).isDefined)
  require(testTree.find(markerItem).isDefined)
  require(testTree.find(markerItem).isDefined)

  println(testTree)
  println("----------------------------------------------------------")
  println(treeString)
  println("----------------------------------------------------------")
  println(treeWatches)
}
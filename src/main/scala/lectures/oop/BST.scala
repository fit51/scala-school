package lectures.oop


/**
  * BSTImpl - это бинарное дерево поиска, содержащее только значения типа Int
  *
  * * Оно обладает следующими свойствами:
  * * * * * левое поддерево содержит значения, меньшие значения родителя
  * * * * * правое поддерево содержит значения, большие значения родителя
  * * * * * значения, уже присутствующие в дереве, в него не добавляются
  * * * * * пустые значения (null) не допускаются
  *
  * * Завершите реализацию методов кейс класс BSTImpl:
  * * * * * Трейт BST и BSTImpl разрешается расширять любым образом
  * * * * * Изменять сигнатуры классов и методов, данные в условии, нельзя
  * * * * * Постарайтесь не использовать var и мутабильные коллекции
  * * * * * В задаче про распечатку дерева, нужно раскомментировать и реализовать метод toString()
  *
  * * Для этой структуры нужно реализовать генератор узлов.
  * * Генератор:
  * * * * * должен создавать дерево, содержащее nodesCount узлов.
  * * * * * не должен использовать переменные или мутабильные структуры.
  *
  */
trait BST {
  val value: Int
  val left: Option[BST]
  val right: Option[BST]

  def add(newValue: Int): BST

  def find(value: Int): Option[BST]
}

object BSTImpl {
  def generate(root: BST, nodesCount: Int, maxValue: Int): BST = {
    def randomVal = (Math.random() * maxValue) toInt
    var tempRoot = root
    for (i <- 1 until nodesCount)
      tempRoot = tempRoot.add(randomVal)
    tempRoot
  }
}

case class BSTImpl(value: Int,
                   left: Option[BSTImpl] = None,
                   right: Option[BSTImpl] = None) extends BST {

  def add(newValue: Int): BSTImpl = {
    if (newValue > value)
      right match {
        case Some(tree) => BSTImpl(value, left, Some(tree.add(newValue)))
        case None => BSTImpl(value, left, Some(BSTImpl(newValue)))
      } else if (newValue < value)
      left match {
        case Some(tree) => BSTImpl(value, Some(tree.add(newValue)), right)
        case None => BSTImpl(value, Some(BSTImpl(newValue)), right)
      } else
      this
  }

  def find(newValue: Int): Option[BST] = {
    if(newValue > value)
      right match {
        case Some(tree) => tree.find(newValue)
        case None =>  None
      }
    if(newValue < value)
      left match {
        case Some(tree) => tree.find(newValue)
        case None => None
      } else
      Some(this)
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

object TreeTest extends App {

  val sc = new java.util.Scanner(System.in)
  val maxValue = 110000
  val nodesCount = sc.nextInt()

  val markerItem = (Math.random() * maxValue).toInt
  val markerItem2 = (Math.random() * maxValue).toInt
  val markerItem3 = (Math.random() * maxValue).toInt

  // Generate huge tree
  val root: BST = BSTImpl(maxValue / 2)
  val tree: BST = BSTImpl.generate(root, nodesCount, maxValue)

  // add marker items
  val testTree = tree.add(markerItem).add(markerItem2).add(markerItem3)

  // check that search is correct
  require(testTree.find(markerItem).isDefined)
  require(testTree.find(markerItem).isDefined)
  require(testTree.find(markerItem).isDefined)

  println(testTree)
}
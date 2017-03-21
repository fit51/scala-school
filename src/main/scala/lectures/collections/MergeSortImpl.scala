package lectures.collections

/**
  * Постарайтесь не использовать мутабильные коллекции и var
  * Подробнее о сортировке можно подсмотреть здесь - https://en.wikipedia.org/wiki/Merge_sort
  *
  *
  */
object MergeSortImpl extends App {

  def mergeSort(data: Seq[Int]): Seq[Int] = {
    val n = data.length/2
    if(n == 0) data
    else {
      def merge(l1: Seq[Int], l2: Seq[Int]): List[Int] = (l1, l2) match {
        case (Nil, ls2) => ls2.toList
        case (ls1, Nil) => ls1.toList
        case _ => {
          if (l1.head < l2.head)
            l1.head :: merge(l1.tail, l2)
          else
            l2.head :: merge(l1, l2.tail)
        }
      }
      merge(mergeSort(data.take(n)), mergeSort(data.drop(n)))
    }
  }

}

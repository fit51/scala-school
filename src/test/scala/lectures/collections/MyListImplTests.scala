package lectures.collections

import lectures.collections.MyListImpl.{MyList, MyListBuffer, MyIndexedList}
import org.scalatest.{Matchers, WordSpec}

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class MyListImplTests extends WordSpec with Matchers  {

  "MyListImpl with Type parameters" should {
    "execute map correctly" in {
      MyList[Int, List[Int]](List(1, 2, 3, 4, 5, 6)).map(p => p * 2).data shouldBe List(2, 4, 6, 8, 10, 12)
    }
    "execute filter correctly" in {
      MyList[Long, ListBuffer[Long]](ListBuffer(1, 2, 3, 4, 5, 6)).filter(_ % 2 == 0).data shouldBe List(2, 4, 6)
    }
    "execute FoldLeft correctly for the same Type of acc and elements" in {
      MyList[Int, List[Int]](List(1, 2, 3, 4, 5, 6)).foldLeft(0)((tpl) => tpl._1 + tpl._2) shouldBe 21
    }
    "execute FoldLeft correctly on Empty Iterable and [acc] <: [elemTypes]" in {
      MyList[Float, IndexedSeq[Float]](ArrayBuffer.empty[Float]).foldLeft(0)((tpl) => tpl._1 + tpl._2) shouldBe 0
    }
  }

  "MyListBuffer" should {
    "execute filter correctly" in {
      MyListBuffer[Long](ListBuffer(1, 2, 3, 4, 5, 6)).filter(_ % 2 == 0).data shouldBe List(2, 4, 6)
    }
  }

  "MyIndexedList" should {
    "execute foldLeft correctly" in {
      MyIndexedList[Float](ArrayBuffer.empty[Float]).foldLeft(0)((tpl) => tpl._1 + tpl._2) shouldBe 0
    }
  }

}

package lectures.matching

import lectures.matching.SortingStuff._
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, WordSpec}

import scala.util.Random


/**
  * Короткий список самых востребованных генераторов:
  * Gen.alphaString
  * Gen.delay
  * Gen.oneOf
  * Gen.resultOf
  * Gen.zip
  * Gen.map
  * Gen.suchThat
  * Gen.mapOf
  * Gen.pic
  * Gen.choose
  *
  * Допишите 2 теста:
  * Для "find knife" теста создайте генератор, Option[Knife]. Тест должен показать, что если нож есть в вещах,
  * то метод findMyKnife его отыщет.
  *
  * Для "put boots ..." создайте генератор и проверьте правильность работы метода sortJunk по аналогии с предыдущими тестами.
  *
  */

class SortingStuffGeneratorBasedTest extends WordSpec with Matchers with PropertyChecks {

  val cheepWatchGen: Gen[Watches] = Gen.zip(Gen.choose(0f, 1000f), Gen.alphaStr).map(w => Watches(w._2, w._1))
  val bookGenerator = Gen.alphaStr.map(name => Book(name, Random.nextBoolean()))
  val interestingBookGen = bookGenerator.filter(_.isInteresting)
//  val knifeGen = Gen.oneOf(Some(Knife), None)
  val knifeOrStuffGen: Gen[Stuff] =  Gen.alphaStr.flatMap(name => Gen.oneOf(Knife, Book(name, Random.nextBoolean())))
  val stuffGen = Gen.listOfN(100, Gen.oneOf(cheepWatchGen, bookGenerator))
  val brandBootsGen = Gen.oneOf("Adidas", "Converse").map(b => Boots(b))
  val chineseBootsGen = Gen.alphaStr.map(b => Boots(b))

  // Override configuration if you need
  implicit override val generatorDrivenConfig =
    PropertyCheckConfig(minSize = 10, maxSize = 20)

  val get: AfterWord = new AfterWord("have")

  "This test" should get {
    "proper cheep watch generator" in {
      forAll(cheepWatchGen) { (watch: Watches) => {
        watch.cost should be <= 1000f
      }
      }
    }
    "proper interesting book generator" in {
      val books = interestingBookGen
      forAll(books) { (book: Book) => {
        book shouldBe 'interesting
      }
      }
    }
  }

  "Sort stuff" should {
    "return collections" which {
      "total size is equal to item amount" in {
        val ms = generatorDrivenConfig.minSuccessful

        val books = (1 to ms) flatMap { _ => interestingBookGen.sample }
        val watches = (1 to ms) flatMap { _ => cheepWatchGen.sample }

        val StuffBox(goodBooks, niceWatches, _, junk) = SortingStuff.sortJunk(Random.shuffle(books ++ watches).toList)
        goodBooks should have size books.size
        niceWatches should have size 0
        junk should have size watches.size
      }
    }
    "find knife" which {
      "was occasionally disposed" in {
//        forAll(knifeGen, stuffGen) { (mayBeKnife: Option[Knife.type], stuff) => {
//          mayBeKnife match {
//            case Some(knife) => ((SortingStuff.sortJunk _ andThen SortingStuff.findMyKnife)
//              (knife +: stuff)) shouldBe true
//            case None => ((SortingStuff.sortJunk _ andThen SortingStuff.findMyKnife)
//              (stuff)) shouldBe false
//          }
//        }
        forAll(knifeOrStuffGen) { (stuff: Stuff) =>
          stuff match {
            case Knife => ((SortingStuff.sortJunk _ andThen SortingStuff.findMyKnife)
              (List(stuff))) shouldBe true
            case _ => ((SortingStuff.sortJunk _ andThen SortingStuff.findMyKnife)
              (List(stuff))) shouldBe false
          }
        }
      }
    }

    "put boots in a proper place" when {
      "boots were produced by Converse or Adidas" in {
        forAll(Gen.listOfN(100, brandBootsGen), Gen.listOfN(100, chineseBootsGen), stuffGen) {
          (brandBoots, chineseBoots, stuff) => {
            val StuffBox(_, _, goodBoots, junk) =
              SortingStuff.sortJunk(Random.shuffle(brandBoots ++ chineseBoots ++ stuff))
            goodBoots should contain theSameElementsAs brandBoots
            for(chineseBoot <- chineseBoots) {
              goodBoots should not contain (chineseBoot)
              junk should contain (chineseBoot)
            }
          }
        }
      }
    }
  }
}

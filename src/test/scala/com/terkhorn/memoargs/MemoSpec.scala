package com.terkhorn.memoargs

import org.scalatest.{BeforeAndAfterEach, Matchers, WordSpecLike}

import scala.collection.mutable

class MemoSpec extends WordSpecLike with Matchers with BeforeAndAfterEach {

  val nastyVar = mutable.Map.empty[String, Int]

  override def beforeEach() {
    nastyVar.keys.foreach {
      nastyVar.remove
    }
  }

  "Memo" when {
    "annotating a simple method" should {
      val TTLMillis = 10L

      @Memo(TTLMillis)
      def expensiveMethod(helloThere: String) = {
        nastyVar(helloThere) = nastyVar.getOrElse(helloThere, 0) + 1
        helloThere + helloThere
      }


      "evaluate the @Memo result only once, when called with the same arguments twice, and when the second evaluation happens within the TTL time" in {

        val result0 = expensiveMethod("bar ")

        result0 shouldBe "bar bar "
        nastyVar("bar ") shouldBe 1

        val result1 = expensiveMethod("bar ")

        result1 shouldBe "bar bar "
        nastyVar("bar ") shouldBe 1
      }


      "evaluate the @Memo result TWICE, when called with the same arguments twice, and when the second evaluation happens OUTSIDE the TTL time" in {
        val result0 = expensiveMethod("grub ")

        result0 shouldBe "grub grub "
        nastyVar("grub ") shouldBe 1

        Thread.sleep(15)

        val result1 = expensiveMethod("grub ")

        result1 shouldBe "grub grub "
        nastyVar("grub ") shouldBe 2
      }


      "allow evaluation of the function when different arguments are passed" in {
        val result0 = expensiveMethod("qux ")

        result0 shouldBe "qux qux "
        nastyVar("qux ") shouldBe 1

        val result1 = expensiveMethod("zib ")

        result1 shouldBe "zib zib "
        nastyVar("zib ") shouldBe 1
      }
    }

    "annotating a CPU-intensive method" should {
      val ExpectedTime = 1000

      @Memo(100000)
      def takesForever() = {
        Thread.sleep(ExpectedTime)
        true
      }

      "allow the original method to execute on the first pass" in {
        lazy val start = System.currentTimeMillis()
        println(s"start $start")
        val result = takesForever()

        result shouldEqual true

        System.currentTimeMillis() >= (start + ExpectedTime) shouldBe true
        System.currentTimeMillis() <= (start + (ExpectedTime * 2)) shouldBe true


      }

      "use the memoized value on a subsequent pass... AND QUICKLY!" in {
        lazy val start = System.currentTimeMillis()
        val result = takesForever()

        result shouldEqual true
        System.currentTimeMillis() >= start + ExpectedTime shouldBe false
        System.currentTimeMillis() <= start + 10 shouldBe true

      }
    }

  }
}

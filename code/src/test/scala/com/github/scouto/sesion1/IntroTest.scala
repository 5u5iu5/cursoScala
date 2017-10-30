package com.github.scouto.sesion1

/**
  * Created by scouto.
  */

import com.github.scouto.MyApp.{factorial, max, second, sum}
import org.scalatest._

class IntroTest extends FlatSpec with Matchers {



  "sum" should "work with both natural numbers" in {
    sum(2,3) should be (5)
  }

  "factorial" should "be 1 for any integer below 0" in {
    factorial(-5) should be (1)
    factorial(-1) should be (1)
  }

  "max" should "return max number from a list" in {
    max(1 :: 5 :: 9 :: 2 :: 1 :: Nil) shouldBe(9)
  }

  "second" should "return second element from list" in {
    second(1 :: 5 :: 9 :: 2 :: 1 :: Nil) shouldBe(Some(5))
    second(1 :: Nil) shouldBe(None)
  }
}

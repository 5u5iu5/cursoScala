package com.github.apozo.sesion10

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import com.github.apozo.sesion10.Sesion10._


/**
  * Created by scouto.
  */
class Sesion10Test extends FlatSpec with Matchers with PropertyChecks {


  "myIf" should "return a proper value in both onTrue and onFalse are valid" in {
    myIf(cond = true, "Hello", "Bye") should be("Hello")
    myIf(cond = false, "Hello", "Bye") should be("Bye")
  }

  it should "not evaluate the unused statement" in {
    myIf(cond = true, "Hello", throw new Exception("failure")) should be("Hello")
    myIf(cond = false, throw new Exception("failure"), "Bye") should be("Bye")
  }

  it should "raise an exception" in {
    assertThrows[Exception](myIf(cond = false, "Hello", throw new Exception("failure")))
    assertThrows[Exception]{myIf(cond = true, throw new Exception("failure"), "Bye")}
  }


}

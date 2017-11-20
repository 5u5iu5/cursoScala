package com.github.apozo.sesion10

/**
  * Created by scouto.
  */
object Sesion10 extends App{


  def duplicateStrict(x: Double): Double = {
    println("hello")
    val result = x*2
    println(result)
    result
  }

  def duplicateNonStrict(x: => Double): Double = {
    println("hello")
    val result = x*2
    println(result)
    result
  }

  //duplicateNonStrict(10+11)
  //duplicateNonStrict(sys.error("failure"))

  def myIf[A](cond: Boolean, onTrue: => A, onFalse: => A): A = {
    if (cond) onTrue else onFalse
  }

}

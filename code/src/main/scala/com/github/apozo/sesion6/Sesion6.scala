package com.github.apozo.sesion6

/**
  * Created by scouto.
  */
object Sesion6 extends App {

  def something() = {
    println("calling something")
    1
  }


  def callByValue(x: Int) = {
    println("callByValue")
    println("1st time x: " + x)
    println("2nd time x: " + x)
  }

  def callByName(x: => Int) = {
    println("callByName")
    println("1st time x: " + x)
    println("2nd time x: " + x)
  }

  callByValue(something())
  println()
  println()
  println()
  println()
  callByName(something())


  //Resultado esperado
  //  calling something
  //    callByValue
  //  1st time x: 1
  //  2nd time x: 1
  //
  //  callByName
  //  calling something
  //    1st time x: 1
  //  calling something
  //    2nd time x: 1

}

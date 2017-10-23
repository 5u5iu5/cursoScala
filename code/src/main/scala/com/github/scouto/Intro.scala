package com.github.scouto

/**
  * Created by scouto.
  */
object MyApp extends App {
  println("Hello World ")

  def sum(x: Int, y: Int): Int = {
    x + y
  }

  def mult(x: Int, y: Int): Int = {
    x * y
  }

  def substract(x: Int, y: Int): Int = {
    x - y
  }

  def divide(x: Int, y: Int): Int = {
    x / y
  }

  def operate(x: Int, y: Int)(f: (Int, Int) => Int) = f(x, y)


  def wrongFactorial(n: Int): Int = {
    if (n <= 0) 1
    else n * factorial(n - 1)
  }

  def factorial(n: Int): Int = {

    @annotation.tailrec
    def rec(acc: Int, current: Int): Int = {
      if (current <= 0) acc
      else rec(acc * current, current - 1)

    }

    rec(1, n)
  }


  def operate[A, B](x: A, y: A)(f: (A, A) => B) = f(x, y)

  def max(list: List[Int]): Int = {
    var numMax = 0

    def findHigherNum(list: List[Int], currentMax: Int): Int = {
      if (list.size > 0) {
        val valueOfList = list.last
        val minList = list.dropRight(1)
        if (valueOfList > currentMax) {
          numMax = valueOfList
        }
        findHigherNum(minList, numMax)
      }
      numMax
    }

    findHigherNum(list, 0)
  }

  def second(list: List[Int]): Int = ???

  def nth(list: List[Int], n: Int): Int = ???


}









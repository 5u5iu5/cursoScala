package com.github.apozo.sesion12

/**
  * Created by couto on 5/07/17.
  */

trait RNG {
  def nextInt: (Int, RNG)
}


object RNG {

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = SimpleRNG(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }


  def nonNegativeInt(rng: RNG): (Int, RNG) = ???

  def double(rng: RNG): (Double, RNG) = ???

  def intDouble(rng: RNG): ((Int, Double), RNG) = ???

  def doubleInt(rng: RNG): ((Double, Int), RNG) = ???

  def double3(rng: RNG): ((Double, Double, Double), RNG) = ???

  def ints(n: Int)(rng: RNG): (List[Int], RNG) = ???


  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = ???

  def unit[A](a: A): Rand[A] = ???

  def map[A,B](s:Rand[A])(f: A => B): Rand[B] = ???

  def nonNegativeEven: Rand[Int] = ???

  def doubleMap: Rand[Double] = ???

  def map2[A,B, C](ra:Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???

  def  both[A, B](ra:Rand[A], rb: Rand[B]): Rand[(A,B)] = ???
  def intDoubleBoth: Rand[(Int, Double)] = ???

  def doubleIntBoth: Rand[(Double, Int)] = ???




}









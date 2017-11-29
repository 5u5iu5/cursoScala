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


  //Sesion 13
  def sequence[A](l: List[Rand[A]]): Rand[List[A]] = ???

  def intsBySeq(n: Int): Rand[List[Int]] = ???


  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???

  //Primera version => Que pasa si MaxInt no es divisiblepor max? Numeros menores aparecerán mas veces
  //Cuando nonNegativeInt genere numeros mas grandes que el multiplo mas grande de max hay que repetirla llamada
  def nonNegativeLessThan1(max: Int): Rand[Int] = {
    map(nonNegativeInt)(_ % max)
  }

  //  //Segunda version => Necesitamos un RNG, debemos pasarselo explicito en lugar de usando map
  //  def nonNegativeLessThan2(max: Int): Rand[Int] = {
  //    map(nonNegativeInt){i =>
  //      val mod = i % max
  //      if (i + (max -1) -mod >= 0) mod else nonNegativeLessThan2(max)
  //    }
  //  }

  //Tercera version => OK Pero es un poco peñazo
  def nonNegativeLessThan2(max: Int): Rand[Int] = {
    rng => {
      val (i, rng2) = nonNegativeInt(rng)
      val mod = i % max
      if (i + (max - 1) - mod >= 0) (mod, rng2) else nonNegativeLessThan2(max)(rng2)
    }
  }


  //Cuarta version => La buena
  def nonNegativeLessThan(max: Int): Rand[Int] = {
    flatMap(nonNegativeInt) {
      i =>
        val mod = i % max
        if (i + (max - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(max)
    }
  }

  def mapFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = ???

  def map2FlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???


  def generalMap[S, A, B](s: S => (A, S))(f: A => B): S => (B, S) = ???

  //  type State[S, +A] = S => (A,S)

}


case class State[S, +A](run: S => (A,S)) {

  def map[B](f: A => B): State[S, B] = ???


  def map2[B, C](s2: State[S, B])(f: (A, B) => C): State[S, C] = ???

  def flatMap[B](f: A => State[S, B]): State[S, B] = ???
}

object State {
  def unit[S, A](a: A): State[S, A] = ???

  def sequence[S, A](l: List[State[S, A]]): State[S, List[A]] = ???

  type newRand[+A] = State[RNG, A]

  val int: newRand[Int] = ???

  def ints(n: Int): newRand[List[Int]] = ???


}









package com.github.apozo.sesion7

/**
  * Created by couto on 27/06/17.
  */
sealed trait Arbol[+A]
case class Hoja[A](value: A) extends Arbol[A]
case class Rama[A](left: Arbol[A], right: Arbol[A]) extends Arbol[A]


object Arbol {

  def size[A] (t: Arbol[A]): Int = ???

  def maximum (t: Arbol[Int]): Int = ???

  def depth[A] (t:Arbol[A]) : Int = ???

  def map[A, B] (t:Arbol[A])(f:A => B): Arbol[B] = ???

  def fold[A, B] (t:Arbol[A])(f: A=> B)(g: (B, B) => B): B = ???

  def sizeFold[A] (t: Arbol[A]): Int = ???


  def maximumFold (t: Arbol[Int]): Int = ???

  def depthFold[A] (t:Arbol[A]) : Int = ???

  def mapFold[A, B] (t:Arbol[A])(f:A => B): Arbol[B] = ???
}


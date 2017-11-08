package com.github.apozo.sesion4

case class CaseClassExample(arg1: String, arg2: Int) {
  def printThis = println(arg1 + " - " + arg2)

  override def toString: String = arg1 + " - " + arg2
}

package com.github.apozo.sesion3

class Person(private val _name: String, private var _age: Int = 0) {

  def name = _name

  def age = _age

  def age_(newAge: Int) = _age = newAge

}

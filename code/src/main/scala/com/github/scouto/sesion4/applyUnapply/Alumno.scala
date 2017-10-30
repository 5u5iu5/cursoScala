package com.github.scouto.sesion4.applyUnapply

object Alumno {
  def apply(_nombre: String, _apellidos: String): Alumno = new Alumno(
    _nombre, _apellidos
  )

  def unapply(arg: Alumno): Option[(String, String)] =
    Some(arg._nombre, arg._apellidos)
}

class Alumno(val _nombre: String, val _apellidos: String) {

}

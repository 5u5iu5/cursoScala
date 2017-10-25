package com.github.scouto.sesion4

object Asignatura {
  def apply(nombre: String, limiteAlumnos: Int = 30, descripcion: String): Asignatura = new Asignatura(
    nombre, limiteAlumnos, descripcion
  )

  def unapply(arg: Asignatura): Option[(String, Int, String)] =
    Some(arg.nombre, arg.limiteAlumnos,arg.descripcion )
}

class Asignatura(val nombre: String, val limiteAlumnos: Int = 30, val descripcion: String) {

}

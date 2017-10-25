package com.github.scouto.sesion4

object Administracion{
  def apply(relacionAlumnos: Map[Asignatura, List[Alumno]] = Map()): Administracion =
    new Administracion(relacionAlumnos)

  def unapply(arg: Administracion): Option[(Map[Asignatura, List[Alumno]])] =
    Some(arg.relacionAlumnos)
}

/**
  * Created by scouto.
  */
class Administracion(val relacionAlumnos: Map[Asignatura, List[Alumno]] = Map()) {


  /**
    * Debe dar de alta un alumno si no supera el máximo y el alumno no está ya presente
    *
    * @param alumno
    * @param asignatura
    * @return
    */
  def alta(alumno: Alumno, asignatura: Asignatura): Option[Administracion] = {
    relacionAlumnos.getOrElse(asignatura, List()) match {
      case Nil => Some(new Administracion(relacionAlumnos + (new Asignatura(asignatura.nombre,
        asignatura.limiteAlumnos, asignatura.descripcion) -> List(alumno))))
      case alumnos if alumnos.contains(alumno) => None
      case alumnos if alumnos.size < asignatura.limiteAlumnos =>
        Some(new Administracion(relacionAlumnos + (asignatura -> (alumnos.::(alumno)))))
      case _ => None
    }
  }

  /**
    * Debe dar de baja un alumno o levantar un error si no es posible
    *
    * @param alumno
    * @param asignatura
    * @return
    */
  def baja(alumno: Alumno, asignatura: Asignatura): Either[String, Administracion] =
    relacionAlumnos.getOrElse(asignatura, List()) match {
      case Nil => new Left("Alumno no inscrito")
      case alumnos if alumnos.contains(alumno) =>
          new Right(new Administracion(relacionAlumnos + (asignatura -> alumnos.filterNot(p => p == alumno))))
      case _ => new Left("Alumno no inscrito")
    }

}

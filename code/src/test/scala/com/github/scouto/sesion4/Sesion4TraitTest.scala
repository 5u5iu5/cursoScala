package com.github.scouto.sesion4


import com.github.scouto.sesion4.traitsEjercicios.AdministracionTrait
import com.github.scouto.sesion4.traitsEjercicios.alumno.{AlumnoNuevo, AlumnoRepetidor, AlumnoTrait}
import com.github.scouto.sesion4.traitsEjercicios.asignatura.{AsignaturaConPrioridad, AsignaturaSinPrioridad, AsignaturaTrait}
import org.scalatest.{FlatSpec, Matchers}


class Sesion4TraitTest extends FlatSpec with Matchers{
  val manjarin: AlumnoTrait = AlumnoRepetidor("Javier", "Manjarin")
  val claudio: AlumnoTrait  = AlumnoRepetidor(nombre = "Claudio", apellidos = "Barragan")
  val alfredo: AlumnoTrait   = AlumnoRepetidor(apellidos = "Santaelena", nombre = "Alfredo")
  val fran: AlumnoTrait  = AlumnoRepetidor("Fran", "González")
  val diego: AlumnoTrait  = AlumnoNuevo("Diego", "Tristan")
  val djalma: AlumnoTrait  = AlumnoNuevo("Djalminha", "Feitosa")
  val turu: AlumnoTrait  = AlumnoNuevo("turu", "Flores")

  val asignaturaScala: AsignaturaTrait = AsignaturaSinPrioridad(
    nombre = "curso scala",
    descripcion = Some("Curso impartido en Amaris"),
    plazas = 3)


  val asignaturaJava: AsignaturaTrait = AsignaturaConPrioridad(
    nombre = "curso java",
    descripcion = Some("Curso impartido en Amaris"),
    plazas = 2)

  "Administracion trait" should "permitir inscribirse un repetidor si hay plazas en una asignatura sin prioridad" in {
    val adm = AdministracionTrait(Map(asignaturaScala -> List(manjarin, claudio), asignaturaJava -> List()))

    adm.alta(alfredo, asignaturaScala).get.relacionAlumnos should be (Map(asignaturaScala -> List(alfredo, manjarin, claudio),  asignaturaJava -> List()))

  }

  it should "permitir inscribirse un repetidor si hay plazas en una asignatura con prioridad" in {
    val adm = AdministracionTrait(Map(asignaturaScala -> List(manjarin, claudio), asignaturaJava -> List()))

    adm.alta(alfredo, asignaturaJava).get.relacionAlumnos should be (Map(asignaturaScala -> List(manjarin, claudio),  asignaturaJava -> List(alfredo)))

  }

  it should "permitir inscribirse un alumno nuevo si hay plazas en una asignatura con prioridad" in {
    val adm = AdministracionTrait(Map(asignaturaScala -> List(manjarin, claudio), asignaturaJava -> List()))

    adm.alta(diego, asignaturaJava).get.relacionAlumnos should be (Map(asignaturaScala -> List(manjarin, claudio),  asignaturaJava -> List(diego)))
  }

  it should "permitir inscribirse un alumno nuevo si hay plazas en una asignatura sin prioridad" in {
    val adm = AdministracionTrait(Map(asignaturaScala -> List(manjarin, claudio), asignaturaJava -> List()))

    adm.alta(diego, asignaturaScala).get.relacionAlumnos should be (Map(asignaturaScala -> List(diego, manjarin, claudio),  asignaturaJava -> List()))

  }

  it should "permitir inscribirse un alumno nuevo si hay repetidor aunque no haya plazas en una asignatura con prioridad" in {
    val adm = AdministracionTrait(Map(asignaturaScala -> List(alfredo), asignaturaJava -> List(fran, manjarin)))

    adm.alta(diego, asignaturaJava).get.relacionAlumnos should be (Map(asignaturaScala -> List(alfredo),  asignaturaJava -> List(diego, manjarin)))

  }

  it should "rechazar la inscripcion de un alumno nuevo si no hay repetidores ni plazas en una asignatura con prioridad" in {
    val adm = AdministracionTrait(Map(asignaturaScala -> List(alfredo), asignaturaJava -> List(djalma, turu)))

    adm.alta(diego, asignaturaJava) should be (None)
  }

  it should "rechazar la inscipcion si ya está inscrito" in {
    val adm = AdministracionTrait(Map(asignaturaScala -> List(manjarin, claudio), asignaturaJava -> List()))

    adm.alta(manjarin, asignaturaScala) should be (None)
  }

  it should "rechazar la inscipcion si no quedan plazas" in {
    val adm = AdministracionTrait(Map(asignaturaScala -> List(manjarin, claudio, alfredo), asignaturaJava -> List(fran)))

    adm.alta(fran, asignaturaScala) should be (None)
  }

  it should "levantar un mensaje de error si el alumno no estaba inscrito" in {
    val adm = AdministracionTrait(Map(asignaturaScala -> List(manjarin, claudio, alfredo)))
    val result = adm.baja(fran, asignaturaScala)

    result should be (Left("Alumno no inscrito"))
  }

  it should "permitir si el alumno esta presente" in {
    val adm = AdministracionTrait(Map(asignaturaScala -> List(manjarin, claudio, alfredo), asignaturaJava -> List(fran)))
    val result = adm.baja(manjarin, asignaturaScala)

    result.right.get.relacionAlumnos(asignaturaScala).size should be (2)
    result.right.get.relacionAlumnos should be (Map(asignaturaScala -> List(claudio, alfredo), asignaturaJava -> List(fran)))
  }
}


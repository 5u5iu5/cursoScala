package com.github.apozo.sesion4


import com.github.apozo.sesion4.caseclasses.{AdministracionCC, AlumnoCC, AsignaturaCC}
import org.scalatest.{FlatSpec, Matchers}


class Sesion4CCTest extends FlatSpec with Matchers{
  val manjarin = AlumnoCC("Javier", "Manjarin")
  val claudio  = AlumnoCC(nombre = "Claudio", apellidos = "Barragan")
  val alfredo   = AlumnoCC(apellidos = "Santaelena", nombre = "Alfredo")
  val fran  = AlumnoCC("Fran", "González")

  val asignaturaScala = AsignaturaCC(
    nombre = "curso scala",
    descripcion = Some("Curso impartido en Amaris"),
    plazas = 3)


  val asignaturaJava = AsignaturaCC(
    nombre = "curso java",
    descripcion = Some("Curso impartido en Amaris"),
    plazas = 2)

  "Administracion CC" should "permitir inscribirse si hay plazas" in {
    val adm = AdministracionCC(Map(asignaturaScala -> List(manjarin, claudio), asignaturaJava -> List()))
    val optAdm = adm.alta(alfredo, asignaturaScala)

    {adm.alta(alfredo, asignaturaScala).get match {
      case AdministracionCC(m) if m == Map(asignaturaScala -> List(alfredo, manjarin, claudio),  asignaturaJava -> List()) => true
      case _ => false
    }} should be (true)

  }

  it should "rechazar la inscipcion si ya está inscrito" in {
    val adm = AdministracionCC(Map(asignaturaScala -> List(manjarin, claudio), asignaturaJava -> List()))

    adm.alta(manjarin, asignaturaScala) should be (None)
  }

  it should "rechazar la inscipcion si no quedan plazas" in {
    val adm = AdministracionCC(Map(asignaturaScala -> List(manjarin, claudio, alfredo), asignaturaJava -> List(fran)))

    adm.alta(fran, asignaturaScala) should be (None)
  }

  it should "levantar un mensaje de error si el alumno no estaba inscrito" in {
    val adm = AdministracionCC(Map(asignaturaScala -> List(manjarin, claudio, alfredo)))
    val result = adm.baja(fran, asignaturaScala)

    result should be (Left("Alumno no inscrito"))
  }

  it should "permitir si el alumno esta presente" in {
    val adm = AdministracionCC(Map(asignaturaScala -> List(manjarin, claudio, alfredo), asignaturaJava -> List(fran)))
    val result = adm.baja(manjarin, asignaturaScala)

    result.right.get.relacionAlumnos(asignaturaScala).size should be (2)
    result.right.get.relacionAlumnos should be (Map(asignaturaScala -> List(claudio, alfredo), asignaturaJava -> List(fran)))
  }
}


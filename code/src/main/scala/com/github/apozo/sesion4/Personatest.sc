import com.github.apozo.sesion4.applyUnapply.Persona

val p: Persona = Persona("Alvaro", "Del Pozo", 36)

println(p.toString)

p match {
  case Persona("Federico", "garcia", 45) => println("FEDE")
  case Persona("Alvaro", "Del Pozo", 36) => println("ALVARO")
}
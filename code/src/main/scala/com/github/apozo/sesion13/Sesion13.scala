package com.github.apozo.sesion13

/**
  * Created by scouto.
  */
object Sesion13  extends App{


  //PREGUNTA 1
  //I am new to Scala and I am trying to execute = following code in Scala
  case class FoldExp(firstname : String, lname : String, age : Int, sex : String)

  //  defined class FoldExp

  object Foo{
    def apply(firstname : String, lname : String, age : Int, sex : String) = new FoldExp(firstname,lname,age,sex)
  }

  //  defined object Foo

  val foldList = List(Foo("Hugh", "Jass", 23, "male"), Foo("Hugh", "Jass", 25, "male"),Foo("Biggus"," Dickus", 43, "male"),Foo("Incontinentia", "Buttocks", 37, "female"))

  //  val secTry = foldList.foldLeft(List[String]()){(w,f) =>
  //    val comment = f.age match{
  //      case (f.age == 25) => "Band A"
  //      case (f.age > 30) => "Band B"
  //    }
  //    w:+ s"${f.firstname},${f.age} $comment"
  //
  //  }
  //
  //  The above block of code threw following error:
  //  <console>:13: error: not found: value ==
  //    case (f.age == 25) => "Band A"
  //    ^
  //    <console>:14: error: not found: value >
  //      case (f.age > 30) => "Band B"
  //      ^
  //      <console>:15: error: not found: value >
  //        case (f.age > 50) => "Band D"
  //
  //        I want to categorize people
  //        in the list into their respective
  //        bands based on their age.
  //        But iam not able to achieve this using pattern matching.
  //        Could anyone tell me why the above approach is wrong and what is the method to be
  //        followed to achieve my objective.
  //        Any attempt to find solution to the above problem is appreciated. Thanks in advance.





  val secTry1 = foldList.foldLeft(List[String]()) { (w, f) =>
    val comment = f match {
      case FoldExp(_, _, 25, _) => "Band A"
      case FoldExp(_, _, a, _) if a > 50 => "Band D"
      case FoldExp(_, _, a, _) if a > 30 => "Band B"
      case _ => "Band Default"
    }
    w :+ s"${f.firstname},${f.age} $comment"
  }

  val secTry2 = foldList.foldLeft(List[String]()) { (w, f) =>
    val comment = f.age match {
      case 25 => "Band A"
      case a if a > 50 => "Band D"
      case a if a > 30 => "Band B"
      case _ => "Band Default"
    }
    w :+ s"${f.firstname},${f.age} $comment"
  }

  secTry1.foreach(println)
  println("\n\n")
  secTry2.foreach(println)





  //PREGUNTA 2
  /*
  In MyMap = scala.collection.mutable.Map[List[String], Int]()

  I have to match string inside MyMap, and get the value of corresponding Int.
  List of string represent topic of interest of my device, int is ID of device.
*/

  val myMap: Map[List[String], Int] = Map(List("hello") -> 1, List("hello", "bye") ->2 , List("good", "bye") -> 3)


  //  val result = myMap.foldRight(List[Int]())((elem, acc) => if (elem._1.contains("hello")) elem._2::acc else acc)
  //  val result = myMap.filter(_._1 contains "hello" ).map(x => x._2).foreach(println)
  val result = myMap.filterKeys(_.contains("hello")).values

  result.foreach(println)



  // PREGUNTA 3
  //  Usa zipWith para construir un  funcion zip donde la salida sea un Streamde tuplas con cada uno de los elementos de las listas originales.
  //  Ejemplo: Stream(1,2,3)
  //           Stream("a","b", "c")
  //  Salida: Stream((1, "a"), (2, "b"), (3, "c")



  // PREGUNTA 4
  // Haz lo mismo con zipWithAll, la funcion se llamara zipAll
  //
  // PREGUNTA 5
  //  Usa zipAll para crear la funcion empiezaPor



  // PREGUNTA 6
  // Usa la funcion empiezaPor y la funcion tails para crear la funcion tieneSubsecuencia


}

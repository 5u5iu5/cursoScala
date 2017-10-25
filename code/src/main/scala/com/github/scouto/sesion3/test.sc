def composicionA(f: Int => String, g: Int => Int): Int => String = {
  a => f(g(a))
}

def composicionB(f: Int => String): Int => String = {
  a => f(a)
}

val list = List.range(1, 500).map(l => "Kafka - " + l )

def toString(num: Int) = num.toString

def process[A](filter:A=>Boolean)(list:List[A]):List[A] = {
  lazy val recurse = process(filter) _

  list match {
    case head::tail => if (filter(head)) {
      head::recurse(tail)
    } else {
      recurse(tail)
    }

    case Nil => Nil
  }
}

val even = (a:Int) => a % 2 == 0

val numbersAsc = 1::2::3::4::5::Nil
val numbersDesc = 5::4::3::2::1::Nil

process(even)(numbersAsc)   // [2, 4]
process(even)(numbersDesc)  // [4, 2]
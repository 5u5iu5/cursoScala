def composicionA(f: Int => String, g: Int => Int): Int => String = {
  a => f(g(a))
}

def composicionB(f: Int => String): Int => String = {
  a => f(a)
}

val list = List.range(1, 500).map(l => "Kafka - " + l )

def toString(num: Int) = num.toString
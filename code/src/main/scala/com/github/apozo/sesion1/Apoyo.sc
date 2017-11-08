val inputList: List[Int] = List(1, 3, 5)
inputList.foldLeft(0) { (acc, i) => acc + i }


def rotate(list: List[Int], x: Int): List[Int] = {
  val shift = x % list.size
  if( shift == 0 ) list
  else {
    val count = if( shift > 0 ) shift else list.size + shift
    list.drop(count) ::: list.take(count)
  }

}


val list1 = rotate(List.range(1,6), 3)
list1.foreach(println)
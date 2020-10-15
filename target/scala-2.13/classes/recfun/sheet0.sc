import scala.annotation.tailrec

def sum(f: Int => Int)(a: Int, b: Int): Int = {
  @tailrec
  def loop(a: Int, acc: Int): Int = {
    println(s"f(a) : ${f(a)} f(acc) : ${f(acc)}")
    if (a > b) acc
    else loop(a + 1, f(a) + acc)
  }

  loop(a, 0)
}

sum(x => x * x)(1, 3)
1 + 4 + 9
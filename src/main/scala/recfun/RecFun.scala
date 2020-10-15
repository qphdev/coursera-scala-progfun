package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
/*    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }

    println("\nBalance")
    val testArr0 = "((()))".toList
    val testArr1 = "(..(.(...)...).)".toList
    val testArr2 = "(.(..(...).)...))".toList
    val testArr3 = "(..((.(..)".toList

    for (ta <- List(testArr0, testArr1, testArr2, testArr3))
      println(s"${ta.mkString("")} : ${balance(ta)}")


    println(countChange(5, List(1, 2, 3)))
    println(countChange(5, List(1, 2)))
    println(countChange(10, List(1, 2, 3)))
    */
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  final def balance(chars: List[Char]): Boolean = {
    @tailrec
    def innerBalance(openingCount: Int, closingCount: Int, charArr: List[Char]): Boolean = {
      if (charArr.isEmpty)
        openingCount == closingCount

      else if (charArr.head == '(')
        innerBalance(openingCount + 1, closingCount, charArr.tail)

      else if (charArr.head == ')')
        innerBalance(openingCount, closingCount + 1, charArr.tail)

      else
        innerBalance(openingCount, closingCount, charArr.tail)
    }
    innerBalance(0, 0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def innerCountChange(money: Int, coins: List[Int]): Int = {
      if (money < 0 || coins.isEmpty) 0
      else if (money == 0) 1
      else
        innerCountChange(money, coins.tail) + innerCountChange(money - coins.head, coins)
    }

    innerCountChange(money, coins)
  }
}

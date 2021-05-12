package recfun

import scala.annotation.tailrec

object Main {

  def main(args: Array[String]) {
    println("Pascal's Triangle")
    print(formatPascalTriangle(10))
  }

  def formatPascalTriangle(n: Int): String =
    (0 to n)
      .map { row =>
        (0 to row).map(pascal(_, row)).mkString(" ")
      }
      .mkString("\n")

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || c == r) {
      1
    } else {
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    @tailrec def bracketTotal(chars: List[Char], total: Int): Boolean = chars match {
      case Nil      => total == 0
      case '(' :: t => bracketTotal(t, total + 1)
      case ')' :: t => if (total > 0) bracketTotal(t, total - 1) else false
      case _ :: t   => bracketTotal(t, total)
    }

    bracketTotal(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    @tailrec def countChangeHelper(money: Int, coins: List[Int], total: Int): Int =
      if (money < 0 || coins.isEmpty || coins.head == 0) {
        total
      } else if (money == 0) {
        total + 1
      } else {
        countChangeHelper(money, coins.tail, total + countChange(money - coins.head, coins))
      }

    countChangeHelper(money, coins, 0)
  }
}

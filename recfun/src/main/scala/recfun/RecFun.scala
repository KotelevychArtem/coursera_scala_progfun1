package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  // -- This is more mathematical (C(N, K)) approach, but for some reason it fails one of a symetry tests (maybe large numbers?) 
//  def pascal(c: Int, r: Int): Int = {
//    def mul(acc: Long, l: Long, r: Long): Long = {
//      if (l == r) acc * l
//      else if (l == 0) mul(acc, 1, r)
//      else mul(acc * l, l + 1, r)
//    }
//    if (c == 0 || c == r) 1 else (mul(1, c + 1, r) / mul(1, 1, r - c)).toInt
//  }

  /**
   * Exercise 2
   */

  // This could be done in a simplier way with tail recursion, 
  // but since we want to practice in pure recursion...
  def balance(chars: List[Char]): Boolean = {
    def seek(chars: List[Char]): List[Char] = {
      if (chars.isEmpty) chars
      else if (chars.head == '(') {
        val t = seek(chars.tail)
        if (t.isEmpty || t.head != ')') List('(')
        else if(t.nonEmpty) seek(t.tail)
        else t
      }
      else if (chars.head == ')') chars
      else seek(chars.tail)
    }

    chars.isEmpty || seek(chars).isEmpty
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def count(m: Int, c: List[Int]): Int = {
      if (m == 0) 1
      else if (c.isEmpty || m < 0) 0
      else {
        count(m - c.head, c) + count(m, c.tail)
      }
    }
    if (money > 0) count(money, coins) else 0
  }
}

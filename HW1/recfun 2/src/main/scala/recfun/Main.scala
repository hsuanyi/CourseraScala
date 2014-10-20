package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    def validCheck() : Boolean = {
      c <= r && r >= 0
    }
    
    if(validCheck()) 
      if(c == 0 || c == r) 
        1
      else 
    	pascal(c - 1, r - 1) + pascal(c, r - 1)
    else 
      throw new IllegalArgumentException()
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
	def isParen(x : Char) : Boolean = {
	  x == '(' || x == ')'
	}
    
	def balanceHelper(chars: List[Char], expRight : Int): Boolean = {
	  if(chars.isEmpty)
	    expRight == 0
	  else 
	    if(chars.head == '(')
	      balanceHelper(chars.tail, expRight + 1)
	    else 
	      if(expRight == 0)
	      	false
	      else
	        balanceHelper(chars.tail, expRight - 1)
	}
	
	balanceHelper(chars.filter(isParen(_)), 0)
  }
  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countChangeHelper(rest: Int, rest_coins: List[Int]): Int = {
      if(rest == 0)
        1
      else
        if(rest_coins.isEmpty || rest < rest_coins.head)
    	  0
        else 
          countChangeHelper(rest - rest_coins.head, rest_coins) + countChangeHelper(rest, rest_coins.tail)
    }
    
    if(money <= 0 || coins.isEmpty)
      0
    else
      countChangeHelper(money, coins.sorted)
  }
}

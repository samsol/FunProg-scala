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
		  //if (r==0 &&c==0) return 1; 
		  if (c==0||c==r)  1 else  pascal(c-1,r-1)+pascal(c,r-1) 
		  
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
  
	def balanceWithStackCount(chars :List[Char], currentStackCount:Int) :Boolean = {
	
	if (currentStackCount<0) false else 	
		if (chars.isEmpty && currentStackCount==0) true 
			else if (chars.isEmpty && currentStackCount!=0) false 
			else if (chars.head.toString == ")") balanceWithStackCount(chars.tail, currentStackCount-1) else 
				if (chars.head.toString() == "(" ) balanceWithStackCount(chars.tail, currentStackCount+1) else 
					balanceWithStackCount(chars.tail, currentStackCount)
		       
		
	}
	
	balanceWithStackCount(chars,0);
	
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money<0 || coins.isEmpty ) 0 else 
    	if (money==0) 1 
    	else	countChange(money,coins.tail) + countChange(money-coins.head,coins); 
  }
}

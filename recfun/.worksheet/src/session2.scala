import common._
object session2 {import scala.runtime.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(77); 
  println("Welcome to the Scala worksheet");$skip(192); 
  
  def sum(f: Int => Int)(a: Int, b: Int): Int = {
        def loop(a: Int, acc: Int): Int = {
          if (a>b) acc
          else loop(a+1, acc+f(a))
        }
        loop(a, 0)
      };System.out.println("""sum: (f: Int => Int)(a: Int, b: Int)Int""");$skip(31); val res$0 = 
      
      sum {x => x}(2,4);System.out.println("""res0: Int = """ + $show(res$0))}
}
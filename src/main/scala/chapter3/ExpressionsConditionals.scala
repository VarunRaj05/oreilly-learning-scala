package chapter3



/** Given a string name, write a match expression that will return
 *  the same string if nonempty, or else the string "n/a" if it is
 *  empty.
 */

object ExpressionsConditionals1 extends App {
  val name: String = ""
  val out: String = name match {
    case in if in.size < 1 => "n/a"
    case in => in
  }
  println(out)
}


/** Given a double amount, write an expression to return "greater"
 *  if it is more than zero, "same" if it equals zero, and "less" if
 *  it is less than zero. Can you write this with if..else blocks?
 *  How about with match expressions?
 */

object ExpressionsConditionals2 extends App {
  val rangeStart: Double = -1
  val rangeEnd: Double = 1
  val rand: Double = rangeStart + (rangeEnd - rangeStart) * util.Random.nextDouble
  
  def withIfElse(in: Double): String = {
    if (in == 0)
      "same"
    else
      if (in < 0)
        "less"
      else
        "greater"
  }
  
  def withMatch(in: Double): String = {
    in match {
      case x if x > 0 => "greater"
      case x if x < 0 => "less"
      case x if x == 0 => "same"
    }
  }
  
  val out: String = {
    util.Random.nextInt(2) match {
      case 0 => withIfElse(rand)
      case 1 => withMatch(rand)
    }
  }
  
  println(out)
}


/** Write an expression to convert one of the input values cyan,
 *  magenta, yellow to their six-char hexadecimal equivalents
 *  in string form. What can you do to handle error conditions?
 */

object ExpressionsConditionals3 extends App {
  def toHex(col: String): String = {
    val cyan: String = "00FFFF"
    val magenta: String = "FF00FF"
    val yellow: String = "FFFF00"
    
    col match {
      case "cyan" => cyan
      case "magenta" => magenta
      case "yellow" => yellow
      case _ => "Error: Incorrect input provided"
    }
  }
  
  println(toHex("cyan"))
  println(toHex("magenta"))
  println(toHex("yellow"))
  println(toHex("red"))
}


/** Print the numbers 1 to 100, with each line containing a group
 *  of five numbers. For example,
 *  1, 2, 3, 4, 5,
 *  6, 7, 8, 9, 10
 *  ....
 */

object ExpressionsConditionals4 extends App {
  val upperBound: Int = 100
  
  for (i <- 1 to upperBound) {
    print(i)
    
    if (i < upperBound) {
      print(',')
    }
    
    if (i % 5 != 0)
      print(' ')
    else
      println
  }
}



/** Write an expression to print the numbers from 1 to 100, except
 *  that for multiples of 3, print "type", and for multiples of 5,
 *  print "safe". For multiples of both 3 and 5, print "typesafe"
 */


object ExpressionsConditionals5 extends App {
  val upperBound: Int = 100
  
  for (i <- 1 to upperBound) {
    i match {
      case x if x % 3 == 0 && x % (3 * 5) != 0 => print("type ")
      case x if x % 5 == 0 && x % (3 * 5) != 0 => println("safe")
      case x if x % (3 * 5) == 0 => println("typesafe")
      case _ => print(i + " ")
    }
  }
  println
}

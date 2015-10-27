package chapter4


/** Write a function that computes the area of a circle given its radius.
 */
 
object Functions1 extends App {
  def area(radius: Double): Double = {
    val r = radius.toDouble
    (22.toDouble / 7.toDouble) * r * r
  }
  
  val radius: Double = 4
  
  println(f"The area is ${area(radius)}%.2f")
}



/** Provide an alternate form of the function in exercise 1 that takes the
 *  radius as a String. What happens if your function is invoked with an
 *  empty String ?
 */


object Functions2 extends App {
  def area(radius: String): Double = {
    val r: Double = radius.toDouble
    val pi: Double = 22.toDouble / 7.toDouble
    pi * r * r
  }
  
  // coin flip generates an empty string or an integer between 1 and 9 as radius
  
  val radius: String = {
    util.Random.nextInt(2) match {
      case 0 => ""
      case 1 => { util.Random.nextInt(9) + 1 }.toString
    }
  }
  
  // with empty string passed to function, it returns this:
  // java.lang.NumberFormatException: empty String
  
  println(f"The area is ${area(radius)}%.2f")
}



/** Write a recursive function that prints the values from 5 to 50
 *  by fives, without using for or while loops. Can you make it
 *  tail-recursive?
 */


object Functions3 extends App {
  val range: (Int, Int) = (5, 50)
  mult5(range._1, range._2)
  
  @annotation.tailrec
  def mult5(floor: Int, ceiling: Int): Unit = {
    println(floor)
    if (floor < ceiling) {
      mult5(floor + 5, ceiling)
    }
  }
}



/** Write a function that takes a milliseconds value and returns a
 *  string describing the value in days, hours, minutes, and seconds.
 *  What is the optimal type for the input value?
 */

object Functions4 extends App {
  val num: Long = 434355643
  println(convert(num))
  
  def convert(ms: Long): String = {
    val s: Int = ((ms / 1000) % 60).toInt
    val m: Int = (((ms / 1000) / 60) % 60).toInt
    val h: Int = ((((ms / 1000) / 60) / 60) % 24).toInt
    val d: Int = ((((ms / 1000) / 60) / 60) / 24).toInt
    
    val s_unit: String = if (s == 1) "second" else "seconds"
    val m_unit: String = if (m == 1) "minute" else "minutes"
    val h_unit: String = if (h == 1) "hour" else "hours"
    val d_unit: String = if (d == 1) "day" else "days"
    
    s"$d $d_unit, $h $h_unit, $m $m_unit, and $s $s_unit"
  }
}



/** Write a function that calculates the first value raised to the
 *  exponent of the second value. Try writing this first using math.pow,
 *  then with your own calculation. Did you implement it with variables?
 *  Is there a solution available that only uses immutable data? Did you
 *  choose a numeric type that is large enough for your uses?
 */

object Functions5 extends App {
  val num1: Long = 2
  val num2: Long = 3
  
  mathPow(num1, num2)
  customPow(num1, num2)
  
  def mathPow(x: Long, y: Long): Unit = {
    println("Using math.pow function")
    println(x + " raised to the power of " + y + " = " + math.pow(x, y).toLong)
  }
  
  def customPow(x: Long, y: Long): Unit = {
    // random number to invoke loop solution or recursive function solution
    val flag: Int = { util.Random.nextInt(100) + 1 } % 2
    
    flag match {
      case 0 => {
        println("Using loop solution")
        withLoop(x, y)
      }
      case 1 => {
        println("Using loop recursive function solution")
        withRecursive(x, y)
      }
    }
    
    def withLoop(a: Long, b: Long): Unit = {
      var result: Long = a
      for (i <- 1 until b.toInt) {
        result *= a
      }
      println(num1 + " raised to the power of " + num2 + " = " + result)
    }
    
    @annotation.tailrec
    def withRecursive(a: Long, b: Long, c: Long = 1): Unit = {
      if (b < 1)
        println(num1 + " raised to the power of " + num2 + " = " + c)
      else
        withRecursive(a, b - 1, a * c)
    }
  }
}


/** Write a function that calculates the difference between a pair of 2D points (x and y)
 *  and returns the result as a point.
 */

object Functions6 extends App {
  val p1: (Int, Int) = (1, 4)
  val p2: (Int, Int) = (4, 12)
  
  println("The distance is " + dist(p1, p2))
  
  def dist(a: (Int, Int), b: (Int, Int)): Double = {
    val x_dist: Int = a._1 - b._1
    val y_dist: Int = a._2 - b._2
    
    math.sqrt(math.pow(x_dist, 2) + math.pow(y_dist, 2))
  }
}


/** Write a function that takes a 2-sized tuple and returns it with the Int value (if
 *  included) in the first position. Hint: this would be a good use for type parameters
 *  and the isInstanceOf type operation.
 */


object Functions7 extends App {
  val in: (String, Int) = ("Scala", 255)
  
  def intFirst[A, B](x: (A, B)): (Any, Any) = {
    if (!x._1.isInstanceOf[Int] && x._2.isInstanceOf[Int])
      (x._2, x._1)
    else
      x
  }
  
  println(intFirst(in))
}


/** Write a function that takes a 3-sized tuple and returns a 6-sized tuple, with each
 *  original parameter followed by its String representation. For example, invoking the
 *  function with (true, 22.25, "yes") should return (true, "true", 22.5, "22.5", "yes", "yes").
 *  Can you ensure that tuples of all possible types are compatible with your function?
 *  When you invoke this function, can you do so with explicit types not only in the function
 *  result but in the value that you use to store the result?
 */

object Functions8 extends App {
  val in: (Int, Boolean, Double) = (1223, false, 334.5) 
  val out: (Int, String, Boolean, String, Double, String) = convertTuple(in)
  
  println("1st element is of type " + out._1.getClass + " and value " + out._1)
  println("2nd element is of type " + out._2.getClass + " and value " + out._2)
  println("3rd element is of type " + out._3.getClass + " and value " + out._3)
  println("4th element is of type " + out._4.getClass + " and value " + out._4)
  println("5th element is of type " + out._5.getClass + " and value " + out._5)
  println("6th element is of type " + out._6.getClass + " and value " + out._6)
  
  def convertTuple[A, B, C](a: (A, B, C)): (A, String, B, String, C, String) = {
    val r = (a._1, a._1.toString, a._2, a._2.toString, a._3, a._3.toString)
    r
  }
}
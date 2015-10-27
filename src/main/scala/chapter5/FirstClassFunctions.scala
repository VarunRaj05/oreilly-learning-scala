package chapter5



/** Write a function literal that takes two integers and returns the higher number. Then
 *  write a higher-order function that takes a 3-sized tuple of integers plus this function
 *  literal, and uses it to return the maximum value in the tuple.
 */

object FirstClassFunctions1 extends App {
  val higher = (x: Int, y: Int) => if (x >= y) x else y
  
  def tupleMax(x: (Int, Int, Int), f: (Int, Int) => Int): Int = {
    f(f(x._1, x._2), x._3)
  }
  
  val tupleInt: (Int, Int, Int) = (332, 65, 111)
  
  println(tupleMax(tupleInt, higher))
}


/** The library function util.Random.nextInt returns a random integer. Use it to
 *  invoke the "max" function with two random integers plus a function that returns
 *  the larger of two given integers. Do the same with a function that returns the smaller
 *  of two given integers, and then a function that returns the second integer every time.
 */


object FirstClassFunctions2 extends App {
  val x: Int = util.Random.nextInt
  val y: Int = util.Random.nextInt
  
  println("x is " + x + ", and y is " + y)
  
  val max: Int = twoIntOps(x, y, (a, b) => if (a >= b) a else b)
  println("The max int is " + max)
  
  val min: Int = twoIntOps(x, y, (a, b) => if (a <= b) a else b)
  println("The min int is " + min)
  
  val second: Int = twoIntOps(x, y, (a, b) => b)
  println("The second int is " + second)
  
  def twoIntOps(x: Int, y: Int, f: (Int, Int) => Int): Int = {
    f(x, y)
  }
}



/** Write a higher-order function that takes an integer and returns a function. The
 *  returned function should take a single integer argument (say, "x") and return the
 *  product of x and the integer passed to the higher-order function.
 */


object FirstClassFunctions3 extends App {
  val prod = (n: Int) => {
    (x: Int) => n * x
  }
  
  val threeTimes = prod(3)
  val sixTimes = prod(6)
  
  println("3 times 4 is " + threeTimes(4))
  println("6 times 5 is " + sixTimes(5))
}



/** Let’s say that you happened to run across this function while reviewing another
 *  developer’s code:
 *  def fzero[A](x: A)(f: A => Unit): A = { f(x); x }
 *  What does this function accomplish? Can you give an example of how you might
 *  invoke it?
 */

object FirstClassFunctions4 extends App {
  def fzero[A](x: A)(f: A => Unit): A = { f(x); x }
  
  val intToString: Int = fzero[Int](util.Random.nextInt){
    i => println("After conversion, has class: " +i.toString.getClass)
  }
  
  println("The original int has this class: " + intToString.getClass)
}



/** There’s a function named "square" that you would like to store in a function value.
 *  Is this the right way to do it? How else can you store a function in a value?
 *  def square(m: Double) = m * m
 *  val sq = square
 */

object FirstClassFunctions5 extends App {
  def square(m: Double): Double = m * m
  val sq = square _
  
  println(sq(3))
}



/** Write a function called "conditional" that takes a value x and two functions, p and
 *  f, and returns a value of the same type as x. The p function is a predicate, taking the
 *  value x and returning a Boolean b. The f function also takes the value x and returns a
 *  new value of the same type. Your "conditional" function should only invoke the
 *  function f(x) if p(x) is true, and otherwise return x. How many type parameters will
 *  the "conditional" function require?
 */

object FirstClassFunctions6 extends App {
  def conditional[T](x: T, p: T => Boolean, f: T => T): T = {
    if (p(x)) f(x) else x
  }
  
  val intOp = conditional[Int](-87, i => i > 0, i => i/2)
  val stringOp = conditional[String]("Learning Scala", _.size > 10, _.substring(0,10))
  
  println(intOp)
  println(stringOp)
}


/** Do you recall the "typesafe" challenge from the exercises in Chapter 3? There is a
 *  popular coding interview question I’ll call "typesafe," in which the numbers 1-100
 *  must be printed one per line. The catch is that multiples of 3 must replace the
 *  number with the word "type," while multiples of 5 must replace the number with
 *  the word "safe." Of course, multiples of 15 must print "typesafe."
 *  Use the "conditional" function from exercise 6 to implement this challenge.
 *  Would your solution be shorter if the return type of "conditional" did not match
 *  the type of the parameter x? Experiment with an altered version of the "conditional"
 *  function that works better with this challenge.
 */


object FirstClassFunctions7 extends App {
  val conditional = (x: Int, p: Int => Boolean, f: Int => String) => {
    if (p(x)) f(x) else x.toString
  }
  
  val test = (x: Int) => {
    x % 3 == 0 || x % 5 == 0
  }
  
  val typesafe = (x: Int) => {
    x match {
      case n if n % 15 == 0 => "typesafe"
      case n if n % 5 == 0 => "safe"
      case n if n % 3 == 0 => "type"
    }
  }
  
  for (i <- 1 to 100) {
    println(conditional(i, test, typesafe))
  }
}
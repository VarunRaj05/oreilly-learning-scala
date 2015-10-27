package chapter2


/** Using the input value 2.7255, generate the string "You owe $2.73." Is this doable
 *  with string interpolation?
 */


object Data3 extends App {
  val in: Double = 2.7255
  
  println("You owe $" + f"${in}%.2f.")
}



/** Using the input string "Frank,123 Main,925-555-1943,95122" and
 *  regular expression matching, retrieve the telephone number.
 *  Can you convert each part of the telephone number to its own
 *  integer value? How would you store this in a tuple?
 */

object Data6 extends App {
  val frank: String = "Frank,123 Main,925-555-1943,95122"
  val regex = """^.*,(\d{3})-(\d{3})-(\d{4}),.*$""".r
  val regex(phone1,phone2,phone3): String = frank
  println(phone1.toInt)
  println(phone2.toInt)
  println(phone3.toInt)
  val phone: (Int, Int, Int) = (phone1.toInt, phone2.toInt, phone3.toInt)
  println(phone._1)
  println(phone._2)
  println(phone._3)
}
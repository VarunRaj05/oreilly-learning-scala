package chapter9


object SafeStringUtils {
  // Returns a trimmed version of the string wrapped in an Option,
  // or None if the trimmed string is empty.
  
  def trimToNone(s: String): Option[String] = {
    Option(s) map (_.trim) filterNot (_.isEmpty)
  }
  
  // converts numeric string to Int
  
  def stringToInt(s: String): Option[Int] = { util.Try(s.toInt) }.toOption
  
  // converts numeric string to Long
  
  def stringToLong(s: String): Option[Long] = {
    val string: String = {
      if (s matches """^\d+L$""") {
        s.replaceAll("L", "")
      }
      else {
        s
      }
    }
    
    { util.Try(string.toLong) }.toOption
  }
  
  // generates random string of upper and lower case letters for given size
  
  private lazy val letters = { { 'A' to 'Z' }.toList ::: { 'a' to 'z' }.toList } map { _.toString }
  
  def randomString(size: Int): Option[String] = {
    def pickLetter(n: Int): List[String] = {
      if (n > 0) {
        letters(util.Random.nextInt(52)) :: pickLetter(n - 1)
      } 
      else {
        Nil
      }
    }
    
    size match {
      case s if s > 0 => Some(pickLetter(size).mkString)
      case _ => None
    }
  }
}
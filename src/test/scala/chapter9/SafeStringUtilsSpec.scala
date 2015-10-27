package chapter9

import org.scalatest._

class SafeStringUtilsSpec extends FlatSpec with Matchers {
  "def trimToNone(s: String): Option[String]" should "return None if empty string is passed to it" in {
    SafeStringUtils.trimToNone("") should be (None)
  }
  
  it should "remove trailing space in string passed to it" in {
    SafeStringUtils.trimToNone("testing now   ") should equal (Some("testing now"))
    SafeStringUtils.trimToNone("testing now\n\n") should equal (Some("testing now"))
  }
  
  it should "remove leading spaces in string passed to it" in {
    SafeStringUtils.trimToNone("   testing now") should equal (Some("testing now"))
    SafeStringUtils.trimToNone("\n\ntesting now") should equal (Some("testing now"))
  }
  
  it should "handle nulls in string passed to it" in {
    SafeStringUtils.trimToNone(null) should be (None)
  }
  
  "def stringToInt(s: String): Option[Int]" should "return integer for numeric string passed to it" in {
    SafeStringUtils.stringToInt("45") should equal (Some(45))
  }
  
  it should "return None for non-numeric string passed to it" in {
    SafeStringUtils.stringToInt("45fortyfive") should be (None)
  }
  
  "def stringToLong(s: String): Option[Long]" should "return long for long string passed to it" in {
    SafeStringUtils.stringToLong("45L") should equal (Some(45L))
  }
  
  it should "return long for int string passed to it" in {
    SafeStringUtils.stringToLong("45") should equal (Some(45L))
  }
  
  it should "return None for non-numeric string passed to it" in {
    SafeStringUtils.stringToLong("45Lfortyfive") should be (None)
  }
  
  "def randomString(size: Int): Option[String]" should "return None when 0 is passed to it" in {
    SafeStringUtils.randomString(0) should be (None)
  }
  
  it should "match size of string with number passed to it" in {
    SafeStringUtils.randomString(9) map (_.size) should equal (Some(9))
    SafeStringUtils.randomString(1) map (_.size) should equal (Some(1))
  }
  
  it should "generate letters only for number passed to it" in {
    SafeStringUtils.randomString(9) map { _ matches """^[A-Za-z]{9}$""" } should equal (Some(true))
  }
}
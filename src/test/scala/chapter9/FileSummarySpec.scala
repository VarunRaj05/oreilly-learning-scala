package chapter9

import org.scalatest._
import FileSummary._

class FileSummarySpec extends FlatSpec with Matchers {
  val data = "Mary had a little lamb.\n\nAnd a goat.\n\nAnd a chicken."
  val para = List("Mary had a little lamb.", "And a goat.", "And a chicken.")
  val words = List("Mary", "had", "a", "little", "lamb.", "And", "a", "goat.","And", "a", "chicken.")
  val stats = Map("mary" -> 1, "had" -> 1, "a" -> 3, "little" -> 1, "lamb" -> 1, "and" -> 2, "goat" -> 1, "chicken" -> 1)
  val top = List("a", "and", "chicken", "goat", "had", "lamb", "little", "mary")
  
  
  "FileSummary object" should "identify paragraphs" in {
    identifyParagraphs(data) should equal (para)
  }
  
  it should "count paragraphs" in {
    countParagraphs(data) should equal (3)
  }
  
  it should "count characters" in {
    countCharacters(data) should equal (48)
  }
  
  it should "find words" in {
    findWords(data) should equal (words)
  }
  
  it should "count words" in {
    countWords(data) should equal (11)
  }
  
  it should "generate word stats" in {
    generateWordStats(data) should equal (stats)
  }
  
  it should "identify top words" in {
    identifyTopWords(data) should equal (top)
  }
}
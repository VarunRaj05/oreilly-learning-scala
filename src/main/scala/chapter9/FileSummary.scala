package chapter9


/** Write an application that summarizes a file. It will take a single text file as input and
 *  print an overall summary including the number of characters, words, and paragraphs
 *  as well as a list of the top 20 words by usage.
 *  
 *  The application should be smart enough to filter out non-words. Parsing a Scala file
 *  should reveal words, for example, and not special characters such as “{” or “//”. It
 *  should also be able to count paragraphs that have real content versus empty space.
 */


object FileSummary extends CharacterProcessor with WordProcessor {
  val path = """src\main\resources\"""
  
  def main(args: Array[String]) = {
    args.toList match {
      case filenames if filenames.nonEmpty => {
        val summaries: List[Summary] = filenames map {
          filename => {
            val file = io.Source.fromFile(s"""${ path }${ filename }""")
            val text = file.mkString
            file.close
            
            // create data object containing summary data for each file
            
            Summary(
                filename = filename,
                paragraphs = countParagraphs(text),
                characters = countCharacters(text),
                words = countWords(text),
                topWords = identifyTopWords(text))
          }
        }
        
        println("FILE SUMMARY -\n")
        
        summaries foreach {
          s => {
            println(s"Filename: ${ s.filename }")
            println(s"Paragraphs: ${ s.paragraphs }")
            println(s"Characters: ${ s.characters }")
            println(s"Words: ${ s.words }")
            println("\nTop Words:\n")
            println(s"${ s.topWords mkString "\n" }")
            println("\n")
          }
        }
      }
      case _ => println("File not provided")
    }
  }
}



case class Summary(
    filename: String,
    paragraphs: Int,
    characters: Int,
    words: Int,
    topWords: List[String])




trait WordProcessor extends ParagraphProcessor {
  def findWords(text: String): List[String] = {
    val paragraphs = identifyParagraphs(text)
    val textsInParagraph = paragraphs map { _.split(' ').toList }
    textsInParagraph flatMap { _.filterNot(_ matches """^\W+$""") }
  }
  
  def countWords(text: String): Int = findWords(text).size
  
  def generateWordStats(text: String): Map[String, Int] = {
    val wordsGrouped = findWords(text) groupBy {
      s => {
        s.toLowerCase match {
          // if word begins with a non-word like punctuation mark, remove it
          case key if key matches """^\W[a-z0-9]+$""" => key drop 1
          
          //if word ends with a non-word, remove it
          case key if key matches """^[a-z0-9]+\W$""" => key dropRight 1
          
          // if word begins and ends in a non-word, like (Mary), remove the non-word character
          case key if key matches """^\W[a-z0-9]+\W$""" => key slice (1, key.size - 1)
          
          case _ => s.toLowerCase
        }
      }
    }
    wordsGrouped mapValues { _.size }
  }
  
  def identifyTopWords(text: String): List[String] = {
    val wordStats = generateWordStats(text)
    val frequenciesOrdered: List[Int] = wordStats.values.toList.sorted.reverse.distinct
    
    val wordsOrdered = frequenciesOrdered flatMap {
      i => {
        val words = wordStats filter { t: (String, Int) => t._2 == i }
        words.keys.toList.sorted
      }
    }
    
    wordsOrdered.size match {
      case i if i > 20 => wordsOrdered take 20
      case _ => wordsOrdered
    }
  }
}



trait CharacterProcessor extends ParagraphProcessor {
  def countCharacters(text: String): Int = {
    val paragraphs = identifyParagraphs(text)
    val paragraphSize = paragraphs map { _.size }
    paragraphSize.sum
  }
}



trait ParagraphProcessor {
  def identifyParagraphs(text: String): List[String] = {
    val lines = text.split('\n').toList
    lines filterNot { _.isEmpty }
  }
  
  def countParagraphs(text: String): Int = {
    val paragraphs = identifyParagraphs(text)
    paragraphs.size
  }
}
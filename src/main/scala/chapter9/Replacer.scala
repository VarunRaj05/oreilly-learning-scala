package chapter9


/** Write a command-line application that will search and replace text inside files. The
 *  input arguments are a search pattern, a regular expression, the replacement text, and one
 *  or more files to search.
 *  
 */

object Replacer {
  import java.io.{ File, FilenameFilter, PrintWriter }
  
  def main(args: Array[String]) = {
    args.toList match {
      case regex :: text :: files if files.nonEmpty => {
        files foreach {
          file => {
            val dir = """src\main\resources\"""
            
            takeBackUp(dir, file)
            
            writeToFile(dir, file, regex, text)
          }
        }
      }
      case _ => println("Incorrect arguments provided")
    }
  }
  
  // read content from file
  
  def readFromFile(path: String, file: String): String = {
    val source = io.Source.fromFile(path + file)
    val data = source.mkString
    source.close
    data
  }
  
  
  // generate the file name for the BAK file in which we will take back up
  
  def generateBakFilename(path: String, file: String): String = {
    val dir = new File(path)
    val filter = new FilenameFilter {
      override def accept(d: File, f: String): Boolean = {
        f matches """^.+\.bak\d+$"""
      }
    }
    
    // get all BAK file names for the original file
    
    val bakFiles = dir.listFiles(filter).toList map { _.toString } filter {
      f => {
        val fileFromPath = f.split('\\').last
        val fileFromBak = fileFromPath split '.' dropRight 1 mkString "."
        fileFromBak == file
      }
    }
    
    // if BAK files exist, then extract max version and increment by 1, or use 1
    
    val bakVersion = bakFiles match {
      case b if b.nonEmpty => {
        val bakVersions = bakFiles map { _.split('.').last.replace("bak", "").toInt }
        bakVersions.max + 1
      }
      case _ => 1
    }
    
    // generate file name for BAK with correct version number
    
    s"${ file }.bak${ bakVersion }"
  }
  
  
  // take back up by copying data from original file into new file
  
  def takeBackUp(path: String, file: String): Unit = {
    val data = readFromFile(path, file)
    val bakFilename = generateBakFilename(path, file)
    val writer = new PrintWriter(path + bakFilename)
    writer write data
    writer.close
  }
  
  // replace original text in string with new text
  
  def findAndReplace(oldText: String, pattern: String, newText: String): String = {
    oldText.replaceAll(pattern, newText)
  }
  
  // replace original text in file with new text
  
  def writeToFile(path: String, file: String, pattern: String, newText: String): Unit = {
    val data = readFromFile(path, file)
    val newData = findAndReplace(data, pattern, newText)
    val writer = new PrintWriter(path + file)
    writer write newData
    writer.close
  }
}
package chapter9

import org.scalatest._
import java.io.{ File, PrintWriter }

class ReplacerSpec extends FlatSpec with Matchers {
  val dir = """src\test\resources\"""
  val file = "test.txt"
  
  
  "readFromFile" should "pull in right content" in {
    val writer = new PrintWriter(dir + file)
    writer write "Mary had a little lamb\n"
    writer.close
    
    Replacer.readFromFile(dir, file) should equal ("Mary had a little lamb\n")
    
    val f = new File(dir + file)
    f.delete
  }
  
  
  
  "generateBakFilename" should "generate version 1 if no BAK file exists" in {
    val f = new File(dir + file)
    f.createNewFile
    
    Replacer.generateBakFilename(dir, file) should equal (file + ".bak1")
  }
  
  it should "generate version 2 if 1 BAK file exists" in {
    val f = new File(dir + file + ".bak1")
    f.createNewFile
    
    Replacer.generateBakFilename(dir, file) should equal (file + ".bak2")
  }
  
  it should "generate version 3 if 2 BAK files exist" in {
    val f = new File(dir + file + ".bak2")
    f.createNewFile
    
    Replacer.generateBakFilename(dir, file) should equal (file + ".bak3")
    
    // delete all BAK files and the original file
    
    val files = List(dir + file, dir + file + ".bak1", dir + file + ".bak2")
    files foreach (f => { { new File(f) }.delete })
  }
  
  
  
  "takeBackUp" should "copy content from original file to BAK file" in {
    val writer = new PrintWriter(dir + file)
    writer write "Mary had a little lamb, and one not so little lamb\n"
    writer.close
    
    Replacer.takeBackUp(dir, file)
    
    val originalSource = io.Source.fromFile(dir + file)
    val originalText = originalSource.mkString
    originalSource.close
    
    val newSource = io.Source.fromFile(dir + file + ".bak1")
    val newText = newSource.mkString
    newSource.close
    
    originalText should equal (newText)
    
    // delete all BAK files and the original file
    
    val files = List(dir + file, dir + file + ".bak1")
    files foreach (f => { { new File(f) }.delete })
  }
  
  
  
  "findAndReplace" should "replace per specified pattern" in {
    Replacer.findAndReplace("XYZ789", """\d+""", "123") should equal ("XYZ123")
    Replacer.findAndReplace("<user><name>Mary</name></user>", """<[^<]+>""", "") should equal ("Mary")
  }
  
  
  
  "writeToFile" should "replace data in file with new data per specified pattern" in {
    val writer = new PrintWriter(dir + file)
    writer write "Mary had a little lamb, and one not so little lamb\n"
    writer.close
    
    Replacer.writeToFile(dir, file, "lamb", "penguin")
    
    val f = io.Source.fromFile(dir + file)
    val content = f.mkString
    f.close
    
    content should equal ("Mary had a little penguin, and one not so little penguin\n")
    
    { new File(dir + file) }.delete
  }
}
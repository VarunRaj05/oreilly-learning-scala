package chapter7




/** The Fibonacci series starts with the numbers “1, 1” and then computes each successive
 *  element as the sum of the previous two elements. We’ll use this series to get
 *  familiarized with the collections in this chapter.
 *  
 *  a. Write a function that returns a list of the first x elements in the Fibonacci series
 *  Can you write this with a Buffer? Would a Builder be appropriate here?
 *  
 *  b. Write a new Fibonacci function that adds new Fibonacci numbers to an existing
 *  list of numbers. It should take a list of numbers (List[Int]) and the count of
 *  new elements to add and return a new list (List[Int]). Although the input list
 *  and returned lists are immutable, you should be able to use a mutable list inside
 *  your function. Can you also write this function using only immutable lists?
 *  Which version, using mutable versus immutable collections, is more appropriate
 *  and readable?
 *  
 *  c. The Stream collection is a great solution for creating a Fibonacci series. Create
 *  a stream that will generate a Fibonacci series. Use it to print out the first 100
 *  elements in the series, in a formatted report of 10 comma-delimited elements
 *  per line.
 *  
 *  d. Write a function that takes an element in the Fibonacci series and returns the
 *  following element in the series. For example, fibNext(8) should return 13. How
 *  will you handle invalid input such as fixNext(9)? What are your options for
 *  conveying the lack of a return value to callers?
 */

object MoreCollections1 extends App {
  
  // a
  
  def fibBuffer(n: Int): List[Int] = {
    val b = collection.mutable.Buffer(1, 1)
    while (b.length < n) {
      b += { b takeRight 2 }.sum
    }
    b.toList
  }
  
  def fibBuilder(n: Int): List[Int] = {
    val b = List.newBuilder[Int]
    b ++= List(1, 1)
    while (b.result.length < n) {
      b += { b.result takeRight 2}.sum
    }
    b.result
  }
  
  // b
  
  def addFibMutable(l: List[Int], n: Int): List[Int] = {
    val b = l.toBuffer
    while (b.length < l.length + n) {
      b += { b takeRight 2 }.sum
    }
    b.toList
  }
  
  def addFibImmutable(l: List[Int], n: Int): List[Int] = {
    def addToList(x: List[Int]): List[Int] = {
      x.head + x.tail.head :: x
    }
    
    val r = l.reverse
    
    while (r.length < l.length + n) {
      addToList(r)
    }
    
    r.reverse
  }
  
  // c
  
  def fibStream: Unit = {
    def makeFib(i: Long, j: Long): Stream[Long] = Stream.cons(i, makeFib(j, i + j))
    val l = { makeFib(1, 1) take 100 }.toList
    
    for (i <- 1 to l.length) {
      if (i % 10 == 0) println(l(i - 1))
      else print(l(i - 1) + ", ")
    }
  }
  
  
  // d
  
  def nextFib(n: Int): Option[Int] = {
    def makeFib(i: Int, j: Int): Stream[Int] = i #:: makeFib(j, i + j)
    
    // takeWhile generates values in the stream until the condition fails,
    // so it will always contain one more element than the last value that meets the condition...
    // meaning that the last element in the stream will be the one that failed the condition...
    // the list won't pull in that last element though because it only pulls in values that meet the condition
    
    val s = makeFib(1,1)
    val f: List[Int] = { s takeWhile (_ <= n) }.toList
    val r: Option[Int] = {
      if (f.last == n)
        Some({ s take (f.length + 1) }.toList.last)
      else
        None
    }
    
    r
  }
  
  // run code and display results
  
  println("a:")
  println(fibBuffer(11))
  println(fibBuilder(7))
  println
  println("b:")
  println(addFibMutable(List(1,1,2,3,5), 4))
  println(addFibMutable(List(1,1,2,3,5), 6))
  println
  println("c:")
  fibStream
  println
  println("d:")
  println(nextFib(8))
  println(nextFib(9))
}



/**  In the example for Array collections (see “Arrays” on page 112) we used the
 *   java.io.File(<path>).listFiles operation to return an array of files in the current
 *   directory. Write a function that does the same thing for a directory, and converts
 *   each entry into its String representation using the toString method. Filter out any
 *   dot-files (files that begin with the . character) and print the rest of the files separated
 *   by a semicolon (;). Test this out in a directory on your computer that has a significant
 *   number of files.
 */

object MoreCollections2 extends App {
  val path = """C:\Program Files (x86)\Git\bin"""
  val files = getFiles(path)
  
  println(files)
  
  def getFiles(p: String): String = {
    val files = new java.io.File(p).listFiles
    
    // extract actual file names from the absolute paths
    val filenames = files.toList map (f => { f.toString split '\\' }.last)
    
    val notHidden = filenames filterNot { _ startsWith "."}
    val myFiles = notHidden mkString ";"
    
    //return
    myFiles
  }
}



/** Take the file listing from exercise 2 and print a report showing each letter in the
 *  alphabet followed by the number of files that start with that letter.
 */

object MoreCollections3 extends App {
  def getFiles(p: String): String = {
    val files = new java.io.File(p).listFiles
    
    // extract actual file names from the absolute paths
    val filenames = files.toList map (f => { f.toString split '\\' }.last)
    
    val notHidden = filenames filterNot { _ startsWith "."}
    val myFiles = notHidden mkString ";"
    
    //return
    myFiles
  }
  
  val alphabet: List[Char] = { 'a' to 'z' }.toList
  
  val path = """C:\Program Files (x86)\Git\bin"""
  val files = { getFiles(path) split ';' }.toList
  
  // generate a list of tuple pairs (letter, count)
  val counts: List[(Char, Int)] = { files groupBy (_ charAt 0) }.toList map {
    (x: (Char, List[String])) => (x._1, x._2.size)
  }
  
  // convert list of alphabets to list of tuple pairs (letter, 0)
  val alphCount: List[(Char, Int)] = alphabet map { l => (l, 0) }
  
  // merge items from counts into the alphabet tuple and print
  alphCount foreach {
    x => {
      val found: Option[(Char, Int)] = counts find (_._1 == x._1)
      val count = {
        if (found.isDefined)
          found.get._2
        else
          x._2
      }
      println(s"${x._1} - $count")
    }
  }
}



/** Write a function to return the product of two numbers that are each specified as a
 *  String, not a numeric type. Will you support both integers and floating-point
 *  numbers? How will you convey if either or both of the inputs are invalid? Can you
 *  handle the converted numbers using a match expression? How about with
 *  a for-loop?
 */

object MoreCollections4 extends App {
  def convert(x: String): Option[Double] = util.Try(x.toDouble).toOption
  
  def productMatch(x: String, y: String): Option[Double] = {
    val nums: (Option[Double], Option[Double]) = (convert(x), convert(y))
    val prod = nums match {
      case (Some(n1), Some(n2)) => Some(n1 * n2)
      case _ => None
    }
    prod
  }
  
  /*
    The for comprehension is syntactic sugar for a map on an option
    Map on an option always returns an option type...
    by applying the input function on the option's value if present...
    returns None otherwise
    For comprehension works similarly...
    yield expression works on the values of the Option if present...
    but returns the computed value as an option type
   */
  
  def productFor(x: String, y: String): Option[Double] = {
    val prod = for {
      n1 <- convert(x)
      n2 <- convert(y)
    } yield n1 * n2
    
    prod
  }
  
  println(productMatch("2", "3.34"))
  println(productMatch("two", "3.34"))
  println(productFor("2", "3.34"))
  println(productFor("two", "3.34"))
}



/** Write a function to safely wrap calls to the JVM library method
 *  System.getProperty(<String>), avoiding raised exceptions or null results.
 *  System.getProperty(<String>) returns a JVM environment property value given
 *  the property’s name. For example, System.getProperty("java.home") will return
 *  the path to the currently running Java instance, while System.
 *  getProperty("user.timezone") returns the time zone property from the operating
 *  system. This method can be dangerous to use, however, because it may throw
 *  exceptions or return null for invalid inputs. Try invoking
 *  System.getProperty("") or System.getProperty("blah") from the Scala REPL
 *  to see how it responds.
 *  
 *  Experienced Scala developers build their own libraries of functions that wrap unsafe
 *  code with Scala’s monadic collections. Your function should simply pass its input
 *  to the method and ensure that exceptions and null values are safely handled and
 *  filtered. Call your function with the example property names used here, including
 *  the valid and invalid ones, to verify that it never raises exceptions or returns
 *  null results.
 */

object MoreCollections5 extends App {
  def getEnv(name: String): Option[String] = {
    val get = util.Try{ System.getProperty(name) }
    val ret = get match {
      case util.Success(s) if s != null => Some(s)
      case _ => None
    }
    ret
  }
  
  val inputs: List[String] = List("java.home", "user.timezone", "", "blah")
  
  inputs foreach { s => println(getEnv(s)) }
}



/** Write a function that reports recent GitHub commits for a project. GitHub provides
 *  an RSS feed of recent commits for a given user, repository, and branch, containing
 *  XML that you can parse out with regular expressions. Your function should take
 *  the user, repository, and branch, read and parse the RSS feed, and then print out
 *  the commit information. This should include the date, title, and author of each
 *  commit.
 *  
 *  You can use the following RSS URL to retrieve recent commits for a given repository
 *  and branch:
 *  
 *  https://github.com/<user name>/<repo name>/commits/<branch name>.atom
 *  
 *  Here is one way to grab the RSS feed as a single string:
 *  
 *  scala> val u = "https://github.com/scala/scala/commits/2.11.x.atom"
 *  u: String = https://github.com/scala/scala/commits/2.11.x.atom
 *  
 *  scala> val s = io.Source.fromURL(u)
 *  s: scala.io.BufferedSource = non-empty iterator
 *  
 *  scala> val text = s.getLines.map(_.trim).mkString("")
 *  text: String = <?xml version="1.0" encoding="UTF-8"?><feed xmlns=...
 *  
 *  Working with the XML will be a bit tricky. You may want to use text.split(<token>)
 *  to split the text into the separate <entry> components, and then use regular
 *  expression capture groups (see “Regular expressions” on page 19) to parse out the
 *  <title> and other elements. You could also just try iterating through all the lines
 *  of the XML file, adding elements to a buffer as you find them, and then converting
 *  that to a new list.
 *  
 *  Once you have completed this exercise (and there is a lot to do here), here are some
 *  additional features worth investigating:
 *  
 *  - Move the user, repo, and branch parameters into a tuple parameter.
 *  
 *  - Have the function take a list of GitHub projects and print
 *  a report of each one’s commits, in order of specified project.
 *  
 *  - Mix the commits together and sort by commit date, then
 *  print your report with an additional “repo” column.
 *  
 */


object MoreCollections6 extends App {
  
  // input data
  
  val projects: List[(String, String, String)] = List(
      ("scala", "scala", "2.11.x"),
      ("typesafehub", "sbteclipse", "master"),
      ("json4s", "json4s", "3.4"))
  
  
  // get XML content
  
  val xml: List[String] = projects map {
    (p: (String, String, String)) => {
      val url = s"https://github.kdc.capitalone.com/${ p._1 }/${ p._2 }/commits/${ p._3 }.atom"
      val xml = io.Source.fromURL(url).getLines.toList map (_.trim)
      xml.mkString
    }
  }
  
  
  // extract individual entry tags from XML
  
  val entries: List[List[String]] =  xml map {
    (xml: String) => { xml split "<entry>" }.toList drop 1
  }
  
  
  // parse out title, date, and author from each entry tag
  
  val commits: List[List[Option[(String, String, String)]]] = entries map {
    _ map {
      (entry: String) => {
        def fetch(e: String, t: String): Option[String] = {
          val regex = s"^.*<$t>(.*)</$t>.*$$".r
          e match {
            case regex(matched) => Some(matched)
            case _ => None
          }
        }
        
        for {
          title <- fetch(entry, "title")
          date <- fetch(entry, "updated") map { _ substring (0, 10) }
          author <- fetch(entry, "name")
        } yield (title, date, author)
      }
    }
  }
  
  
  // merge project information with commits information
  
  val merged: List[((String, String, String), List[Option[(String, String, String)]])] =
    projects zip commits
  
  
  // create individual commits as string with project information
  
  val commitsAll: List[String] = merged flatMap {
    t => {
      val project: String = s"Project: ${ t._1._1 }/${ t._1._2 }/${ t._1._3 }\n"
      val commits: List[String] = t._2 map {
        _ match {
          case Some(c) => s"${ project }Title: ${ c._1 }\nDate: ${ c._2 }\nAuthor: ${ c._3 }"
          case _ => ""
        }
      }
      
      commits
    }
  }
  
  
  // sort commits by most recent commit date
  
  val commitsSorted: List[String] = {
    commitsAll sortBy {
      c => {
        val regex = """^.+\n.+\n.+(\d{4}-\d{2}-\d{2})\n.+$""".r
        val regex(date) = c
        date
      }
    }
  }.reverse
  
  
  
  // print commits by project or by commit date based on coin flip
  
  val out = {
    val flip = util.Random.nextInt(2)
    val header = if (flip == 1) "BY PROJECT\n\n" else "BY COMMIT DATE\n\n"
    val data = if (flip == 1) commitsAll else commitsSorted
    
    header + { data mkString "\n\n" }
  }
  
  println(out)
}
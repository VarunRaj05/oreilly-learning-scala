package chapter8


/** We’re working on a gaming site, and need to track popular consoles like the Xbox
 *  Two and Playstation 5 (I'm planning for the future here).
 *  
 *  a. Create a console class that can track the make, model, debut date, WiFi
 *  type, physical media formats supported, and maximum video resolution. Over‐ride the
 *  default toString method to print a reasonably sized description of the
 *  instance (< 120 chars).
 *  
 *  The debut date (or launch date) should be an instance of java.util.Date.
 *  
 *  Keep the WiFi type (b/g, b/g/n, etc.) field optional, in case some consoles
 *  don’t have WiFi.
 *  
 *  The physical media formats should be a list. Is a String the best bet here, or
 *  an Int that matches a constant value?
 *  
 *  The maximum video resolution should be in a format that would make it
 *  possible to sort consoles in order of greatest number of pixels.
 *  
 *  
 *  b. Test your new console class by writing a new class that creates four instances of
 *  this console class. All of the instances should have reasonably accurate values.
 *  
 *  
 *  c. Now it’s time for games. Create a game class that includes the name, maker, and
 *  a list of consoles it supports, plus an “isSupported” method that returns true if
 *  a given console is supported.
 *  
 *  
 *  d. Test out this game class by generating a list of games, each containing one or
 *  more instances of consoles. Can you convert this list to a lookup table for consoles
 *  with a list of supported games? How about a function that prints a list of
 *  the games, sorted first by maker and then by game name?
 *  
 */


object Classes1 extends App {
  class Console(
      val make: String,
      val model: String,
      date: String,
      val wifiType: Option[String] = None,
      mediaFormat: List[Int],
      val maxResolution: Int) {
    val launchDate: java.util.Date = new java.text.SimpleDateFormat("MM/dd/yyyy") parse date
    val physicalMediaSupported: List[String] = mediaFormat map {
      _ match {
        case 1 => "USB"
        case 2 => "HDMI"
        case 3 => "DVR"
      }
    }
    
    override def toString = s"$make - $model at $maxResolution pixels launched ${ launchDate.toString }"
  }
  
  
  class Instantiate {
    val consoles: List[Console] = List(
        new Console(
            "playstation",
            "2.008",
            "09/12/2011",
            Some("b/g/n"),
            List(2,3),
            1080),
        new Console(
            make = "Xbox",
            model = "9.8",
            date = "12/25/2012",
            mediaFormat = List(1,3),
            maxResolution = 1080),
        new Console(
            make = "playstation",
            model = "1.09",
            date = "11/28/2007",
            mediaFormat = List(2),
            maxResolution = 780),
        new Console(
            make = "playstation",
            model = "0.04",
            date = "04/13/2005",
            mediaFormat = List(1),
            maxResolution = 780))
  }
  
  class Game(
      val name: String,
      val maker: String,
      val consoles: List[Console]) {
    override def toString = s"$name - $maker"
    
    def isSupported(console: Console): Boolean = consoles contains console
  }
  
  // test out classes
  
  val consoles = new Instantiate().consoles
  
  println("\nPrinting all consoles...")
  consoles foreach { obj => println(obj.toString) }
  
  val games: List[Game] = List(
      new Game(
          name = "Fifa",
          maker = "EA",
          consoles = List(consoles(0), consoles(1))),
      new Game(
          name = "NFL Madden",
          maker = "Gamex49",
          consoles = List(consoles(0), consoles(2), consoles(3))),
      new Game(
          name = "racing",
          maker = "EA",
          consoles = List(consoles(1), consoles(3))),
      new Game(
          name = "cricket",
          maker = "EA",
          consoles = List(consoles(2))))
  
  println("\nPrinting all games...")
  games foreach { obj => println(obj.toString) }
  
  val gameExists = games(1).isSupported(consoles(1))
  println("\nDoes the game exist?\n" + gameExists)
  
  // create a look up table... generate a map of the Map[Console, List[Game]] type
  
  val consolesInGames: List[Console] = { games flatMap (_.consoles) }.distinct
  val lookUp: List[(Console, List[Game])] = consolesInGames map {
    c => {
      val gamesForThisConsole = games.foldLeft[List[Game]](Nil) {
        (result, game) => {
          if (game.consoles contains c) {
            game :: result
          }
          else {
            result
          }
        }
      }
      
      (c, gamesForThisConsole.reverse)
    }
  }
  val table: Map[Console, List[Game]] = lookUp.toMap
  
  // should only print games applicable to console(0)
  println("\n" + table(consoles(0)))
  
  // sort by make and then by name... use string by merging maker and name with an underscore
  def sortGames(gm: List[Game]): Unit = {
    val sorted = gm sortBy { g => s"${ g.maker.toLowerCase }_${ g.name.toLowerCase }" }
    sorted foreach { g => println(g.toString) }
  }
  
  println("\nSorting...")
  sortGames(games)
}


/** Linked list, object oriented style
 *  
 *  Create a container class that has an instance of itself plus an instance of a parameterized
 *  type. The constructor should take a variable number of the instances
 *  (e.g., strings or ints or any other parameterized type), which can be implemented
 *  with VarArg parameters. Implement a "foreach" method that users can call to iterate over the
 *  list, invoking their function for every element.
 *  
 *  Make your container class abstract with two subclasses:
 *  one representing a node with a valid item and one representing a node without
 *  a valid item, signifying the last item in the list.
 *  
 *  Add the standard head, tail, filter, size, and map collection methods for your
 *  linked list. Can you implement any of these using lazy values? Which of these
 *  should be implemented in the parent class versus being implemented in its subclasses?
 */


object Classes2 extends App {
  abstract class Node[X] {
    def apply(i: Int): Option[X]
    def head: Option[X]
    def tail: Node[X]
    def foreach(f: X => Unit): Unit
    
    def size: Int = {
      var i = 0
      foreach {
        item => {
          i = i + 1
        }
      }
      i
    }
    
    def reverse: Node[X] = {
     var node: Node[X] = new EmptyNode[X]
     foreach {
       item => {
         node = new NonEmptyNode[X](item, node)
       }
     }
     node
    }
    
    def map[Y](f: X => Y): Node[Y] = {
      var node: Node[Y] = new EmptyNode[Y]
      foreach {
        item => {
          node = new NonEmptyNode[Y](f(item), node)
        }
      }
      node.reverse
    }
    
    def filter(f: X => Boolean): Node[X] = {
      var node: Node[X] = new EmptyNode[X]
      foreach {
        item => {
          if (f(item)) {
            node = new NonEmptyNode[X](item, node)
          }
        }
      }
      node.reverse
    }
  }
  
  class EmptyNode[X] extends Node[X] {
    def apply(i: Int): Option[X] = None
    def head: Option[X] = None
    def tail: Node[X] = null
    def foreach(f: X => Unit): Unit = {}
  }
  
  class NonEmptyNode[X](val data: X, val node: Node[X]) extends Node[X] {
    def head: Option[X] = apply(0)
    def tail: Node[X] = node
    
    def apply(i: Int): Option[X] = {
      if (i == 0) {
        Some(data)
      }
      else {
        node.apply(i - 1)
      }
    }
    
    def foreach(f: X => Unit): Unit = {
      f(data)
      node.foreach(f)
    }
  }
  
  
  /* A node consists of a value field to hold data, and another value field to hold a new node
   * Nodes are linked to each other because they're contained in another node
   * The final node's node field won't contain anything because it needs to be empty
   * Two types of nodes: Non-empty node, and Empty node
   * An abstract class called Node will have the functions that are common to both types of nodes
   * 
   * Linked list is created by first creating an empty node, then reading the input data items...
   * for each item, a new non-empty node is created and the item is placed into its data value field...
   * and its node field is then populated with the previous node.
   */
  
  
  object LinkedList {
    def apply[X](items: X*): Node[X] = {
      var node: Node[X] = new EmptyNode[X]
      for (i <- items) {
        node = new NonEmptyNode[X](i, node)
      }
      node.reverse
    }
  }
  
  
  // test it out
  
  val list = LinkedList(1,3,5,7,9)
  print("The original list: ( ")
  list foreach { i => print(s"$i ") }
  println(")")
  
  println(s"The size of this list is: ${ list.size }")
  println(s"The 2nd index of this list contains: ${ list(2) getOrElse (-1) }")
  println(s"The head of this list is: ${ list.head getOrElse (-1) }")
  println(s"The 1st index of the tail of this list is: ${ list.tail.apply(1) getOrElse (-1) }")
  
  val revList = list.reverse
  print("The reversed list: ( ")
  revList foreach { i => print(s"$i ") }
  println(")")
  
  val filterList = list filter { i => i % 3 == 0 }
  print("The filtered list: ( ")
  filterList foreach { i => print(s"$i ") }
  println(")")
  
  val newList = list map { i => i + 10 }
  print("Mapped list: ( ") 
  newList foreach { i => print(s"$i ") }
  println(")")
}




/** For a change of pace, let’s create a directory listing class. The constructor fields
 *  should be the full path to the directory and a predicate function that takes a String
 *  (the filename) and returns true if the file should be included. The method "list"
 *  should then list the files in the directory.
 *  To implement this, create an instance of java.io.File and use its listFiles(filter: FilenameFilter)
 *  to list files that match the given filter. You'll find Javadocs
 *  for this method and for the java.io.FilenameFilter class, but you will need to
 *  figure out how this would be called from Scala. You should pass in the FilenameFilter argument
 *  as an anonymous class.
 *  
 *  Is there any part of this class that would work well as a lazy value?
 *  
 *  Would it make sense to store the anonymous subclass of java.io.FilenameFilter as a lazy val?
 *  
 *  How about the filtered directory listing?
 */


object Classes3 extends App {
  import java.io.{ File, FilenameFilter }
  
  class Listing(val p: String, val f: String => Boolean) {
    lazy val file = new File(p)
    
    lazy val filter = new FilenameFilter {
      def accept(dir: File, name: String): Boolean = f(name)
    }
    
    // file and filter are only needed when list function is called, hence lazy
    
    def list: List[String] = file.listFiles(filter).toList map (_.toString)
  }
  
  // test class... get files that begin with the letter S
  
  val path = """src\main\resources"""
  
  val test = (file: String) => file(0).toLower == 's'
  
  val files = new Listing(path, test)
  
  files.list foreach { println(_) }
}



/** The JVM library includes a working MIDI sound synthesizer. Here’s an example of
 *  playing a short set of notes:
 *  
 *  scala> val synth = javax.sound.midi.MidiSystem.getSynthesizer
 *  synth: javax.sound.midi.Synthesizer = com.sun.media.sound.SoftSynthesizer@283a8ad6
 *  scala> synth.open()
 *  scala> val channel = synth.getChannels.head
 *  channel: javax.sound.midi.MidiChannel = com.sun.media.sound.SoftChannelProxy@606d6d2c
 *  scala> channel.noteOn(50, 80); Thread.sleep(250); channel.noteOff(30)
 *  scala> synth.close()
 *  
 *  Create a simpler interface to this by writing a class that plays a series of notes. The
 *  class’s constructor should take the volume (set to 80 in the example) but always use
 *  the same duration (250 milliseconds in the example). Its “play” method should take
 *  a list of the notes, for example Seq(30, 35, 40, 45, 50, 55, 60, 65, 70), and
 *  play them in the synthesizer.
 *  
 *  Assume the getSynthesizer method call is expensive. How can you prevent
 *  unnecessarily calling it in case the “play” method is never called?
 *  
 *  Make sure to hide fields that callers don’t need to know about.
 *  
 *  Can you support a Range as input, e.g., play(30 to 70 by 5) ?
 *  
 *  Can you support multiple ranges, for example a series of notes that rise, fall, and
 *  then rise again?
 *  
 *  Assume we only ever need one instance, ever, with the volume set to 95. Can you
 *  use access controls to ensure that there will never be more than one instance of
 *  this class?
 */


object Classes4 extends App {
  class Synthesizer(val vol: Int) {
    private val duration: Int = 250
    private lazy val synth = javax.sound.midi.MidiSystem.getSynthesizer
    private lazy val channel = synth.getChannels.head
    
    // overloading the play method to accept either Seq[Int] or a list of Seq[Int]
    
    def play(notes: Seq[Int]): Unit = generateSound(notes)
    
    def play(notes: List[Seq[Int]]): Unit = notes foreach { generateSound(_) }
    
    private def generateSound(notes: Seq[Int]) = {
      synth.open()
      notes foreach {
        n => {
          channel.noteOn(n, vol)
          Thread.sleep(duration)
        }
      }
      synth.close()
    }
  }
  
  
  // test it out
  
  val sound = new Synthesizer(vol = 95)
  
  println("playing harmonic minor scale now...")
  sound.play(Seq(50, 52, 53, 55, 57, 58, 61, 62))
  
  println("playing random range of notes...")
  sound.play(30 to 70 by 5)
  
  println("playing multiple range of notes: rise, fall, rise...")
  sound.play(List(30 to 70 by 5, 70 to 30 by -5, 30 to 70 by 5))
}
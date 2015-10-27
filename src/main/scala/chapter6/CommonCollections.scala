package chapter6

/** Create a list of the first 20 odd Long numbers. Can you create this with a for-loop,
 *  with the filter operation, and with the map operation? What's the most efficient
 *  and expressive way to write this?
 */

object CommonCollections1 extends App {
  // using for loop
  val withForLoop: List[Long] = {
    var list: List[Long] = Nil
    for (i <- 1L to 20L) {
      if (i % 2 != 0) {
        list = i :: list
      }
    }
    list.reverse
  }
  println(withForLoop)
  
  // using filter
  val withFilter: List[Long] = { 1L to 20L }.toList filter (_ % 2 != 0)
  println(withFilter)
  
  // using map
  val withMap: List[Long] = { 1L to 10L }.toList map (_ * 2 - 1)
  println(withMap)
}



/** Write a function titled "factors" that takes a number and returns a list of its factors,
 *  other than 1 and the number itself. For example, factors(15) should return
 *  List(3, 5).
 *  Then write a new function that applies "factors" to a list of numbers. Try using the
 *  list of Long numbers you generated in exercise 1. For example, executing this function
 *  with List(9, 11, 13, 15) should return List(3, 3, 5), because the factor
 *  of 9 is 3 while the factors of 15 are 3 again and 5. Is this a good place to use map and
 *  flatten? Or would a for-loop be a better fit?
 */

object CommonCollections2 extends App {
  val factors = (x: Int) => {
    var list: List[Int] = Nil
    for (i <- 2 until x) {
      if (x % i == 0) {
        list = i :: list
      }
    }
    list.reverse
  }
  
  println(factors(15))
  
  def findFactors(l: List[Int]): List[Int] = { l map factors }.flatten
  
  println(findFactors(List(9, 11, 13, 15)))
  
  // list of first 20 odd numbers
  val oddNums: List[Int] = { 1 to 20 }.toList filter (_ % 2 != 0)
  println(findFactors(oddNums))
}



/** Write a function, first[A](items: List[A], count: Int): List[A], that returns
 *  the first x number of items in a given list. For example,
 *  first(List('a','t','o'), 2) should return List('a','t'). You could make
 *  this a one-liner by invoking one of the built-in list operations that already performs
 *  this task, or (preferably) implement your own solution. Can you do so with a forloop?
 *  With foldLeft? With a recursive function that only accesses head and tail?
 */


object CommonCollections3 extends App {
  val list: List[Char] = List('a', 't', 'o')
  
  // using loop
  def first1[A](items: List[A], count: Int): List[A] = {
    var list: List[A] = Nil
    for (i <- 0 until count) {
      if (i <= items.size) {
        list = items(i) :: list
      }
    }
    list.reverse
  }
  
  println(first1(list, 2))
  
  // using foldLeft
  def first2[A](items: List[A], count: Int): List[A] = {
    items.foldLeft[List[A]](Nil) {
      (l: List[A], i: A) => {
        if (l.size < count)
          i :: l
        else
          l.reverse
      }
    }
  }
  
  println(first2(list, 2))
  
  // using the recursive function
  def first3[A](items: List[A], count: Int): List[A] = {
    if (count > 0 && items.tail != Nil)
      items.head :: first3(items.tail, count - 1)
    else
      Nil
  }
  
  println(first3(list, 2))
}



/** Write a function that takes a list of strings and returns the longest string in the list.
 *  Can you avoid using mutable variables here? This is an excellent candidate for the
 *  list-folding operations (Table 6-5) we studied. Can you implement this with both
 *  fold and reduce? Would your function be more useful if it took a function parameter
 *  that compared two strings and returned the preferred one? How about if
 *  this function was applicable to generic lists, i.e., lists of any type?
 */


object CommonCollections4 extends App {
  val string: List[String] = List("Mary", "had", "a", "little", "lamb")
  val int: List[Int] = List(44, 23, 62, 43, 22, 65, 47)
  
  val longestString = (x: String, y: String) => if (x.size >= y.size) x else y
  
  // using sortBy
  println({ string sortBy (_.size) }.reverse.head)
  
  // using fold
  println(string.fold[String]("")(longestString))
  
  // using reduce
  println(string reduce longestString)
  
  // generic function
  def findLongest[A](l: List[A], f: (A, A) => A): A = l reduce { (x: A, y: A) => f(x, y) }
  
  // use generic function for String
  println(findLongest[String](string, (x: String, y: String) => if (x.size >= y.size) x else y))
  
  // use generic function for Int
  println(findLongest[Int](int, (x: Int, y: Int) => if (x >= y) x else y))
}




/** Write a function that reverses a list. Can you write this as a recursive function?
 */

object CommonCollections5 extends App {
  val string: List[String] = List("Mary", "had", "a", "little", "lamb")
  val int: List[Int] = List(44, 23, 62, 43, 22, 65, 47)
  
  def reversify[A](list: List[A]): List[A] = {
    if (list != Nil)
      list(list.size - 1) :: reversify(list take { list.size - 1 })
    else Nil
  }
  
  println(reversify(string))
  println(reversify(int))
}




/** Write a function that takes a List[String] and returns a
 *  (List[String],List[String]), a tuple of string lists. The first list should be items
 *  in the original list that are palindromes (written the same forward and backward,
 *  like “racecar”). The second list in the tuple should be all of the remaining items
 *  from the original list. You can implement this easily with partition, but are there
 *  other operations you could use instead?
 */


object CommonCollections6 extends App {
  val string: List[String] = List("kayak", "mary", "radar", "had", "sagas", "civic", "a little", "lamb")
  
  // using partition
  val partitioned: (List[String], List[String])= string partition { s => s == s.reverse }
  
  println(partitioned)
  
  
  // using filter
  val filtered: (List[String], List[String]) = (string filter { s => s == s.reverse },
      string filter { s => s != s.reverse })
  
  println(filtered)
  
  
  // custom solution using foldLeft
  val isolated: (List[String], List[String]) = {
    val result = string.foldLeft[(List[String], List[String])]((Nil, Nil)) {
      (tuple: (List[String], List[String]), item: String) => {
        if (item == item.reverse)
          (item :: tuple._1, tuple._2)
        else
          (tuple._1, item :: tuple._2)
      }
    }
    
    (result._1.reverse, result._2.reverse)
  }
  
  println(isolated)
}

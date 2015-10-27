package chapter9


/** Write an application that reports on the most recently closed issues in a given
 *  GitHub project. The input arguments should include the repository name, project
 *  name, and an optional number of issues to report with a default value of 10. The
 *  output will have a report header and display each issue's number, title, user-name,
 *  number of comments, and label names. The output should be well-formatted, with
 *  fixed-width columns delimited with pipes (|) and a header delimited with equals
 *  signs (=).
 *  
 *  Re-factor the JSON handling code out to its own trait, e.g., "JsonSupport." Write
 *  tests to verify that it parses JSON code correctly, and handles exceptions that
 *  may be thrown by the Json4s library. Would it be useful to provide an object
 *  version of this trait?
 *  
 *  Do the same for the web handling code. Create your own "HtmlClient" trait and
 *  object that can take a URL and return the content as a list of strings. Can you
 *  include the server’s status response in a class along with the content? Make sure
 *  to write tests to verify the web handling code can prevent any exceptions from
 *  being thrown.
 *  
 *  Finally, re-factor your report generation code, the part that handles the clean
 *  fixed-width columns, into a reusable trait. Can it take a tuple of any size and
 *  print out its contents? Is there a more appropriate data type that it should take,
 *  one that supports variable numbers of columns but knows how to print out
 *  strings versus double values? Make sure your report generation code takes the
 *  maximum line width as an argument.
 */


object IssueReporter extends JsonHandler with HtmlClient with FixedWidthReporter {
  
  // define data types for parsing JSON content from GitHub Issues API
  
  case class User(login: String)
  case class Label(name: String)
  case class Issue(number: Int, title: String, user: User, comments: Int, labels: List[Label])
  
  
  // generate data set of Record type to feed into the createReport function
  
  def createRecordDataSet(issues: List[Issue]): List[Record] = {
    issues map {
      i => {
        val number = DataItem(data = i.number.toString,
            column = Column(name = "Number", width = 8))
            
        val title = DataItem(data = i.title,
            column = Column(name = "Title", width = 80))
            
        val user = DataItem(data = i.user.login,
            column = Column(name = "User", width = 20))
            
        val comments = DataItem(data = i.comments.toString,
            column = Column(name = "CommentCount", width = 8))
            
        val labels = DataItem(data = i.labels map { _.name } mkString ", ",
            column = Column(name = "Labels", width = 40))
        
        Record(record = List(number, title, user, comments, labels))
      }
    }
  }
  
  
  // application entry points
  
  def main(args: Array[String]) = {
    val url = args.toList match {
      case user :: repo :: count :: Nil if count matches """\d+""" => {
        s"https://api.github.com/repos/${ user }/${ repo }/issues?state=closed&per_page=${ count.toInt }"
      }
      case user :: repo :: Nil => {
        s"https://api.github.com/repos/${ user }/${ repo }/issues?state=closed&per_page=10"
      }
      case _ => ""
    }
    
    
    // process inputs
    
    val result: util.Try[String] = util.Try {
      url match {
        case u if u.nonEmpty => {
          val response: Response = pullContent(url)
          response  match {
            case resp if resp.status == "Success" => {
              val json: String = resp.content
              val issues: Option[List[Issue]] = parseJson[List[Issue]](json)
              issues match {
                case Some(i) if i.nonEmpty => {
                  val records: List[Record] = createRecordDataSet(i)
                  val report: Report = createReport(records, 175)
                  report match {
                    case rep if rep.status == 1 => rep.message
                    case rep if rep.status == 0 => throw new Exception(s"${ rep.message }")
                  }
                }
                case Some(i) if i.isEmpty => throw new Exception("No data found in matching JSON structure")
                case _ => throw new Exception("JSON structure returned does not match input structure")
              }
            }
            case _ => throw new Exception("Content could not be retrieved from the URL")
          }
        }
        case _ => throw new Exception("Incorrect arguments provided")
      }
    }
    
    
    // print output
    
    result match {
      case util.Success(s) => println(s)
      case util.Failure(f) => println(f)
    }
  }
}



// Handles HTML connectivity

trait HtmlClient {
  import util._
  import io.Source
  
  case class Response(status: String, content: String)
  
  def pullContent(url: String): Response = {
    val connection = Try { Source.fromURL(url)("UTF-8") }
    
    connection match {
      case Success(s) => {
        val content = s.mkString
        s.close
        Response(status = "Success", content = content)
      }
      case Failure(f) => Response(status = "Failure", content = "")
    }
  }
}



// Handles JSON content

trait JsonHandler {
  import util.Try
  import org.json4s.DefaultFormats
  import org.json4s.native.JsonMethods
  
  implicit val formats = DefaultFormats
  
  def parseJson[T](json: String)(implicit m: reflect.Manifest[T]): Option[T] = {
    val parsedData: Try[T] = Try { JsonMethods.parse(json).extract[T] }
    parsedData.toOption
  }
}



// Generates fixed width reports

trait FixedWidthReporter {
  case class Column(name: String, width: Int)
  case class DataItem(data: String, column: Column)
  case class Record(record: List[DataItem])
  case class Report(status: Int, message: String)
  
  // ensure all rows have same number of columns
  
  def validateColumnCount(records: List[Record]): Boolean = {
    records.map(_.record.size).distinct.size == 1
  }
  
  // ensure all rows have same column names and are in the same order
  
  def validateColumnNames(records: List[Record]): Boolean = {
    records.map(_.record map { _.column.name }).distinct.size == 1
  }
  
  // ensure all rows have same column widths and are in the same order
  
  def validateColumnWidths(records: List[Record]): Boolean = {
    records.map(_.record map { _.column.width }).distinct.size == 1
  }
  
  // extract column names and generate header with = sign as delimiter
  
  def generateHeader(records: List[Record]): String = {
    records.map(_.record map { _.column.name }).distinct.flatten mkString "="
  }
  
  // fits data item to specified width
  
  val fitToWidth: (DataItem => String) = d => {
    if (d.data.size > d.column.width) {
      d.data take d.column.width
    }
    else {
      val formatter = s"%-${ d.column.width }s"
      formatter.format(d.data)
    }
  }
  
  // generates fixed width report
  
  def createReport(records: List[Record], maxWidth: Int): Report = {
    val isColCountSame: Boolean = validateColumnCount(records)
    val areColNamesSame: Boolean = validateColumnNames(records)
    val areColWidthsSame: Boolean = validateColumnWidths(records)
    
    isColCountSame && areColNamesSame && areColWidthsSame match {
      case true => {
        val header = generateHeader(records)
        val rows: List[String] = records map { _.record map fitToWidth mkString "|" }
        val message: String = rows map { _ take maxWidth } mkString "\n"
        
        Report(status = 1, message = s"${ header }\n${ message }")
      }
      case false => Report(status = 0, message = "Column integrity not maintained across rows")
    }
  }
}
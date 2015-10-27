package chapter9

import org.scalatest._

class IssueReporterSpec extends FlatSpec with Matchers {
  import IssueReporter._
  
  
  // the data sets to use for testing
  
  case class Transaction(date: String, amount: Double, merchant: String)
  case class Account(acct: Long, statement: String, transactions: List[Transaction])
  
  val acctsJson = """
      [
        {
          "acct": 10025567847,
          "statement": "2015-10-01",
          "transactions": [
            {
              "date": "2015-09-03",
              "amount": 45.11,
              "merchant": "Starbucks"
            },
            {
              "date": "2015-09-07",
              "amount": 22.99,
              "merchant": "Target"
            },
            {
              "date": "2015-09-22",
              "amount": 109.88,
              "merchant": "Amazon"
            }
          ]
        },
        {
          "acct": 10016547899,
          "statement": "2015-10-01",
          "transactions": [
            {
              "date": "2015-09-11",
              "amount": 11.99,
              "merchant": "Netflix"
            },
            {
              "date": "2015-09-17",
              "amount": 65.75,
              "merchant": "Walmart"
            },
            {
              "date": "2015-09-27",
              "amount": 38.22,
              "merchant": "Hardywood Brewery"
            }
          ]
        }
      ]
    """
  
  
  val acct1 = Account(acct = 10025567847L, statement = "2015-10-01", transactions = List(
                  Transaction(date = "2015-09-03", amount = 45.11, merchant = "Starbucks"),
                  Transaction(date = "2015-09-07", amount = 22.99, merchant = "Target"),
                  Transaction(date = "2015-09-22", amount = 109.88, merchant = "Amazon")))
  
  
  val acct2 = Account(acct = 10016547899L, statement = "2015-10-01", transactions = List(
                  Transaction(date = "2015-09-11", amount = 11.99, merchant = "Netflix"),
                  Transaction(date = "2015-09-17", amount = 65.75, merchant = "Walmart"),
                  Transaction(date = "2015-09-27", amount = 38.22, merchant = "Hardywood Brewery")))
  
  
  val accts = List(acct1, acct2)
  
  val badJson = """
      {
        "message": "Not Found",
        "documentation_url": "https://developer.github.com/v3"
      }
    """
  
  val emptyJson = """
      [
      
      ]
    """
  
  val acct = DataItem(data = "10025567847", column = Column(name = "account", width = 11))
  val stmt = DataItem(data = "2015-10-01", column = Column(name = "statement", width = 20))
  val trxn20 = DataItem(data = "Starbucks, $45.11", column = Column(name = "transaction", width = 20))
  val trxn10 = DataItem(data = "Starbucks, $45.11", column = Column(name = "transaction", width = 10))
  
  
  
  val issueAsIssue = List(
      Issue(number = 1234, title = "just another title", user= User(login = "tester"),
      comments = 5, labels = List(Label(name = "review"), Label(name = "audit"))),
      Issue(number = 5678, title = "yet another title", user= User(login = "coder"),
      comments = 10, labels = List(Label(name = "code"), Label(name = "write"))))

  
  val issueAsReport = List(
      Record(record = List(
          DataItem(data = "1234", column = Column(name = "Number", width = 8)),
          DataItem(data = "just another title", column = Column(name = "Title", width = 80)),
          DataItem(data = "tester", column = Column(name = "User", width = 20)),
          DataItem(data = "5", column = Column(name = "CommentCount", width = 8)),
          DataItem(data = "review, audit", column = Column(name = "Labels", width = 40)))),
      Record(record = List(
          DataItem(data = "5678", column = Column(name = "Number", width = 8)),
          DataItem(data = "yet another title", column = Column(name = "Title", width = 80)),
          DataItem(data = "coder", column = Column(name = "User", width = 20)),
          DataItem(data = "10", column = Column(name = "CommentCount", width = 8)),
          DataItem(data = "code, write", column = Column(name = "Labels", width = 40)))))
  
  
  
  
  // the tests
  
  
  "JSON Parser" should "parse JSON if JSON data structure matches input parameter type" in {
    val data = parseJson[List[Account]](acctsJson)
    
    data shouldBe defined
    
    for (a <- 0 until accts.size) {
      data.get(a).acct should equal (accts(a).acct)
      data.get(a).statement should equal (accts(a).statement)
      
      for (t <- 0 until 3) {
        data.get(a).transactions(t).date should equal (accts(a).transactions(t).date)
        data.get(a).transactions(t).amount should equal (accts(a).transactions(t).amount)
        data.get(a).transactions(t).merchant should equal (accts(a).transactions(t).merchant)
      }
    }
  }
  
  it should "return None if JSON data structure does not match input parameter type" in {
    val data = parseJson[List[Account]](badJson)
    data should equal (None)
  }
  
  it should "return empty list if JSON data structure matches input parameter type but is empty" in {
    val data = parseJson[List[Account]](emptyJson)
    
    data shouldBe defined
    data.get.isEmpty should be (true)
  }
  
  
  
  
  "HTML client" should "return content and have status as success" in {
    val url = "https://api.github.com/repos/scala/scala/issues?state=closed&per_page=10"
    val response = pullContent(url)
    
    response.status should equal ("Success")
    response.content should not be empty
  }
  
  it should "return nothing in content and have status as failure" in {
    val url = "http://www.notarealurl987654321.com"
    val response = pullContent(url)
    
    response.status should equal ("Failure")
    response.content shouldBe empty
  }
  
  
  
  
  "Fixed Width Reporter" should "ensure column count is same across all rows" in {
    val r1 = List(Record(List(acct, stmt, trxn20)), Record(List(acct, stmt, trxn20)))
    validateColumnCount(r1) should be (true)
    
    val r2 = List(Record(List(acct, stmt, trxn20)), Record(List(acct, stmt)))
    validateColumnCount(r2) should be (false)
  }
  
  it should "ensure column names are same and in same order across all rows" in {
    val r1 = List(Record(List(acct, stmt, trxn20)), Record(List(acct, stmt, trxn20)))
    val r2 = List(Record(List(acct, stmt, trxn20)), Record(List(acct, trxn20, stmt)))
    val r3 = List(Record(List(acct, stmt, trxn20)), Record(List(acct, stmt, stmt)))
    
    validateColumnNames(r1) should be (true)
    validateColumnNames(r2) should be (false)
    validateColumnNames(r3) should be (false)
  }
  
  it should "ensure column widths are same and in same order across all rows" in {
    val r1 = List(Record(List(acct, stmt, trxn20)), Record(List(acct, stmt, trxn20)))
    val r2 = List(Record(List(acct, stmt, trxn20)), Record(List(acct, stmt, trxn10)))
    
    validateColumnWidths(r1) should be (true)
    validateColumnWidths(r2) should be (false)
  }
  
  it should "extract column names and generate header with = sign as delimiter" in {
    val r1 = List(Record(List(acct, stmt, trxn20)), Record(List(acct, stmt, trxn20)))
    generateHeader(r1) should equal ("account=statement=transaction")
  }
  
  it should "fit data item to specified width" in {
    fitToWidth(trxn20) should equal ("Starbucks, $45.11   ")
    fitToWidth(trxn10) should equal ("Starbucks,")
  }
  
  it should "generate fixed width report" in {
    val r1 = List(Record(List(acct, stmt, trxn20)), Record(List(acct, stmt, trxn20)))
    val r2 = List(Record(List(acct, stmt, trxn20)), Record(List(acct, stmt, trxn10)))
    val r3 = List(Record(List(acct, stmt, trxn20)), Record(List(acct, trxn20, stmt)))
    
    val header = "account=statement=transaction"
    val data1 = "10025567847|2015-10-01          |Starbucks, $45.11   "
    val data2 = "10025567847|2015-10-01          |Starbucks"
    
    val report1 = createReport(r1, 80)
    report1.status should equal (1)
    report1.message should equal(s"${ header }\n${ data1 }\n${ data1 }")
    
    val report2 = createReport(r1, 42)
    report2.status should equal (1)
    report2.message should equal(s"${ header }\n${ data2 }\n${ data2 }")
    
    val report3 = createReport(r2, 80)
    report3.status should equal (0)
    report3.message should equal ("Column integrity not maintained across rows")
    
    val report4 = createReport(r3, 80)
    report4.status should equal (0)
    report4.message should equal ("Column integrity not maintained across rows")
  }
  
  
  
  "Issue Reporter" should "create Record data set to feed into report creator" in {
    createRecordDataSet(issueAsIssue) should equal (issueAsReport)
  }
}
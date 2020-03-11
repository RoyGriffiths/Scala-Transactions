

import scala.io.Source
import scala.math.{abs, max}
import java.io._

case class Transaction(
                        transactionId: String,
                        accountId: String,
                        transactionDay: Int,
                        category: String,
                        transactionAmount: Double)

object ScalaTransactions extends App {

  // Here we detail where to find the transactions.txt file.
  val fileName = "C:\\Users\\Royah\\Desktop\\Programming\\Quantexa\\transactions.txt"
  val transactionslines = Source.fromFile(fileName).getLines().drop(1)

  val transactions: List[Transaction] = transactionslines.map { line =>
    val split = line.split(',')
    Transaction(split(0), split(1), split(2).toInt, split(3), split(4).toDouble)}.toList

  //
  // Question 1
  //

  var total = 0.00      // The total amount transferred in a day.
  var rounded = 0.00    // The total variable rounded to 2 decimal places.

  // Create a function that takes in a list of "Transaction" and an Int which is the date.
  // This then calculates how many times a transaction has occurred on the date specified as well as the total value.
  // Also works out the transaction category to be used in a later question.
  def totalValue(x: List[Transaction], y: Int) {
    for (z <- x) {
      if (z.transactionDay == y) {
        total     = total + z.transactionAmount
      }
    }
    rounded = BigDecimal(total).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
    total = 0
  }

  println()
  println("Question 1")
  println()

  // Prints all, if we have more dates we can simply change the while(day < x).
  // We'll write a new text file for the output as well as to the console.
  var day = 1
  val Q1 = new PrintWriter(new File("Question1.txt" ))
  while(day < 30){
    totalValue(transactions, day)
    Q1.write("Day " + day + ". " + " Total transaction value for today is:  " + rounded)
    Q1.write("\n")
    println("Day " + day + ". " + " Total transaction value for today is:  " + rounded)

    day = day + 1
  }
  Q1.close

  println()

  //
  // Question 2
  //

  // Create a list of all the types of transactions that can occur.
  // We can just write them out since there are only 7.
  var category  = List("AA","BB","CC","DD","EE","FF","GG")

  // Here we create a list of Ints which will hold the account Ids.
  // We hold them as Ints rather than Axy as it is easier to sort this way.
  var ids       = List[Int]()

  // stringId holds the number part of the ID as a String while numId holds it as a Int.
  var stringId      = ""
  var numId         = 0

  // Here we create a simple for loop that appends every new accountId found to our list.
  // This way if we are to get new accounts, we can just put it through our function.
  for(x <- transactions){
    stringId = x.accountId.slice(1,3)
    numId = stringId.toInt
    if(ids.contains(numId) == false){
      ids =  numId :: ids
    }
  }

  // We sort the list afterwards.
  ids = ids.sorted

  // Here we put them back into strings.
  var back = ids.map(_.toString)

  // We'll create a new list of strings which will have the 'A' part at the start too.
  // We'll do this in the for loop just below.
  // This helps when we want to use a comparison with IDs.
  var newIds   = List[String]()
  var idWithA  = " "
  for(x <- back){
    idWithA = 'A' + x
    newIds = idWithA :: newIds
  }

  // Create variable to hold the average.
  var average = 0.00

  // Counts how many transactions have occurred in a day.
  var count = 0

  // Function takes the list of transaction, accountIds as well as the category.
  // Loops through adding them up for each individual category for each account.
  // Then calculates the average by dividing the total by the count.
  def accountAverage(lst: List[Transaction], id: String, cat: String): Unit ={
    for(a <- lst){
      if (a.accountId == id){
        if (a.category == cat){
          count = count + 1
          total = total + a.transactionAmount
        }
      }
    }
    if(count == 0){
      average = 0
    } else {
      average = total / count
    }

    // Make sure to round ot 2 decimal places to be clean.
    rounded = BigDecimal(average).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
    count = 0
    total = 0
  }

  println("Question 2 ")
  println()

  val Q2 = new PrintWriter(new File("Question2.txt" ))

  // Now we just need to print it all out.
  // To get it in ascending order we just use the .reverse.
  // We will write it into a text file as well as the console again.
  for(x <- newIds.reverse){
    print("Account: " + x + " ")
    Q2.write("Account: " + x + " ")

    for(y <- category){
      accountAverage(transactions, x, y)
      print(y + ": " + rounded + " ")
      Q2.write(y + ": " + rounded + " ")
    }
    println()
    Q2.write("\n")
  }
  Q2.close()

  //
  // Question 3
  //

  // Here we create a lot of variables to use in our following functions.
  // The variables beginning with five refer to the relating past 5 days.
  // ie. fiveTotal refers to the total amount of the past 5 days.
  var fiveTotal     = 0.00
  var fiveAverage   = 0.00
  var fiveAA        = 0.00
  var fiveCC        = 0.00
  var fiveFF        = 0.00
  var fiveMax       = 0.00

  // Here are some of the day variables.
  var dayTotal      = 0.00
  var aaTotal       = 0.00
  var ccTotal       = 0.00
  var ffTotal       = 0.00
  var dayMax        = 0.00

  // These refer to the five values being rounded to 2 decimal places.
  var fARounded     = 0.00
  var fTRounded     = 0.00
  var fFFRounded    = 0.00
  var fAARounded    = 0.00
  var fCCRounded    = 0.00
  var fMRounded     = 0.00

  // Helps us count how many times an account had a transaction in the past 5 days.
  var totalCount = 0

  // This function will be used within another function later. Helps clean things up a little.
  // Calculates the totals for the day of an account as well as the total amounts of the 3 categories.
  // Also calculates the maximum transaction amount from a specific account on a certain day.
  // Also counts how many transactions have occurred for the specific account and day.
  def dayTotals(a: List[Transaction], day: Int, id: String): Unit ={
    for(x <- a){
      if(x.transactionDay == day && x.accountId == id && x.category == "AA"){
        aaTotal = aaTotal + x.transactionAmount
      }
      if(x.transactionDay == day && x.accountId == id && x.category == "CC"){
        ccTotal = ccTotal + x.transactionAmount
      }
      if(x.transactionDay == day && x.accountId == id && x.category == "FF"){
        ffTotal = ffTotal + x.transactionAmount
      }
      if(x.transactionDay == day && x.accountId == id){
        dayTotal = dayTotal + x.transactionAmount
      }
      if(x.transactionDay == day && x.accountId == id){
        dayMax = max(dayMax, x.transactionAmount)
      }
      if(x.transactionDay == day && x.accountId == id && x.transactionAmount != 0 ){
        count = count + 1
      }
    }
  }

  // Here's our file.
  val Q3 = new PrintWriter(new File("Question3.txt" ))

  // This function does all the tracking for the past 5 days of transactions.
  // We start off by looking at the past five days from the current date.
  // Looping through our previous dayTotals function for 5 days to get the rolling effect.
  def  fiveDays(a: List[Transaction], day: Int, id: String): Unit ={
    for(five <- (day-5) to (day-1)){
      dayTotals(a, five, id)
      totalCount = count + totalCount
      fiveTotal = dayTotal + fiveTotal
      fiveAA    = aaTotal + fiveAA
      fiveCC    = ccTotal + fiveCC
      fiveFF    = ffTotal + fiveFF
      fiveMax   = max(dayMax, fiveMax)
    }

    // This makes sure that we don't try to divide zero by zero.
    if(count == 0){
      fiveAverage = 0
    } else (fiveAverage = fiveTotal / totalCount)

    // Let's round all of our values to 2 decimal places here.
    fARounded   = BigDecimal(fiveAverage).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
    fTRounded   = BigDecimal(fiveTotal).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
    fAARounded  = BigDecimal(fiveAA).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
    fCCRounded  = BigDecimal(fiveCC).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
    fFFRounded  = BigDecimal(fiveFF).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
    fMRounded   = BigDecimal(fiveMax).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble

    println("Day " + day + ". " + "Account ID: " + id + " " +  "Maximum: " + fMRounded + " " + "Average: " + fARounded + " " +  "AA: " + fAARounded + " "+ "CC: " + fCCRounded +" "+ "FF: " + fFFRounded)
    Q3.write("Day " + day + ". " + "Account ID: " + id + " " +  "Maximum: " + fMRounded + " " + "Average: " + fARounded + " " +  "AA: " + fAARounded + " "+ "CC: " + fCCRounded +" "+ "FF: " + fFFRounded)
    Q3.write("\n")

    // Let's reset all of our values here.
    fTRounded   = 0.00
    fiveTotal   = 0.00
    fiveAverage = 0.00
    fiveAA      = 0.00
    fiveCC      = 0.00
    fiveFF      = 0.00
    fiveMax     = 0.00
    dayMax      = 0.00
    count       = 0
    totalCount  = 0
  }


  println()
  println("Question 3")
  println()

  // Create a new day variable to loop through.
  // Keep in mind that starting on day 1 would not give us anything.
  // This is because it would try to look at dates from day -4 to 0 which obviously has no transactions.
  // Note that because of this, we also finish on day 30 despite there being no transactions on this day.
  // Also even if there have been no transactions for the past 5 days, the account will still produce a line, it just has all the values as 0.
  var day2 = 2
  while(day2 < 31){
    for(x <- newIds.reverse){
      fiveDays(transactions, day2, x)
      dayTotal = 0.00
      aaTotal =0.00
      ccTotal =0.00
      ffTotal =0.00
      fiveMax = 0.00
    }
    day2 = day2 + 1
  }
  Q3.close()
}

package warmup

import org.scalacheck.Prop._
import org.scalacheck.Prop
import org.scalacheck.Properties

import collection.mutable.Stack
import scala.collection.mutable
import scala.collection.mutable.Map


object Exercises {


  /* assignment 1:

      calculate the _index_ of the first of occurrence of the maximum of an array, give -1 if the array is empty
      (there is no index of the maximum)

      examples:
        indexOfMax(Array(6,5,4,3,6)) == 0
        indexOfMax(Array(4,5,6,3,6)) == 2
        indexOfMax(Array()) == -1

   */
  def indexOfMax(a : Array[Int]) : Int = {
    if (a.isEmpty) return -1
    else {
      var indexOfMax = 0
      for (i <- 0 until a.length) {
        if (a(i) > a(indexOfMax)) {
          indexOfMax = i
        }
      }
      return indexOfMax
    }
  }



  /* Assignment 2:

  calculate the average exam score from a string containing a list of grades. A grade is either a number in the 1-10
  range or the string "NS" for no show. No show grades have no effect on the average grade.

  examples:

  averageGrade("NS NS 9 NS NS 5") == 7
  averageGrade("8 8.2 NS NS 9 4 1 5.5") == 5.95

   */

  def averageGrade(grades : String) : Double = {
    var totalSum : Double = 0
    var numberOfGrades : Double = 0
    var averageGrade : Double = -1

    for(el <- grades.split(" ")) {
      if (el != "NS"){
        totalSum += el.toDouble
        numberOfGrades += 1
      }
    }

    averageGrade = totalSum/numberOfGrades.toDouble
    return averageGrade
  }

    /* Assignment 3:

      An infamous unsolved problem in discrete math is the
      "Collatz conjecture" (https://en.wikipedia.org/wiki/Collatz_conjecture).


      For any positive integer n, the corresponding "Collatz sequence" starts with the number n.
      The next number in the sequence is defined as follows:
          if n is even, the next number is n/2
          if n is odd,  the next number is 3n + 1
      The subsequencent numbers are constructed in the same way.
      For example, if we start with 11, the corresponding Collatz sequence is:
      11,34,17,52,26,13,40,20,10,5,16,8,4,2,1,4,2,1,4,...
      Once a Collatz sequence has reached 1, the sequence will repeat the pattern 4,2,1 forever.

      The Collatz conjecture states that, no matter which integer we start with, the collatz sequence will
      eventually reach 1 and repeat the pattern 4,2,1 forever. The conjecture is probably true, and has been checked
      by computers for all starting values up to 87×2^60(!). However, there is no mathematical proof for seemingly
      simple statement. There have been  mathematicians that have spent years of continued study on the conjecture,
      without success.Mathematical professor Jeffrey Lagarias has written a book about Collatz conjecture, and stated
      that "this is an extraordinarily difficult problem, completely out of reach of present day mathematics".

      Luckily, programming the Collatz sequence is much easier! Please implement a function which when given a starting
       number, returns the number of elements in the sequence before reaching 1. For example:
       collatzLength(11) = 14
       collatzLength(1) = 0
       collatzLength(4) = 2
       collatzLength(7) = 16
       collatzLength(27) = 111
     */



  def collatzLength(start : Long) : Long = {
    var currentNumber = start
    var length = 0
    while (currentNumber != 1){
      if (isOdd(currentNumber)){
        currentNumber = currentNumber * 3 + 1
        length += 1
      }
      else {
        currentNumber /= 2
        length += 1
      }
    }
    return length
  }

  def isOdd(number: Long): Boolean = {
    if ((number%2)==0) return false
    else return true
  }


  /* Assignment 4: Write a function that produces "ASCII art" of a diamond of height n such the following diamond
   of height 7 :

   #
  ###
 #####
#######
 #####
  ###
   #


Diamond of height 12

     #
    ###
   #####
  #######
 #########
###########
###########
 #########
  #######
   #####
    ###
     #


Note that the last character of each line should be # and that hence the line should not end in spaces.
   */

  def diamondString(height : Int) : String = {
    var currentHeight = 0

    val initNumberOfSpaces = (height - 1)/2
    val initNumberOfSymbols = 1

    var numberOfSpaces = initNumberOfSpaces
    var numberOfSymbols = initNumberOfSymbols

    var diamond = """""".stripMargin

    while (numberOfSpaces > 0) {

      for (sp <- 0 until numberOfSpaces) {
        diamond += """ """
      }
      for (sy <- 0 until numberOfSymbols) {
        diamond += """#"""
      }
      diamond +=
        """
          |""".stripMargin
      numberOfSpaces -= 1
      numberOfSymbols += 2
    }

    if ((height % 2) == 0){
      for (sp <- 0 until numberOfSpaces) {
        diamond += """ """
      }
      for (sy <- 0 until numberOfSymbols) {
        diamond += """#"""
      }
      diamond +=
        """
          |""".stripMargin
    }

    while (numberOfSymbols >= 1) {
      for (sp <- 0 until numberOfSpaces) {
        diamond += """ """
      }
      for (sy <- 0 until numberOfSymbols) {
        diamond += """#"""
      }
      if (numberOfSymbols > 1) {
        diamond +=
          """
            |""".stripMargin
      }
      numberOfSpaces += 1
      numberOfSymbols -= 2
    }
    return diamond
  }


  /* Assignment 5: Implement a function word count that gives the word counts for all words in a string.
  Ignore whitespace and capitalization.

   Examples:
    "the monkey hugs the monkey" gives Map(the -> 2, monkey -> 2, hugs -> 1)

  "A horse, a horse! My kingdom for a horse!" gives Map(for -> 1, a -> 3, my -> 1, horse -> 3, kingdom -> 1)


  "A woman is but a woman. A man is but a man. A child is but a child."

  gives

  Map(is -> 3, but -> 3, man -> 2, child -> 2, woman -> 2, a -> 6)

   */

  def wordCount(text : String) : mutable.Map[String,Int] = {
    val count = mutable.Map.empty[String, Int].withDefaultValue(0)
    for (el <- text.split("[ ,!.]+")) {
      val word = el.toLowerCase
      count(word) += 1
    }
    return count
  }



  /* Assignment 6: Elo ratings

  In chess and various other forms of competition the skill of a player is indicated using
  the Elo-rating system. The elo-rating is a number in the 0-3000 range, where 0 is an absolute beginner and
  2900 is the world champion.

  When given the ratings of two players rA and rB, the probability of a winning the game is calculated as follows:

  eA = 1 / ( 1 + 10^((rB - ra) / 400)))

  This gives a number between 0 (100 % sure that a loses) and 1 (100 % sure that a wins).

  After playing a game or tournament, the elo ratings of the player are updated based on their wins and loses. The
  new rating of a player is calculated based on the probability of a winning eA and the actual outcome aA.

  rA' = rA + k * (aA - eA)

  where k is a constant controlling the importance of the game (typically between 16-32). We use k=24.

  For example, if player a has an elo rating of 2000 and player b has a elo rating of 2200, then the probability of
  player a winning is 1 / ( 1 + 10^((2200 - 2000) / 400))) = 0.24. Suppose player a wins the game, so aA=1.0.
  Then the new elo score of player a is 2000 + 24 * (1.0 - 0.24) = 2018.24. For player b the computation is reversed,
  his probability of winning was (1 - 0.24) = 0.76. The new elo score of player b is
  2200 + 24 * (0.0 - 0.76) = 2181.76

  NOTICE: During a tournament, the rating of players is not changed until the tournament finishes, after which all
  games are processed and everyone's new elo score is determined. The elo scores of the players do not change during
   the tournament, only after. 

  Program a method which updates the elo ratings of all players. You can (and should) add method to classes below.


  We test this as follows:

    val jaap = new Player("Jaap",2000)
    val piet = new Player("Piet", 2200)
    val game = new Game(jaap,piet,0.0)
    Exercises.updateEloScores(List(jaap,piet), List(game) )
    assert(jaap.rating === 2018.24 +- 0.01)
    assert(piet.rating === 2181.76 +- 0.01)

   */

  val eloK = 24

  def updateEloScores(players : List[Player] , games : List[Game]) : Unit = {
    for (game <- games){
      if(game.outcome == 0.5){
        game.playerA.updateRating(game.playerB.initRating , 0.5)
        game.playerB.updateRating(game.playerA.initRating, 0.5)
      }
      else if(game.outcome == 0.0){
        game.playerA.updateRating(game.playerB.initRating, 1.0)
        game.playerB.updateRating(game.playerA.initRating, 0.0)
      }
      else {
        game.playerA.updateRating(game.playerB.initRating, 0.0)
        game.playerB.updateRating(game.playerA.initRating, 1.0)
      }
    }
  }

  class Player(
                val name : String,
                var rating : Double) {
    val initRating: Double ={
      rating
    }
    def updateRating(opponentRating: Double , outcome: Double): Unit ={
      rating = rating + eloK * (outcome - (1 / ( 1 + scala.math.pow(10,(opponentRating - initRating) / 400) )))
    }
  }

  class Game(
              val playerA : Player,
              val playerB : Player,
              val outcome : Double, // 0 means playerA won, 1 means playerB won, 0.5 means draw
            ) {
  }

  /* Assignment 7: List speed offenders.

  Two sets of cameras have been set up on the A2 highway near Utrecht to find speed offenders. The sets of cameras,
  named "A" and "B", are 1500 meters apart and the speed limit is 100 km/h between 6:00 and 19:00 and 120 km/h between
  19:00 and 6:00. If a car passed both cameras in a (single) 6:00 to 19:00 period then it is going too fast if its speed
  is over 100 km/h. If a car passed either camera in between 19:00 and 6:00, then it is going too fast if its speed is over
  120 km/h.

  The cameras are only positioned on the North-bound direction of the highway, and the cars first pass the "A" cameras
  and then the "B" cameras. After processing, the cameras produce a list of cars and the times they passed as
  follows (definitions of Observation and Time below):

  Observation(cameraSet = "A", licensePlate = "DX-98-DW", time = Time(18492, 13, 3, 5.0))
  Observation(cameraSet = "B", licensePlate = "WW-11-XX", time = Time(18492, 13, 3, 6.0))
  Observation(cameraSet = "A", licensePlate = "MV-33-PP", time = Time(18492, 13, 3, 6.2))
  Observation(cameraSet = "A", licensePlate = "33-XX-RR", time = Time(18492, 13, 3, 6.7))
  ....
  Observation(cameraSet = "A", licensePlate = "DX-98-DW", time = Time(18492, 13, 4, 1.0))

  Program a method that takes the observations of the cameras and produces a list of speed offenders and their speed
  in the order that the cars passed cameraset B.

   ----> Use a map of type Map[String,Time] to store & look up for each license plate at which time cameraset "A" was passed.

   */

  // The used epoch is 1 January 1970. An epoch is an instant in time chosen as the origin of a time scale.
  // (see https://en.wikipedia.org/wiki/Epoch)
  case class Time(daysSinceEpoch : Int, hours : Int, minutes : Int, seconds : Double)
  // case class means (among other things) that you do not have to type new to create one
  // so instead of new Time(43,6,3,0) you just type Time(43,6,3,0)
  // equality and pretty printing are also defined for you


  case class Observation(cameraSet : String, licensePlate : String, time : Time )

  // to convert your speed of type double to an Int use Math.round(speed).toInt
  case class SpeedOffender(licensePlate : String, speed : Int)

  val distanceAB = 1500

  val speedLimitDay: Double = 100/3.6

  val speedLimitNight: Double = 120/3.6

  def speedOffenders(observations: Seq[Observation]) : Seq[SpeedOffender] = {
    val firstPass = mutable.Map.empty[String, Time]
    val secondPass = mutable.Map.empty[String, Time]
    var speedOffenders: Seq[SpeedOffender] = Seq()

    for(ob <- observations){
      if(ob.cameraSet == "A") {
        firstPass += (ob.licensePlate -> ob.time)
      }
      else secondPass += (ob.licensePlate -> ob.time)
    }

    for (carA <- firstPass){
      val timeA = carA._2
      for (carB <- secondPass){
        if (carA._1 == carB._1){
          val timeB = carB._2
          if (overSpeedLimit(timeA, timeB)._1){
            speedOffenders = speedOffenders :+ SpeedOffender(carA._1, Math.round(overSpeedLimit(timeA, timeB)._2*3.6).toInt)
          }
        }
      }
    }

    return speedOffenders.sortBy(_.speed).reverse
  }

  def overSpeedLimit(timeA: Time, timeB: Time): (Boolean, Double) = {
    var timeInSeconds = 0.0
    var speed = 0.0

    if (timeA.daysSinceEpoch == timeB.daysSinceEpoch){
      timeInSeconds = (timeB.seconds + timeB.minutes * 60 + timeB.hours * 3600) - (timeA.seconds + timeA.minutes * 60 + timeA.hours * 3600)
      speed = 1500/timeInSeconds
    }
    else {
      timeInSeconds = (60 - timeA.seconds) + ( (59 - timeA.minutes) * 60 ) + ( (23 - timeA.hours) * 3600 ) + timeB.hours * 3600 + timeB.minutes * 60 + timeB.seconds
      speed = 1500/timeInSeconds
    }

    if (6 <= timeA.hours && timeA.hours < 19){
      return ( (speedLimitDay < speed), speed)
    }
    else {
      return ( (speedLimitNight < speed), speed)
    }
  }

  /* Assignment 8: Program a method that split any non-empty array into two arrays.

  If the input array has length l , the first array should have length (l / 2),
  the second array should have length (l - (l / 2)). For example, for
  length 7, the first array has length 3, while the second has length 4.

  If we put the second resulting array behind the first one, we should get the original array back.

  example: [5,3,10,5,6,1,3] -> ([5,3,10],[5,6,1,3])

   */
  def splitArray(a : Array[Int]) : (Array[Int],Array[Int]) = {
    var firstArray: Array[Int] = Array()
    var secondArray: Array[Int] = Array()

    for (i <- 0 until a.length/2){
      firstArray = firstArray :+ a(i)
    }

    for (i <- a.length/2 until a.length){
      secondArray = secondArray :+ a(i)
    }

    return (firstArray, secondArray)
  }


  /* Assignment 9:

     Program a method that when given two sorted arrays a and b, returns a new sorted array c that has the elements from
     array a and array b. For example
     when given
     a = [1,3,5,6,10]
     b = [1,4,6,8]

     the resulting array should be:

     c = [1,1,3,4,5,6,6,9,10]

     This method should not call a sorting method. Instead the resulting array should be produced by "zipping" the
     two input arrays together. We repeatedly select the least element that we did not consider before from a and b
     and include this in c.

     For example:

     a = [1,3,5,6,10]
              ^
     b = [1,4,6,8]
            ^
     c = [1,1,3,...]
      the arrows (^) point to the lowest element we did not consider before. Of these, the element 4 from b is less
      then the element 5 from a. For this reason, we select 4 as the next element and advance the arrow ^ for b to point
      to 6.

   */
  def mergeSortedArrays(a : Array[Int], b : Array[Int]) : Array[Int] = {
    var c : Array[Int] = Array()

    var i : Int = 0
    var j : Int = 0

    while (c.length < (a.length + b.length)){
      if (i == a.length){
        c = c :+ b(j)
        j += 1
      }
      else if(j == b.length){
        c = c :+ a(i)
        i += 1
      }
      else {
        if (a(i) < b(j)) {
          c = c :+ a(i)
          i += 1
        }
        else {
          c = c :+ b(j)
          j += 1
        }
      }
    }

    return c
  }





  /* Assignment 10 : Mergesort

  Implement mergesort. Mergesort is a recursive sorting algorithm that works by splitting arrays and merging them.
  More precisely, mergeSort works as follows:

   If the input array is of length <= 1, then the array is already sorted an the input array is returned.
   If the input array is of length >= 2, then the input array is split in two (using the splitArray function),
   the two smaller arrays are sorted using mergesort, and the resulting two sorted arrays are merged using
   mergeSortedArrays.

   Example:
        7 5 6 1    -- split
       /      \     -- merge sort left and right recursively
     7 5     6 1     -- split both sides
     /  \    /  \
    7   5   6   1     -- merge both sides
    \   /   \  /    -- left and right merge sort done
    5 7     1 6    -- merge
     \       /
      1 5 6 7
   */

  def mergeSort(a : Array[Int]) : Array[Int] = {
    if (sorted(a)){
      return a
    }
    else {
      mergeSortedArrays(mergeSort(splitArray(a)._1), mergeSort(splitArray(a)._2))
    }
  }

  def sorted(a : Array[Int]) : Boolean = {
    for (i <- 0 until (a.length - 1)){
      if (a(i) > a(i + 1)){
        return false
      }
    }
    true
  }
}




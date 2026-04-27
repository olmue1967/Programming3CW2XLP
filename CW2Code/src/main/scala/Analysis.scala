import Main.players
class Analysis {

//----------------------------------Get Recent Week Points - Part 1-----------------------------------

// Pure function to get the recent week points of each player
def getRecentWeek(players: List[Player]): (List[(String, Int)]) = {

 //val recentWeek = players.map(_.getPlayerPoints().last) - commented out as is a procedural method to get the last week points score - not functional/recursive method
 
 
 players.map(player => (player.getPlayerName(), getRecentWeekPoints(player.getPlayerPoints())))

}

//get the last week points score using recursive/functional methods which is called by the recentWeeks variable in the getRecentWeek method
def getRecentWeekPoints(list: List[Int]): Int = list match {
  case head :: Nil => head
  case _ :: tail => getRecentWeekPoints(tail)
}

// Impure function to get the datta and write to screen
def displayRecentWeek(players: List[Player]) = {

  // get the recent scores of each player
  val recentWeeks = getRecentWeek(players)

  //using case to match the name and score of each player and print it out in a formatted way
  recentWeeks.map {case (name, score) => 
      println("---------------Part 1---------------")
      println(s" $name's Recent week score: $score")
  }

}

//--------------------------------------End of Part 1----------------------------------------------





//--------------------------------------Highest & Lowest Scores - Part 2----------------------------------------------

//pure function to get the highest and lowest scores of each player
def getMaxMinPlayerScores(players: List[Player]): (List[(String, Int, Int)])  = {

  players.map(player => 
    (player.getPlayerName(), 
    minVal(player.getPlayerPoints()), 
    maxVal(player.getPlayerPoints())))
}

//using foldleft to find the minimum and maximum values in a list of integers
//here the 1000 value is used to make a starting point for the system to evaluate a minimum element
//
  def minVal(list: List[Int]): Int = {
    list.foldLeft(1000) { (accumulator, nextElement) =>
    if (nextElement < accumulator) nextElement else accumulator
    }
  }

def maxVal(list: List[Int]): Int = {
  list.foldLeft(0) { (accumulator, nextElement) =>
  if (nextElement > accumulator) nextElement else accumulator
  }
}

def maxValImperative(list: List[Int]): Int = {
  var max = 0
  for (num <- list) {
    if (num > max) {
      max = num
    }
  }
  max
}

def displayPlayerScores(players: List[Player])= {

  val playerScores = getMaxMinPlayerScores(players)

  playerScores.foreach { case (name, min, max) =>
    
    println("--------------Part 2--------------")
    println(s"$name's Highest Score: $max")
    println(s"$name's Lowest Score: $min")
  }
}
//--------------------------------------End of Part 2---------------------------------------




//------------------------------Names and Total Points of those +500 - Part 3--------------------------


def totalPointsOver500(players: List[Player]): List[Player] = {
  players.filter(player => totalPoints(player.getPlayerPoints()) > 500)
}

def totalPoints(list: List[Int]): Int = {
  list.reduceLeft { (accumulator, nextElement) =>
  accumulator + nextElement
  }
}

def displayTotalPointsOver500(players: List[Player])= {

  val playersOver500 = totalPointsOver500(players)

playersOver500.map { player =>
    println("--------------Part 3--------------")
    println(s"${player.getPlayerName()} has total points over 500 with a total of ${totalPoints(player.getPlayerPoints())}")
  }
}
//------------------------------End of Part 3--------------------------


//------------------------------Month Average Points Comparison of Two Players - Part 4--------------------------
//defining function to return a tuple of the 2 selected averages
def monthAveragePoints(players: List[Player], player1Name: String, player2Name: String, month: Int): (Int, Int) = {

  val player1Option = players.find(_.getPlayerName() == player1Name)
  val player2Option = players.find(_.getPlayerName() == player2Name)

  (player1Option, player2Option) match {
    case (Some(player1), Some(player2)) =>
      val player1MonthPoints = player1.getPlayerPoints().slice((month - 1) * 4, month * 4)
      val player2MonthPoints = player2.getPlayerPoints().slice((month - 1) * 4, month * 4)

      //val player1Average = if (player1MonthPoints.nonEmpty) totalPoints(player1MonthPoints) / player1MonthPoints.size else 0
      val player1Average = if (player1MonthPoints.nonEmpty) getAverage(player1MonthPoints, 0, 0) else 0
      //val player2Average = if (player2MonthPoints.nonEmpty) totalPoints(player2MonthPoints) / player2MonthPoints.size else 0
      val player2Average = if (player2MonthPoints.nonEmpty) getAverage(player2MonthPoints, 0, 0) else 0

      (player1Average, player2Average)
  
  }
}


def getAverage(list: List[Int], acc: Int, size: Int): Int = {

  list match {
    case Nil => acc / size
    case head :: tail => getAverage(tail, acc + head, size + 1)
  }
}
 
def getAverage2(list: List[Int], acc: Int, size: Int): Int = {
  if (list.isEmpty) {
      acc / size
  } else {
    getAverage(list.tail, acc + list.head, size + 1)
  }
}


def displayMonthAveragePoints(players: List[Player], player1Name: String, player2Name: String, month: Int): Unit = {

  val (player1Average, player2Average) = monthAveragePoints(players, player1Name, player2Name, month)

  println("--------------Part 4--------------")
  println(s"$player1Name's average points for month $month: $player1Average" + s"\n$player2Name's average points for month $month: $player2Average")
}

//------------------------------End of Part 4--------------------------

//------------------------------Accumulated Team Points - Part 5--------------------------
def accumulatedTeamPoints(players: List[Player], teamPlayers: List[String], week: Int): Int = {
  val teamPlayerObjects = players.filter(player => teamPlayers.contains(player.getPlayerName()))
  //teamPlayerObjects.map(player => totalPoints(player.getPlayerPoints())).sum
  totalPlayerPoints(teamPlayerObjects, 0, 0, week)
}

def totalPlayerPoints(list: List[Player], acc: Int, size: Int, week: Int): Int = {

  var runningTotal = acc

  list match {
    case Nil => acc // removed the add + size as we want the total points not the total + the number of players in the team
    case head :: tail => {
      //runningTotal = runningTotal + totalPoints(head.getPlayerPoints().slice(0, week)) // here we are calling the weeksPlayerPoints function to get the total points for the specified week for each player in the team and adding it to the running total
      //totalPlayerPoints(tail, runningTotal , size + 1, week)
      val runningTotal = acc + totalPoints(head.getPlayerPoints().slice(0, week)) // here we are calling the weeksPlayerPoints function to get the total points for the specified week for each player in the team and adding it to the running total
      totalPlayerPoints(tail, runningTotal , size + 1, week)
    }
  }
}


def displayAccumulatedTeamPoints(players: List[Player], teamPlayers: List[String], week: Int): Unit = {
  val totalTeamPoints = accumulatedTeamPoints(players, teamPlayers, week)

  players.filter(player => teamPlayers.contains(player.getPlayerName())).map(player => println(s"${player.getPlayerName()}'s points up until week $week: ${totalPoints(player.getPlayerPoints().slice(0, week))}"))

  println(s"Accumulated Team Points: $totalTeamPoints")
}
}
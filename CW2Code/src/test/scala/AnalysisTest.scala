import org.scalatest.funsuite.AnyFunSuite

class AnalysisTest extends AnyFunSuite {

  //test 1--------------------------------------
  // test the pure function getRecentWeek
  test("testRecentWeek") {

    //create input
    val testPlayers = List(
      new Player("Daizen", List(10, 20, 30, 40, 50)),
      new Player("Dembele", List(5, 15, 25, 35, 45)),
      new Player("Edouard", List(0, 10, 20, 30, 40))
    )

    //create what we expect

    val expectedLastWeekPoints = List(
      ("Daizen", 50),
      ("Dembele", 45),
      ("Edouard", 40)
    ) 

    //run the pure function
    val analysis = new Analysis()    

    val actualLastWeekPoints = analysis.getRecentWeek(testPlayers)

    //check this against what we expected

    assert(actualLastWeekPoints == expectedLastWeekPoints)

}
//end of test 1--------------------------------------

//test 2--------------------------------------
// test the pure function getMaxMinPlayerScores
test("testMaxMinPlayerScores") {  
  
  //create input
  val testPlayers = List(
    new Player("Daizen", List(10, 20, 30, 40, 50)),
    new Player("Dembele", List(5, 15, 25, 35, 45)),
    new Player("Edouard", List(0, 10, 20, 30, 40))
  )

  //create what we expect

  val expectedMaxMinScores = List(
    ("Daizen", 10, 50),
    ("Dembele", 5, 45),
    ("Edouard", 0, 40)
  ) 

  //run the pure function
  val analysis = new Analysis()    

  val actualMaxMinScores = analysis.getMaxMinPlayerScores(testPlayers)

  //check this against what we expected

  assert(actualMaxMinScores == expectedMaxMinScores)
}
//end of test 2--------------------------------------

//test 3--------------------------------------
// test the pure function totalPointsOver500

test("testTotalPointsOver500") {  
  
  //create input
  val testPlayers = List(
    new Player("Daizen", List(120, 100, 100, 90, 140, 110, 100, 90)),
    new Player("Dembele", List(100, 150, 125, 95, 115, 120, 90, 100)),
    new Player("Edouard", List(100, 90, 20, 70, 40, 50, 80, 45))
  )

  //create what we expect

  val expectedTotalPointsOver500 = List(
    ("Daizen", 850),
    ("Dembele", 895)   
  ) 

  //run the pure function
  val analysis = new Analysis()    

  val actualTotalPointsOver500 = analysis.totalPointsOver500(testPlayers).map(player => (player.getPlayerName(), analysis.totalPoints(player.getPlayerPoints())))

  //check this against what we expected

  assert(actualTotalPointsOver500 == expectedTotalPointsOver500)
}
//end of test 3--------------------------------------

//test 4--------------------------------------
// test the pure function monthAveragePoints
test("testMonthAveragePoints") {  
  
  //create input
  val testPlayers = List(
    new Player("Daizen", List(120, 100, 100, 90, 140, 110, 100, 90)),
    new Player("Dembele", List(100, 150, 125, 95, 115, 120, 90, 100)),
    new Player("Edouard", List(100, 90, 20, 70, 40, 50, 80, 45))
  )

  //create what we expect

  val expectedMonthAveragePoints = (110, 106) //Daizen's average for month 2 is (100 + 90 + 140 + 110) / 4 = 110, while Dembele's average for month 2 is (125 + 95 + 115 + 120) / 4 = 106.25, which we round down to 106

  //run the pure function
  val analysis = new Analysis()    

  val actualMonthAveragePoints = analysis.monthAveragePoints(testPlayers, "Daizen", "Dembele", 2)

  //check this against what we expected

  assert(actualMonthAveragePoints == expectedMonthAveragePoints)
}
//end of test 4--------------------------------------

//test 5--------------------------------------
// test the pure function accumulatedTeamPoints
test("testAccumulatedTeamPoints") {
  //create input
  val testPlayers = List(
    new Player("Daizen", List(120, 100, 100, 90, 140, 110, 100, 90)),
    new Player("Dembele", List(100, 150, 125, 95, 115, 120, 90, 100)),
    new Player("Edouard", List(100, 90, 20, 70, 40, 50, 80, 45))
  )

  val testTeamPlayers = List("Daizen", "Edouard")

  //create what we expect

  val expectedAccumulatedTeamPoints = 760 + 450 // Daizen's total points is 760 and Edouard's total points is 450

  //run the pure function
  val analysis = new Analysis()    

  val week = 7 // we want to calculate the accumulated points for the team up until week 7

  val actualAccumulatedTeamPoints = analysis.accumulatedTeamPoints(testPlayers, testTeamPlayers, week)

  //check this against what we expected

  assert(actualAccumulatedTeamPoints == expectedAccumulatedTeamPoints)
}
//end of test 5--------------------------------------

//notes
//Test 5 assisted in the discovery of a fairly important bug in the accumulatedTeamPoints function 
//where I was adding the size of the team to the total points which was incorrect 
//as we only want the total points not the total points + the number of players in the team. 
//I had to fix this bug in the Analysis.scala file and once updated, invoked test 5 to verify the fix.
}

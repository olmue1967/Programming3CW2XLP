import scala.io.Source
object Main extends App {

// create an empty list to hold player objects
var players = List[Player]()

// start the application running
start
menu

//The function that runs when the application starts. 
def start: Unit = {
  // read in a file and print its content
  val filename = "data.txt"
  val source = Source.fromFile(filename)
  val lines = source.getLines().toList


  // create player object
  lines.map(splitPlayerData).map{
  case (name, scores) => players = players :+ new Player(name, scores)
  }

  source.close()

  // show all players and their scores
  ///players.map { player =>
  ///println(s"Player: ${player.getPlayerName()}, Scores: ${player.getPlayerPoints().mkString(", ")}")
  ///}
}

def menu: Unit = {
  println("--------------Menu--------------")
  println("0. Exit") 
  println("1. Show Recent Week Points")
  println("2. Show Highest and Lowest Scores")
  println("3. Show Names and Total Points of those +500")
  println("4. Month Average Points of Two Players")
  println("5. Accumulated Team Points")

  val choice = scala.io.StdIn.readInt()

    // My analysis functions class
    val analysis = new Analysis()    

  choice match {
    case 0 => System.exit(0)  // shut down program
    case 1 => analysis.displayRecentWeek(players)
    case 2 => analysis.displayPlayerScores(players)
    case 3 => analysis.displayTotalPointsOver500(players)
    case 4 => {
                val player1Name = scala.io.StdIn.readLine("Please enter Player 1 Name: ")
                val player2Name = scala.io.StdIn.readLine("Please enter Player 2 Name: ")
                println("please enter the month period to compare (1-5): ")
                val chosenMonth = scala.io.StdIn.readInt()
                // call the impure displayMonthAveragePoints function which in turn uses the pure monthAveragePoints function to get the averages
                analysis.displayMonthAveragePoints(players, player1Name, player2Name, chosenMonth)
              }
    case 5 => {
                val userTeamName = scala.io.StdIn.readLine("Please enter Team Name: ")
                var playerInTeam =scala.io.StdIn.readLine("Please enter each player in team and press enter after each player (type 'done' when finished): ")
                
                var teamPlayers = List[String]()

                //adding inital player into the team if the user has not exited the process
                if (playerInTeam != "done" && players.exists(_.getPlayerName() == playerInTeam)) {
                  teamPlayers = teamPlayers :+ playerInTeam
                }
                else if (playerInTeam != "done") {
                  println(s"Player $playerInTeam does not exist. Please enter a valid player name.")
                }

                while (playerInTeam != "done") {

                  playerInTeam = scala.io.StdIn.readLine("Please enter next player in team (type 'done' when finished): ")
                  
                  if (playerInTeam != "done" && players.exists(_.getPlayerName() == playerInTeam)) {
                  teamPlayers = teamPlayers :+ playerInTeam
                  } 
                  else if (playerInTeam != "done") {
                    println(s"Player $playerInTeam does not exist. Please enter a valid player name.")
                  }
                }        

                println("please enter the week period to compare up til (1-20): ")
                val chosenWeek = scala.io.StdIn.readInt()


                println(s"Calculating accumulated points for team $userTeamName with players: ${teamPlayers.mkString(", ")}")
                analysis.displayAccumulatedTeamPoints(players, teamPlayers, chosenWeek)
              }
      case _ => println("Invalid choice. Please try again.")
    }
    menu
}

  // Get the lines of players into a list of player and their scores
  def splitPlayerData(data: String): (String, List[Int]) = {
    val parts = data.split(",")
    val name = parts.head.trim
    val scores = parts.tail.map(_.trim.toInt).toList

    (name, scores)
  }
}

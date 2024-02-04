#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=rep( c("goat", "car"), c(2,1) ), size=3, replace=FALSE )
    return( a.game )
}



#' @title
#'  Select a random door
#' @description
#'  'select_door()' selects a door at random.
#' @details
#'  The function works as an initial selection for which door the contestant chooses.
#' @param
#'  ... no arguments used by the function.
#' @return
#'  The function returns a randomly selected number between 1 and 3.
#' @examples
#'   select_door()
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}




#' @title
#'  Open 1 of 3 doors.
#' @description
#'  open_goat_door() selects one of the three available doors.
#' @details
#'  The function will reveal if there is a goat or car behind the door.
#' @param
#'  Uses 2 arguments. 'game' where "car" indicates a car and "goat" indicates a
#'  goat and 'a.pick' which is the index of the door that a player picks.
#' @return
#'  Function returns the index of the opened door, which is an integer between 1
#'  and 3.
#' @examples
#'  opened.door <- open_goat_door(this.game, my.initial.pick)
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats
   if( game[ a.pick ] == "car" )
   {
     goat.doors <- doors[ game != "car" ]
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   {
     opened.door <- doors[ game != "car" & doors != a.pick ]
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#'  Change Door Selection or Stay.
#' @description
#'  Simulate the decision to either stay or change their pick.
#' @details
#'  Creates a vector for 3 doors. The function then checks the value of the stay
#'  argument, either TRUE or FALSE. If the player stays, final.pick is set to a.pick.
#'  If the player chooses not to stay, the function checks remaining options that
#'  was not opened.
#' @param
#'  Uses 3 arguments: stay = TRUE, opened.door object, and a.pick object.
#' @return
#'  Returns a value between 1 and 3.
#' @examples
#'  final.pick.stay <- change_door(stay = TRUE, opened.door = opened.door, a.pick = my.initial.pick)
#'  final.pick.switch <- change_door(stay = FALSE, opened.door = opened.door, a.pick = my.initial.pick)
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3)

   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ]
   }

   return( final.pick )  # number between 1 and 3
}




#' @title
#'  Determine the Winner.
#' @description
#'  Function determines the winner of the game.
#' @details
#'  If the final pick is "car", then the player wins. If the final pick is "goat"
#'  the player loses.
#' @param
#'  Uses 2 arguments: final.pick (either switch or stay) and "game" which is the
#'  current game.
#' @return
#'  "WIN" or "LOSE" based on the game's outcome.
#' @examples
#'  game.outcome.stay <- determine_winner( final.pick=final.pick.stay, game=this.game )
#'  game.outcome.switch <- determine_winner( final.pick=final.pick.switch, game=this.game )
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}



#' @title
#'  Play a Game
#' @description
#'  Create and play a game of Monty Hall.
#' @details
#'  This function runs the entire game and then catalogs the outcomes into a
#'  dataframe. The outcomes will show some percentage of wins and losses based on
#'  the strategy of staying and switching.
#' @param
#'  ... no arguments are used in this function.
#' @return
#'  Returns a 2x2 dataframe that shows if the strategy of "stay" or "switch" is
#'  a win or a loss.
#' @examples
#'  game.outcome <- play_game()
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )

  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}



#' @title
#'  Play the game 100x.
#' @description
#'  Continually simulates playing the game 100 times.
#' @details
#'  This function will continually simulate the play_game() function 100 times. It
#'  then will store the results into results.list and then bind them to a new data
#'  frame, results.df. This will then run a statistical analysis of the game results
#'  and generate a prop.table detailing the results.
#' @param
#'  uses the function( n=100 ) to iterate a simulation of 100x.
#' @return
#'  Returns a dataframe of game results based on strategy of "stay" or "switch" along
#'  with the corresponding statistical percentages.
#' @examples
#'  play_n_games()
#' @export
play_n_games <- function( n=100 )
{

  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome
    loop.count <- loop.count + 1
  }

  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>%
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>%
  print()

  return( results.df )

}

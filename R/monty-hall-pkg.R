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
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title 
#'  Randomly choose the first door to open
#' @description
#'  `select_door()` randomly chooses a number 1, 2, or 3, 
#'  corresponding to the three available doors, to open first.
#' @details
#'   This function acts as the first main step for a participant
#'   in the Monty Hall game, where they are presented three doors
#'   and choose one at random. The participant does not know 
#'   what is behind any of the doors.
#' @param 
#'  no arguments are used by this function
#' @return 
#'  the function returns a single numeric, either 1, 2, or 3.
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
#'  Host opens a goat door
#' @description
#'  `open_goat_door()` first determines if the participant
#'  selected a door with a car or a goat. Then, the function
#'  chooses a different door with a goat behind it to 
#'  "reveal" the goat.
#' @details
#'  This function acts as the second part to the game, where
#'  the host reveals a door with a goat behind it. The host
#'  must choose a door with a goat behind it. If the 
#'  participant initially chose a goat door, the host will 
#'  reveal the other. If the participant initially chose 
#'  the door with the car, then the host can choose 
#'  either of the two un-selected doors. 
#' @param 
#'  The function uses the arguments of "game" and "a.pick",
#'  typically taken from the previous two functions.
#' @return 
#'  the function returns a single numeric, either 1, 2, or 3.
#' @examples
#'   open_goat_door(c("goat","car","goat"), 1)
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
#'  Choose to stay with the initial door or change to the final door
#' @description
#'  `change_door()` shows the result of if the contestant wants to 
#'  keep the door they initially selected, or switch to the other 
#'  unopened door.
#' @details
#'  This function represents the third part of the game, once the 
#'  host has revealed a door with a goat behind it. The participant
#'  can either keep the door they initially selected, or change to
#'  the other unopened door.
#' @param 
#'  This function requires the arguments of "stay" (either TRUE or
#'  FALSE), "opened.door", and "a.pick". The two later arguments
#'  come from previous functions, the "stay" argument is input
#'  by the user.
#' @return 
#'  the function returns a single numeric, either 1, 2, or 3.
#' @examples
#'   change_door(stay = TRUE, 3, 1)
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
#'  Determine if the contestant won or lost the game
#' @description
#'  `determine_winner()` identifies what item was behind the door
#'  that the contestant ultimately chooses, and whether or not
#'  they won or lost the game based on the item behind that door
#' @details
#'  This function is the final part of the game. The contestant
#'  has chosen the initial door, the host has reveled a goat, 
#'  and then the contestant has chosen whether to stick with 
#'  their initial door choice or change to the other door. Now,
#'  depending on their choice and what was behind the door, 
#'  the final selected door is revealed to either be a goat 
#'  (lost the game) or car (won the game).
#' @param 
#'  This function requires the arguments of "final.pick" and
#'  "game", both typically from previous functions. 
#' @return 
#'  The function returns a single character item of either 
#'  "WIN" or "LOSE"
#' @examples
#'   determine_winner(1, c("goat","car","goat"))
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
#'  Runs a complete Monty Hall Game simulation
#' @description
#'  `play_game()` takes all previous functions in the package
#'  and wraps them into a single function. 
#' @details
#'  This function runs two complete Monty Hall Game 
#'  simulations from the previously used functions. This package
#'  simulates one game where the contestant keeps their initial
#'  door choice, and another where the contestant changes to
#'  the other un-opened door.
#' @param 
#'  No arguments are used by this function
#' @return 
#'  The function returns a 2 x 2 data frame, showing the two 
#'  strategies used in the game (stay = TRUE or stay = FALSE)
#'  and the results of each strategy (WIN or LOSE).
#' @examples
#'   play_game()
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
#'  Simulate the Monty Hall game any number of times
#' @description
#'  `play_n_games()` allows the user to determine the number
#'  of times they want to run the Monty Hall simulation.
#' @details
#'  This function lets the user choose how many times they
#'  want to run the previous "play_game()" function. This 
#'  allows the user to simulate the function many times to
#'  determine which strategy, staying or switching, is the
#'  best strategy for winning.
#' @param 
#'  The function requires an argument of a numeric, with the
#'  default being 100.
#' @return 
#'  The function returns a 3 x 3 table showing the two 
#'  strategies (stay = TRUE or stay = FALSE) and then the 
#'  number of WINs and LOSSes for both. The number of 
#'  WINs and LOSSes should correspond with the numeric that
#'  was input for the arguments function.
#' @examples
#'   play_n_games(50)
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

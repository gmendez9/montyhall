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
#' This is a select door function.
#' @description
#' This select door function enables a player to select a door for their first case 
#' @details
#' The contestant makes their first selection, and this function enables us to choose one door at random.
#' @param ... Here, the arguments stored in the a.pick vector,  samples from our doors vector which contains three option choices. Per the offcial documentation," sample takes a sample of the specified size from the elements of x using either with or without replacement". The size argument within represents a non-negative integer specifying the number of items to choose from. 
#' @return - we are returned a.pick, which stores the sample taken from the door options
#' @examples
#' select_door()
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title - Open Goat Door
#' @description - The Game Host Opens a Goat Door, a door with a Goat behind it. 
#' @details
#' 
#' The host will always open a door with a goat behind it. But it can’t be a door the contestant has already selected. So it must be a door that is not a car and not a current contestant selection.Note that if the contestant selects the car on the first guess the host can open either door, but if the contestant selects a goat the host only has one option.
#' @param - the parametrs we setup consist of if statement whuich capture the possibility of the contestant selecting a door with a car or goat behind it, which influences the hosts decision of opening a certain door. Either way, the opened door vector uses the sample function to ensure we open a goat door.
#' @return - we are returned with an opened door, which contains a goat
#' @examples
#' this.game <- create_game()
#' this.game
#' my.initial.pick <- select_door()
#' my.initial.pick
#' open_goat_door( this.game, my.initial.pick )
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



#' @title - this is called the change_door function 
#' @description - The contestant is given the option to change from their initial selection to the other door that is still closed. The function will represent the game-playing strategy as the argument stay=TRUE or stay=FALSE.
#' @details - in the change_door obeject, we initiate a function which by default has stay = TRUE, which indicates
#' that the contestant has stayed. We utilize our opened.door and a.pick objects, as well as bring in our doors argument. we then use if statements to come to our final pick.  
#' @param - The arguments here are if statements which account for whether or not a contestant decides to stay with their original pick, which would then make their final pick their initial pick represented by a.pick. If they deicde to switch or not stay, then their final pick will represent a choice from the doors that are not an opened door, and no a,pick. 
#' @return we are returned a final.pick, or the contestant's final selection
#' @examples - # test it
#'opened.door <- open_goat_door( this.game, my.initial.pick )
#'
#'change_door( stay=T, 
#'            opened.door=opened.door, 
#'             a.pick=my.initial.pick )
#'change_door( stay=F, 
#'            opened.door=opened.door, 
#'            a.pick=my.initial.pick )
#'
#'my.final.pick <- change_door( stay=F, 
#'                              opened.door=opened.door, 
#'                              a.pick=my.initial.pick )
#'
#'this.game
#'my.initial.pick
#'my.final.pick 
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



#' @title - this is the determine a winner function
#' @description - the function will enable us to decide whether or not a contestant has won a car. 
#' @details - not much else to share here.
#' @param - in terms of the parameters here, our first argument articulates that if a contestant chooses the car door, they win. The second argument articulates that if they choose a goat door as their final pick, they lose,
#' @return - we are returned win or lose as a result of two options.
#' @examples
#' # test code
#'
#'this.game
#'my.initial.pick
#'
#'my.final.pick <- change_door( stay=T, 
#'                              opened.door=opened.door, 
#'                              a.pick=my.initial.pick )
#'
#'determine_winner( final.pick=my.final.pick, 
#'                  game=this.game )
#'
#'my.final.pick <- change_door( stay=F, 
#'                              opened.door=opened.door, 
#'                             a.pick=my.initial.pick )
#'
#'determine_winner( final.pick=my.final.pick, 
#'                  game=this.game )
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





#' @title - the play game wrapper function
#' @description - Using the functions we've initially created, we package them all together into a single play_game() function which executes each step of a single game in order.
#' @details - please refer to previous descriptions. We must account for all pieces of the game inside of the function. We are setting ourselves up for simuulating the game many times. 
#' @param 
#' new.game <- creates a new game
#' first.pick <- selects door
#' opened.door <- references the goat door function and uses the current game and first pick
#' final pick stay <- represents the change door instructions in case a contestant stays with their initial pick
#' final pick switch <- represents the change door instructions in case a contestant decides to switch
#' outcome.stay and outcome.switch leverage the previous two lines of codes to account for the possibilities 
#' strategy is a vector which uses concatenate our stay and switch options
#' outcomes stores our outcome.stay and outcome.switch functions whihc store our determine winner function
#'  game.switch created a data frame out of strategy and outcome
#' @return - we are returned with game.results 
#' @examples
#' play_game()
#' 
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






#' @title - the game loop 
#' @description - we construct a loop in order to simulate the goat game as manu times as we want. 
#' @details
#' When running simulations for inferential purposes the general rule of thumb is they should run at least 10,000 times in order for the simulated statistics to converge close to the actual theoretial value (average proportion of wins achieved by each strategy in this case).
#' @param - here, we set n to 100, which means we will play the game 100 times. In addition, we add a collector calld results.list, as well as an iterator which will run the actual simulation. Enclosed inside our simulator are game.outcome, results.list, as well as loop.count. The game.outcome stores the play_game function, while results.list stores loop count and feeds it the game.outcome. the loop count is then incremented by one. 
#' @return - we are returned a results dataframe which binds rows from the results list collector. We then execute a table command using the results.df and convert our results into proportions. 
#' @examples
#' ##         outcome
## strategy LOSE  WIN
##   stay   0.66 0.34
##   switch 0.34 0.66
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

#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game_5_doors()` generates a new game that consists of 5 doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are five doors for a contestant
#'   to choose from, one of which has a car behind it and four 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened doors. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 5 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game_5_doors()
#'
#' @export
create_game_5_doors <- function( )
{
  a.game <- sample( x=c("goat","goat","car","goat","goat"), size=5, replace=F )
  return( a.game )
} 



#' @title
#'   Make an initial selection as contestant in the 5 door Monty Hall game.
#'
#' @description
#'   `select_door_5_doors()` selects a door in the 5 door Monty Hall game 
#'   as the contestant
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are five doors for a contestant
#'   to choose from, one of which has a car behind it and four 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened doors. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a single door value, 1-5
#'
#' @examples
#'   select_door_5_doors()
#'
#' @export
select_door_5_doors <- function( )
{
  
  doors <- c(1,2,3,4,5) 
  a.pick <- sample(x=doors, size=1)
  return( a.pick )  # number between 1 and 5
  
}



#' @title
#'   Host selects one of the goat doors.
#'
#' @description
#'   `open_goat_door_5_doors()` selects one of the other goat doors as 
#'   the host. This door is always a goat door, but is never the same 
#'   door that the contestant selects.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are five doors for a contestant
#'   to choose from, one of which has a car behind it and four 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened doors. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param game Vector with the doors made by create_game_5_doors().
#' @param a.pick is the door selected by the contestant.
#' @return The function returns a single door value, 1-5
#'
#' @examples
#'   newgame <- create_game_5_doors()
#'   my.initial.pick <- select_door_5_doors()
#'   open_goat_door_5_doors(newgame,my.initial.pick)
#' @export
open_goat_door_5_doors <- function( game, a.pick )
{
  doors <- c(1,2,3,4,5)
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
  return( opened.door ) # number between 1 and 5
}



#' @title
#'   Selects final door based on strategy chosen.
#'
#' @description
#'   `change_door_5_doors()` selects a door based on the strategy in the 
#'   params. If the strategy is stay, the contestant keeps the pick
#'   from select_door. If not, they choose a new door that is neither
#'   the door from select_door_5_doors nor open_goat_door_5_doors
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are five doors for a contestant
#'   to choose from, one of which has a car behind it and four 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened doors. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param stay Boolean describing strategy of stay or switch. Switch = False
#' @param opened.door Door with goat opened by the host.
#' @param a.pick Door opened by the contestant.
#' 
#' @return The function returns a single door value, 1-5
#'
#' @examples
#'   change_door_5_doors(stay=T,opened.door=2,a.pick=1)
#' @export
change_door_5_doors <- function( stay=T, opened.door, a.pick )
{
  
  door_indices <- c(1,2,3,4,5)
  final_door_indeces <- door_indices[!door_indices %in% c(a.pick, opened.door)]
  final_door <- sample(x=final_door_indeces, size=1)
  
  if (stay==T) {
    final.pick <- a.pick
  } else {
    final.pick <- door_indices[final_door]
  }
  
  return( final.pick )  # number between 1 and 5
  
}



#' @title
#'   Checks final door chosen and determines whether it is a winner, i.e.
#'   car is behind door.
#'
#' @description
#'   `determine_winner()` determines whether a car is behind the door 
#'   chosen in change_door function.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are five doors for a contestant
#'   to choose from, one of which has a car behind it and four 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened doors. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param final.pick Final door picked by change_door_5_doors
#' @param game Vector containing the doors and what's behind them.
#' 
#' @return The function returns "WIN" or "LOSE"
#'
#' @examples
#'   new.game <- create_game_5_doors()
#'   a.pick <- select_door_5_doors()
#'   opened.door <- open_goat_door_5_doors(new.game,a.pick)
#'   final.pick <- change_door_5_doors(stay=T,opened.door,a.pick)
#'   determine_winner(final.pick,new.game)
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
#'   Plays an entire game.
#'
#' @description
#'   `play_game_5_doors()` Wraps up all the functions prior into one.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are five doors for a contestant
#'   to choose from, one of which has a car behind it and four 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened doors. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no parameters
#' 
#' @return The function returns a dataframe with two columns, strategy and outcome.
#' The strategy column has stay and switch, and the outcome is whether or not the
#' strategy led to a win or loss.
#' 
#'#'   new.game <- create_game_5_doors()
#'   a.pick <- select_door_5_doors()
#'   opened.door <- open_goat_door_5_doors(new.game,a.pick)
#'   final.pick <- change_door_5_doors(stay=T,opened.door,a.pick)
#'   determine_winner(final.pick,new.game)

#' @examples
#'   play_game_5_doors()
#' @export
play_game_5_doors <- function( )
{
  new.game <- create_game_5_doors()
  first.pick <- select_door_5_doors()
  opened.door <- open_goat_door_5_doors( new.game, first.pick )
  
  final.pick.stay <- change_door_5_doors( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door_5_doors( stay=F, opened.door, first.pick )
  
  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#'   Plays N number of games and returns a dataframe with the results of all N
#'   games.
#'
#' @description
#'   `play_n_games_5_doors()` loops play_game n times and appends the returned dataframe
#'   to a list results.list. After all N games have been played, the list is binded
#'   into a dataframe results.df via bind_rows. A table is produced to show the 
#'   percentages of wins and losses for each strategy.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are five doors for a contestant
#'   to choose from, one of which has a car behind it and four 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened doors. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param n The number of games to simulate.
#' 
#' @return The function returns a dataframe results.df
#'
#' @examples
#'   play_n_games_5_doors(100)
#' @export
play_n_games_5_doors <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1
  
  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game_5_doors()
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

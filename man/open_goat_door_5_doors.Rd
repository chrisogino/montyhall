% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-pkg_Lab02.R
\name{open_goat_door_5_doors}
\alias{open_goat_door_5_doors}
\title{Host selects one of the goat doors.}
\usage{
open_goat_door_5_doors(game, a.pick)
}
\arguments{
\item{game}{Vector with the doors made by create_game_5_doors().}

\item{a.pick}{is the door selected by the contestant.}
}
\value{
The function returns a single door value, 1-5
}
\description{
\code{open_goat_door_5_doors()} selects one of the other goat doors as
the host. This door is always a goat door, but is never the same
door that the contestant selects.
}
\details{
The game setup replicates the game on the TV show "Let's
Make a Deal" where there are five doors for a contestant
to choose from, one of which has a car behind it and four
have goats. The contestant selects a door, then the host
opens a door to reveal a goat, and then the contestant is
given an opportunity to stay with their original selection
or switch to the other unopened doors. There was a famous
debate about whether it was optimal to stay or switch when
given the option to switch, so this simulation was created
to test both strategies.
}
\examples{
  newgame <- create_game_5_doors()
  my.initial.pick <- select_door_5_doors()
  open_goat_door_5_doors(newgame,my.initial.pick)
}

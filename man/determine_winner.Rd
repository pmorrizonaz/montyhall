% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-pkg.R
\name{determine_winner}
\alias{determine_winner}
\title{Determine if the contestant won or lost the game}
\usage{
determine_winner(final.pick, game)
}
\arguments{
\item{This}{function requires the arguments of "final.pick" and
"game", both typically from previous functions.}
}
\value{
The function returns a single character item of either
"WIN" or "LOSE"
}
\description{
\code{determine_winner()} identifies what item was behind the door
that the contestant ultimately chooses, and whether or not
they won or lost the game based on the item behind that door
}
\details{
This function is the final part of the game. The contestant
has chosen the initial door, the host has reveled a goat,
and then the contestant has chosen whether to stick with
their initial door choice or change to the other door. Now,
depending on their choice and what was behind the door,
the final selected door is revealed to either be a goat
(lost the game) or car (won the game).
}
\examples{
  determine_winner(1, c("goat","car","goat"))
}

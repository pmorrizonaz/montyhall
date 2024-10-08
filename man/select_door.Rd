% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-pkg.R
\name{select_door}
\alias{select_door}
\title{Randomly choose the first door to open}
\usage{
select_door()
}
\arguments{
\item{no}{arguments are used by this function}
}
\value{
the function returns a single numeric, either 1, 2, or 3.
}
\description{
\code{select_door()} randomly chooses a number 1, 2, or 3,
corresponding to the three available doors, to open first.
}
\details{
This function acts as the first main step for a participant
in the Monty Hall game, where they are presented three doors
and choose one at random. The participant does not know
what is behind any of the doors.
}
\examples{
  select_door()
}

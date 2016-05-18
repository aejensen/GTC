\name{guessTheCorrelation}
\alias{guessTheCorrelation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  The game "Guess The Correlation".
}
\description{
  An R implementation of "Guess the Correlation" - a fun and wholesome game for the whole family. The player is presented with a scatter plot of random data generated from a standard bivariate normal distribution with an unknown correlation coefficient (rho). The objective is to guess rho. Se Details for the rules.
}
\usage{
guessTheCorrelation(n=NA, population=FALSE, variation=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
    The number of random pairs to generate in each round.

    If n = NA (default) a random number of random pairs are generated in each round chosen uniformly in seq(50, 250, by=50).
}
  \item{population}{
    If population = TRUE the task is to guess the population correlation from which the random sample was generated.

    If population = FALSE (default) the task is to guess the sample correlation coefficient computed from the randomly generated sample.
}
\item{variation}{
  Not implemented yet.
}
}
\details{
The player has a number of lives and a certain amount of coins. At the beginning of the game these are initialized to 5 coins and 3 lives.

If the player guesses the correlation coefficient within +/- 0.05 of the true value, the player is awarded an extra life and 5 coins.

If the player guesses the correlation coefficient within +/- 0.1 of the true value, the player is awarded an extra coin.

If the guess exceeds +/- 0.1 the player loses a life.

When there are no lives left it's GAME OVER!

The true correlation coefficients and the guesses are rounded to two decimal places.
}
\value{
A vector of signed differences ([-2;2]) between the guesses and the true correlation coefficients (population or sample depending on the  argument).
}
\references{
http://guessthecorrelation.com/
}
\author{
  A. K. Jensen
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
\dontrun{stats <- guessTheCorrelation(n=100)}
}


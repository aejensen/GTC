getInput <- function() {
  n <- readline(prompt="Enter your guess: ")
  n <- as.numeric(n)
  if (is.na(n)) {
  	cat("Invalid input. Guess must be numeric in the interval [-1;1].\n")
  	n <- getInput()
  }

  if(abs(n) > 1) {
  	cat("Invalid input. Guess must be numeric in the interval [-1;1].\n")
  	n <- getInput()
  }
  return(round(n, 2))
}

plotValues <- function(true, guess, diff) {
  text(3, 4, labels=bquote(rho == .(true)), pos=4, cex=1.5)
  text(3, 3.5, labels=bquote(widehat(rho) == .(guess)), pos=4, cex=1.5)

  diffCol <- ifelse(abs(diff) > 0.1, "red", "black")
  text(3, 3, labels=bquote(delta == .(diff)), pos=4, cex=1.5, col=diffCol)
}

plotPlayerState <- function(ps) {
  if(ps$life > 0) {
    for(k in 1:ps$life) {
      points(-4 + 0.2*(k-1), 4, pch=169, font=5, cex=2, col="red")
    }
  }

	if(ps$coin > 0) {
    points(-4, 3.5, pch=160, font=5, cex=2, col="black")
    text(-3.8, 3.5, labels=paste(" ", ps$coin, sep=""), cex=1.8)
    #for(k in 1:ps$coin) {
    #  points(-4 + 0.2*(k-1), 3.5, pch=160, font=5, cex=2, col="black")
    #}
	}
}

updatePlayerState <- function(ps, diff) {
  if(abs(diff) <= 0.05) {
  	cat("You got an extra life and 5 coins!\n")
    return(list(coin = ps$coin+5, life = ps$life + 1))
  }

	if(abs(diff) > 0.05 & abs(diff) <= 0.10) {
		cat("You got 1 coin!\n")
		return(list(coin = ps$coin + 1, life = ps$life))
	}

	if(abs(diff) > 0.10) {
		cat("You lost a life! :(\n")
		return(list(coin = ps$coin, life = ps$life - 1))
	}
}


guessTheCorrelation <- function(n=NA, population=FALSE, variation=1) {
  playerState <- list(coin = 5, life = 3)
  diffHistory <- c()

  #Main loop - repeat until death
  repeat {
    #Sample a random population correlation coefficient
    rhoPopulation <- round(runif(1, -1, 1), 2)

    #Generate random sample size
    if(is.na(n)) {
    	N <- sample(seq(50, 250, by = 50), 1)
    } else {
    	N <- n
    }

    #Generate random sample
    covMat <- matrix(rhoPopulation, 2, 2)
    diag(covMat) <- 1
    dat <- mvtnorm::rmvnorm(n = N, sigma = covMat)

    #Calculate sample correlation coefficient
    rhoSample <- round(cor(dat[,1], dat[,2]), 2)

    #Plot data and current player state
    plot(dat[,1], dat[,2], xlim = c(-4,4), ylim = c(-4,4), bty = "n", pch=19, xlab = "X", ylab = "Y")
    title("Guess The Correlation")
    #title(rhoSample) #(debug)
    plotPlayerState(playerState)

    #Get guess from user
    rhoGuess <- getInput()

    #Difference between guess and sample/population coefficient
    difference <- rhoGuess - ifelse(population, rhoPopulation, rhoSample)
    diffHistory <- c(diffHistory, difference) #save for return

    #Plot true, guess and difference
    plotValues(ifelse(population, rhoPopulation, rhoSample), rhoGuess, difference)

    #Update player state
    playerState <- updatePlayerState(playerState, difference)

    if(playerState$life < 0) {
    	#Are we off-by-one (<=) here?
  	  cat("--------- GAME OVER ---------\n")
  	  return(diffHistory)
    }

    end <- readline(prompt="Press [Enter] for next round.")
  }
}

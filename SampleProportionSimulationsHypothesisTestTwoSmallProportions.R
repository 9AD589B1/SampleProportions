#If we consider the Mythbusters experiment of people being able to identify
#the back of their hands, and include the ability to identify the palm of
#their hand, the data is as follows:

#          Back   Palm   Total
#Correct   11     7      18
#Incorrect 1      5      6
#Total     12     12     24
#          0.9167 0.5833 0.75


#Do these data provide convincing evidence that there is a difference in how
#good people are at recognizing the backs and the palms of their hands?

#Since our sample sizes are too small, we will need to use simulation for hypothesis test

rm (list = ls (all = TRUE))

suppressWarnings ( suppressMessages ( library ( dplyr ) ) )

set.seed (123)

ObservedDiff <- (11/12) - (7/12)

CorrectGuesses <- c(rep.int(1, 18))
IncorrectGuesses <- c(rep.int(0, 6))
CombinedGuesses <- c(CorrectGuesses, IncorrectGuesses)

nrows <- 10000

GuessTrials <- matrix(NA, nrow = nrows, ncol = 24)

#Create matrix of simulated guess trials
for (n in 1:nrows)
  GuessTrials[n, ] <-
  c(unlist(split(
    sample(CombinedGuesses), sample(rep(1:2, c(12, 12)), replace = FALSE)
  ), use.names = FALSE))

#Coerce matrix to a data frame
GuessTrials <- as.data.frame (GuessTrials)

#From the simulated trials, calculate the difference of correct back of hand and correct palm of hand guesses
GuessesSuccessPercent <-
  data.frame (
    BackSuccess = apply (GuessTrials[1:12], 1, mean, na.rm=TRUE),
    PalmSuccess = apply (GuessTrials[13:24], 1, mean, na.rm=TRUE),
    #Note that with "SuccessDiff," we want to use the absolute value so we capture the both tails
    SuccessDiff = abs (apply (GuessTrials[1:12], 1, mean, na.rm=TRUE) - apply (GuessTrials[13:24], 1, mean, na.rm=TRUE))
  )

#From the calculated differences of guess success, determine the percent of the trials that are
#at least as extreme as the observed difference. This will be our p-value
GuessSuccessTotalPercentvsObserved <- GuessesSuccessPercent %>%
  summarise (
    TotalSuccessCount   = sum(SuccessDiff >= ObservedDiff),
    TotalSuccessPercent = sum(SuccessDiff >= ObservedDiff) / nrows
  )

GuessSuccessTotalPercentvsObserved$TotalSuccessPercent #Display the P-Value

#Since our P-Value is 0.1501, we don't reject the null hypothesis

#GuessesSuccessTotal <-
#  data.frame (
#    BackSuccessTotal = apply (GuessTrials[1:12], 1, function(x)
#      sum(x == 1)),
#    PalmSuccessTotal = apply (GuessTrials[13:24], 1, function(x)
#      sum(x == 1))
#  )
#
#GuessSuccessTotalPercent <- GuessesSuccessTotal %>%
#  summarise (
#    TotalBackSuccessCount   = sum(BackSuccessTotal),
#    TotalPalmSuccessCount   = sum(PalmSuccessTotal),
#    TotalBackSuccessPercent = sum(BackSuccessTotal) / (12 * nrows),
#    TotalPalmSuccessPercent = sum(PalmSuccessTotal) / (12 * nrows),
#    TotalSuccessPercentDiff = (sum(BackSuccessTotal) / (12 * nrows)) - (sum(PalmSuccessTotal) / (12 * nrows))
#  )

#Difference in Proportions. Needs to be tweaked
#backtrials <- (12 * nrows)
#palmtrials <- (12 * nrows)
#phatback <- GuessSuccessTotalPercent$TotalBackSuccessPercent
#phatpalm <- GuessSuccessTotalPercent$TotalPalmSuccessPercent
#phatpool <- (GuessSuccessTotalPercent$TotalBackSuccessCount + GuessSuccessTotalPercent$TotalPalmSuccessCount) / (24 * nrows)
#nonphatpool <- 1 - phatpool
#nullvalue <- 0
#pointestimate <- abs ( phatback - phatpalm )
#se <- sqrt ( ( ( phatpool * nonphatpool ) / backtrials ) + ( ( phatpool * nonphatpool ) / palmtrials ) )
#z <- ( pointestimate - nullvalue ) / se
#pvalue <- pnorm ( abs(z), lower.tail = FALSE )

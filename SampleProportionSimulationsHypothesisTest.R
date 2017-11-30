#Paul the Octopus predicted 8 World Cup games, and predicted them all
#correctly. Does this provide convincing evidence that Paul actually has psychic
#powers, i.e. that he does better than just randomly guessing?

rm ( list = ls ( all = TRUE ) )

suppressWarnings ( suppressMessages ( library ( dplyr ) ) )

#simulate a 8 coin tosses 1,000,000 times
set.seed (123) #set seed to ensure reproducibility
n <- 8 #number of coin toss to simulate
trials <- 1000000 #number of trials to perform
CoinFlipTrials <-
  as.data.frame (matrix (sample (c(1, 0), n * trials, rep = T), trials, n)) #Create a dataframe of the trials

CoinFlipTrialMeans <- data.frame ( TrialMean = apply ( CoinFlipTrials, 1, mean) ) #Create a dataframe of trial means

#hist(CoinFlipTrialMeans$TrialMean)

#Calculate the count and percent of successes, where success is 8 out of 8 heads
HeadsTotalSuccess <- CoinFlipTrialMeans %>%
  summarise (
    TotalSuccessCount   = sum(TrialMean == 1),
    TotalSuccessPercent = sum(TrialMean == 1) / trials
  )

#Our alternative hypothesis is Ha > 0.5, and our null hypothesis is Ho = 0.5. Our test
#statistic is 0.003943, which is the proportion of 8 of 8 heads from our 1,000,000 trials

p <- 0.5 #test statistic
phat <- HeadsTotalSuccess$TotalSuccessPercent #sample proportion
nonp <- 1 - p
se <- sqrt((p * nonp) / n) #calculate the standard error
z <- (phat - p) / se #calculate the Z-score
prob <- pnorm(z) #calculate the area under the curve
prob
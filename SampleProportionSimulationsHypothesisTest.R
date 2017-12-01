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

#Our probability is less than 1%, so we can reject the null hypothesis

#There is a saying in English "to know something like the back of your hand", which
#means to know something very well. MythBusters (a popular TV show) put to test the validity of this saying.
#They recruited 12 volunteers, each of whom were shown 10 pictures of backs of
#hands (while wearing gloves so they couldn't see their own hands), and asked them
#to identify their own hand among the 10 pictures. 11 out of 12 people completed
#the task successfully.
#What are the hypotheses for evaluating whether these data provide convincing
#evidence of the validity of the saying, i.e. that people do better than random guessing
#when it comes to recognizing the back of their own hand?

rm ( list = ls ( all = TRUE ) )

#simulate 12 selections 1,000,000 times
set.seed (123) #set seed to ensure reproducibility
n <- 12 #number of people in the experiment
trials <- 1000000 #number of trials to perform
GuessTrials <-
  as.data.frame (matrix (sample (c(1:10), n * trials, rep = T), trials, n)) #Create a dataframe of the trials

GuessTrialSuccess <- data.frame ( TrialSuccess = apply ( GuessTrials, 1, function(x)sum(x == 1) / 12 ) ) #Create a dataframe of trial successes

GuessTotalSuccess <- GuessTrialSuccess %>%
  summarise (
    TotalSuccessCount   = sum(TrialSuccess >= 11/12),
    TotalSuccessPercent = sum(TrialSuccess >= 11/12) / trials
  )

#hist(GuessTrialMeans$TrialMean)

#Our alternative hypothesis is Ha > 0.1, and our null hypothesis is Ho = 0.1. Our test
#statistic is 0.0, which is the proportion of 11/12 correct guesses from our 1,000,000 trials

p <- 0.1 #test statistic
phat <- GuessTotalSuccess$TotalSuccessPercent #sample proportion
nonp <- 1 - p
se <- sqrt((p * nonp) / n) #calculate the standard error
z <- (phat - p) / se #calculate the Z-score
prob <- pnorm(z) #calculate the area under the curve
prob
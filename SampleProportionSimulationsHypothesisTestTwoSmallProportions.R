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

set.seed (123)

CorrectGuesses <- c(rep.int(1, 18))
IncorrectGuesses <- c(rep.int(0, 6))
CombinedGuesses <- c(CorrectGuesses, IncorrectGuesses)

nrows <- 1000000
Guesses <- matrix(NA, nrow = nrows, ncol = 24)

for (n in 1:nrows)
  Guesses[n, ] <-
  c(unlist(split(
    sample(CombinedGuesses), sample(rep(1:2, c(12, 12)), replace = FALSE)
  ), use.names = FALSE))

Guesses <- as.data.frame (Guesses)

GuessesSuccess <-
  data.frame (
    BackSuccess = apply (Guesses[1:12], 1, mean, na.rm=TRUE),
    PalmSuccess = apply (Guesses[13:24], 1, mean, na.rm=TRUE)
  )
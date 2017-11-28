#The general social survey (GSS) found that 571/670 (85%) people answered the 
#question on experiment design correctly. Using a 95% confidence interval, we want
#to estimate the proportion of all Americans who have good intuition about experiment design. 

rm ( list = ls ( all = TRUE ) ) #clear variables

n <- 670 #sample size
phat <- 571 / 670 #calculate the sample proportion
nonphat <- 1 - phat
z <- 1.96 #Z-Score for the the 95% confidence intervale
se <- sqrt ((phat * nonphat) / n)
CI <- c(phat - se, phat + se)
CI

#If, for a new confidence interval based on a new sample, we want to reduce 
#the margin of error to 1% while keeping the confidence level the same. 
#At least how many respondents should we sample?

#We would solve for n: 0.01 = 1.96 * sqrt ( ( 0.85 * 0.15 ) / n )

n <- ceiling((z ^ 2 * phat * nonphat) / 0.01 ^ 2) #round up
n

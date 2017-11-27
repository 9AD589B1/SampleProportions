#The general social survey (GSS) found that 571/670 (85%) people answered the 
#question on experiment design correctly. Using a 95% confidence interval, we want
#to estimate the proportion of all Americans who have good intuition about experiment design. 

rm ( list = ls ( all = TRUE ) ) #clear variables

s <- 670 #sample size
phat <- 571 / 670 #calculate the sample proportion
nonphat <- 1 - phat
z <- 1.96 #Z-Score for the the 95% confidence intervale
se <- sqrt ((phat * nonphat) / s)
CI <- c(phat - se, phat + se)
CI


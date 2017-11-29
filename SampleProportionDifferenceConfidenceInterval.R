#In early October 2013, a Gallup poll asked "Do you think
#there should or should not be a law that would ban the
#possession of handguns, except by the police and other
#authorized persons?" Also, Coursera students were polled
#and the results are as follows:

#         Successes Sample Size  P Hat
#US       257       1028         0.25
#Coursera 59        83           0.71

#How do Coursera students and the American public at large
#compare with respect to their views on laws banning possession
#of handguns?

rm ( list = ls ( all = TRUE ) ) #clear variables

n1 <- 1028 #US sample size
n2 <- 83 #Coursera sample size
phat1 <- 257/1028 #sample proportion of US
nonphat1 <- 1 - phat1 
phat2 <- 59/83
nonphat2 <- 1 - phat2
phatdiff <- phat2 - phat1
z <- 1.96 #Z-score for 95% Confidence Level
se <- sqrt ( ( (phat1 * nonphat1) / n1) + (phat2 * nonphat2) / n2 ) #calculate standard error

#Confidence interval (95% Confidence Level) formula: ( Phat Coursera - Phat US ) +/- z * SE
CI <- c( phatdiff - ( z * se ), phatdiff + ( z * se ) ) #Calculate confidence interval
CI
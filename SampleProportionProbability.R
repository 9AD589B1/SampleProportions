#90% of all plant species are classified as angiosperms. These are flowering plants. 
#If you were to randomly sample 200 plants from the list of all known plant species, 
#what is the probability that at least 95% of the plants in your sample will be flowering plants?

rm ( list = ls ( all = TRUE ) ) #clear variables

n <- 200 #sample size
phat <- 0.95 #probability of our sample containing 95% angiosperms
p <- 0.9 #population proportion of angiosperms
nonp <- 1 - p #population proportion of non-angiosperms
x <- 0.9 #population mean
se <- sqrt ((p * nonp) / n) #Calculate the Standard Error
z <- ( phat - p ) / se #Calculate Z-Score
prob <- pnorm(z, lower.tail = FALSE) #calculate aread under the curve to obtain the probability
prob

#Alternatively, we can use a binomial distribution to calculate the probability

binomprob <- sum(dbinom(190:200, 200, 0.9))
binomprob
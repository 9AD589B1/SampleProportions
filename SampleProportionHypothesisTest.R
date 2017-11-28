#A 2013 Pew Research poll found that 60% of 1,983 randomly sampled American adults believe in evolution. 
#Does this provide convincing evidence that majority of Americans believe in evolution? 

#Our alternative hypothesis is Ha > 0.5, and our null hypothesis is Ho = 0.5. Our test
#statistic is 0.6 (60%)

rm ( list = ls ( all = TRUE ) ) #clear variables

p <- 0.5 #test statistic
phat <- 0.6 #sample proportion
nonp <- 1 - p
n <- 1983
se <- sqrt((p * nonp) / n) #calculate the standard error
z <- (phat - p) / se #calculate the Z-score
prob <- pnorm(z, lower.tail = FALSE) #calculate the area under the curve
prob

#A SurveyUSA poll asked respondents whether any of their
#children have ever been the victim of bullying. Also recorded on
#this survey was the gender of the respondent (the parent). Below
#is the distribution of responses by gender of the respondent.

#         Male Female
#Yes      34   61
#No       52   61
#Not Sure 4    0
#Total    90   122
#Phat     0.38 0.5
#

#Because we are using hypothesis testing for proportions, we need
#to calculate a pooled value to use in the standard error

rm ( list = ls ( all = TRUE ) ) #clear variables

nmales <- 90
nfemales <- 122
phatmales <- 34/90
phatfemales <- 61/122
phatpool <- ( 34 + 61 ) / ( 90 + 122 )
nonphatpool <- 1 - phatpool
pointestimate <- phatmales - phatfemales
nullvalue <- 0
se <- sqrt ( ( ( phatpool * nonphatpool ) / nmales ) + ( ( phatpool * nonphatpool ) / nfemales ) )
z <- ( pointestimate - nullvalue ) / se
pvalue <- pnorm ( abs(z), lower.tail = FALSE ) * 2
#Compute posterior distribution by hand
#Problem settings: 9 tosses, 6 success
#-----------------------------------------
# define grid
p_grid <- seq( from=0 , to=1 , length.out=100)
# define prior
prior <- rep(1, length(p_grid))
# compute likelihood at each value in grid, 9 toss, 6 sucessess
likelihood <- dbinom( 6 , size=9 , prob=p_grid )
plot(p_grid, likelihood)
# compute product of likelihood and prior
unstd.posterior <- likelihood * prior

# standardize the posterior, so it sums to 1
posterior <- unstd.posterior/ sum(unstd.posterior)
cat("sum is", sum(posterior), '\n')
plot( p_grid , posterior , type="b" ,
      xlab="probability of water" , ylab="posterior probability" )
mtext( "100 points" )

#---------------------------------------------
#Changing prior
#Now lest's making prior a normal distribution as a funtion of p_grid
# define grid
p_grid <- seq( from=0.001 , to=0.999 , length.out=100)
# define prior
#prior <- ifelse( p_grid < 0.5 , 0 , 1 )
prior <- dnorm(p_grid, mean=-2, sd = 1)
plot(p_grid, prior)

#Check if the intergration is less that 1. Note: A prior does not need to be intergrated to 1
#In this case we have improper prior
f <- function(x){dnorm(x, mean=-2, sd = 1)}
integrate(f, lower = 0, upper = 1)

# compute likelihood at each value in grid, 9 toss, 6 sucessess
likelihood <- dbinom( 6 , size=9 , prob=p_grid )
# compute product of likelihood and prior
unstd.posterior <- likelihood * prior

# standardize the posterior, so it sums to 1
posterior <- unstd.posterior/ sum(unstd.posterior)
cat("sum is", sum(posterior), '\n')
plot( p_grid , posterior , type="b" ,
      xlab="probability of water" , ylab="posterior probability" )
mtext( "100 points" )

#---------------------------------------------------------
#chaging prior again: 
#According to the literature 60-80% chances will get a success. 
#Take this information into account in your prior and calculate new posterior.
#In this case we need to calculate prior distribution
#make the mean of prior .7; let's make our prior a conjugate prior of posterior
#let's make a pseudo sample size as 100
#prior ~ beta(a,b); a = 0.7*100 = 70; b =0.3*100 = 30
#---------------------------------------------------------
# define grid
p_grid <- seq( from=0, to=1, length.out=100)
prior <-dbeta(p_grid,70,30)
plot(p_grid, prior)
# check intergration 
f <- function(x){dbeta(x,70,30)}
integrate(f, lower = 0, upper = 1)

# compute likelihood at each value in grid, 9 toss, 6 sucessess
likelihood <- dbinom( 6 , size=9 , prob=p_grid )
plot(p_grid, likelihood)

# compute product of likelihood and prior
unstd.posterior <- likelihood * prior

# standardize the posterior, so it sums to 1
posterior <- unstd.posterior/ sum(unstd.posterior)
cat("sum is", sum(posterior), '\n')
plot( p_grid , posterior , type="b" ,
      xlab="probability of water" , ylab="posterior probability" )
mtext( "100 points" )

# reference web: http://stats.stackexchange.com/questions/58564/help-me-understand-bayesian-prior-and-posterior-distributions
library(rethinking)
samples<-sample(p_grid, prob=posterior, size = 1000, replace=TRUE)
HPDI(samples, prob=.5)
PI(samples, prob =.5)


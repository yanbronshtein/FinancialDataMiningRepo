library(ISLR)
data("Portfolio")

alpha.fn = function(data, index) {
  X = data$X[index]
  Y = data$Y[index]
  return((var(Y) - cov(X,Y)) / (var(X) + var(Y) - 2*cov(X,Y)))
}

alpha.fn(Portfolio, 1:100)


#Now let us use a sample function to randomly select 100 observations 1:100 with
#replacement

set.seed(1)
alpha.fn(Portfolio, sample(100, 100, replace = T))


#We can implement a bootstrap analysis by performing this command many times,
#recording all corresponding estimates for alpha, and computing resulting standard
#dev. Boot() function automates this approach.
#Below we produce R = 1000 bootstrap estimates for alpha


boot(Portfolio,alpha.fn, R = 1000)
#The final output shows that using the original data, alpha hat = 0.5758 
#and that bootstrap estimate for SE is 0.0886


#Estimating accuracy of a linear regression model

boot.fn = function(data, index) {
  return(coef(lm(mpg ~ horsepower, data = data, subset = index)))
}
boot.fn(Auto, 1:392)

boot(Auto, boot.fn, 1000)

library(MASS) # Collection of data sets and functions
library(ISLR) # Contains datasets associated with the textbook
library(ggplot2)

#3.6.2 Simple Linear Regression
#We will seek to predict medv using 13 predictors such as rm(average # rooms per house)
#age(average age of houses)
#lsat(percent of households with low socioeconomic status)

fix(Boston)
names(Boston)
Boston


#Start by using the lm() function to fit a simple lienar regression model,
#with medv as the response and lsat as the predictor. The basic syntax is lm(y~x, data)
#where y is the response, x is the predictor, and data is the data set in which these 
#two variables are kept

lm.fit = lm(medv~lstat, data = Boston)
attach(Boston)
lm.fit = lm(medv~lstat)
summary(lm.fit)

names(lm.fit)

coef(lm.fit)

#obtain a confidence interval for the coefficient estimates
confint(lm.fit)

#lstat again is the lower status of the population as a percent
#The predict() function can be used to produce confidence intervals and prediction
#intervals for the prediction of medv for a given value of lstat.
predict(lm.fit, data.frame(lstat = (c(5, 10, 15))),
        interval = "confidence")


predict(lm.fit, data.frame(lstat = (c(5, 10, 15))),
        interval = "prediction")


#The 95% confidence interval associated with a lstat value of 10 is (24.47,  25.63)
#95% prediction interval is (12.828, 37.28). As expected, the confidence and prediction
# intervals are centered around the same point ( a predicted value of 25.05 ) for
# medv when lstat equals 10) but the latter are substanially wider

plot(lstat, medv)
ggplot(data = Boston, aes(x = lstat, y = medv)) + 
  geom_point() +
  geom_smooth()
#Here we find that there is some evidence for non-lienarity in the relationship
#between lstat and medv

#abline() function can be used to draw any line, not jus the least
# squares regression line
abline(lm.fit, lwd=3)
abline(lm.fit, lwd=3, col = "red")
plot(lstat, medv, col = "red")
plot(lstat, medv, pch=20)
plot(lstat, medv, pch="+")
plot(1:20, 1:20, pch = 1:20)

par(mfrow = c(2, 2))
plot(lm.fit)


#Alternatively, we can compute the residuals from a linear regression fit
#using the residuals() function. The function rstudent() will return the studentized 
#residuals, and we can use this function to plot the residuals against the fitted values
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
#On the basis of the residual plots, there is some evidence of non-linearity. 
#Leverage statistics can be computed for any number of predictors using hatvalues()
#function

plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

#3.6.3 Multiple Linear Regression
#Use the syntax lm(y~x1+x2+x3) with 3 predictors x1, x2, x3
lm.fit <- lm(medv~lstat+age, data = Boston)
summary(lm.fit)


# The beston data set contains 13 variables, and so it would be cumbersome
# so we do the following:
lm.fit <- lm(medv~ ., data = Boston)
summary(lm.fit)$r.squared

library(car)
vif(lm.fit) # compute the variance inflation factors


# Include all variables except age in the regression because it has a high p-value
lm.fit1 <- lm(medv~.-age, data = Boston) 
summary(lm.fit1)


#Or we can use the update() function
lm.fit1 <- update(lm.fit, ~.-age)
summary(lm.fit1)

#3.6.5 Non-linear Transformations of the Predictors

#Given predictor X, we can cratea  predictor X%2 using I(X^2).
# The function I() needed since ^ has special meaning. 
lm.fit2 <- lm(medv ~ lstat + I(lstat^2))
summary(lm.fit2)
#This was regression of medv onto lstat and lstat^2
# The near 0 P value aka Pr(>|t|) associated with the quadratic term
#suggest that it leads to an improved model.
# We use the anova() function to further quantify the extent to which the quadratic 
#fit is superior to the linear fit.
lm.fit <- lm(medv ~ lstat)
my_anova <- anova(lm.fit, lm.fit2)
my_anova
#Model 1: linear submodel with only one predictor
#Model2: Quadratic model with predictors: lstat and lstat^2
#anova() does hypothesis test comparing the two models. 
#H_0: the two models fit the data equally well.
#H_a: full model is superior.
#the F-statistic is 135 and the associated p-value is virtually 0.
#This provides clear evidence that the model containing the predictors lstat and 
#lstat^2 is far superior to the model that only contains the predictor lstat.
#

par(mfrow = c(2, 2))
#When lstat^2 is included in the model, there is little discernible pattern in the residuals

#Poly is  a better approach
lm.fit5 <- lm(medv ~ poly(lstat, 5))
summary(lm.fit5)
#This is even better. however, no polynomial terms beyond fifth order have significant
#p-values in a regression fit

summary(lm(medv ~ log(rm), data = Boston)) # Here we try a log transformation


#Qualitative Predictors
fix(Carseats)
names(Carseats)


# Given a qualitative variable such as Shelveloc, R generates dummy variables
# automatically. Below we fit a multiple regression model that includes some 
# interaction terms
lm.fit <- lm(Sales ~ . + Income:Advertising + Price:Age, data = Carseats)
summary(lm.fit)

attach(Carseats)
contrasts(ShelveLoc) # Returns the coding that R uses for the dummy variables
# R has create a ShelveLocGood dummy variable that takes on a value of 1 if the
#shelving location is good, and 0 otherwise.
# A bad shelving location corresponds to a zero for each of the two dummy variables
# The fact that the coefficient for ShelveLocGood in the regression output is positive
#indicates that a good shelving location is associated with high sales 
#(relative to a bad location). 
#And ShelveLocMedium has a smaller positive coefficient, indicating that 
# a medium shelving location leads to higher sales than a bad shelving location
# but lower sales than a good shelving location.


#3.6.7 Writing functions
LoadLibraries <- function() {
  library(ISLR)
  library(MASS)
  print("The libraries having been loaded")
}

LoadLibraries()

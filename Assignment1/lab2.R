library(ISLR)
fix(Hitters)
names(Hitters)
sum(is.na(Hitters))
dim(Hitters)
library(leaps)
regfit.full <- regsubsets(Salary~. , Hitters) # Performs subset selection 
#by identifying the best model that contains a given number of predictors
#where best is quantified using RSS. Syntax is the same as for lm()
#Asterisk means var included in model
#from the model we see that hits and CRBI must be included

summary(regfit.full)

#Lets fit up to 19 variable model
regfit.full <- regsubsets(Salary~., data=Hitters, nvmax = 19)
reg.summary <- summary(regfit.full)
names(reg.summary)
#We see that R^2 statistic increases monotonically as more variables are included
reg.summary$rsq

par(mfrow=c(2,2))
plot(reg.summary$rss, xlab = "Num variables", ylab = "RSS", type ="l") 
#type=l for line


#6.5.3 Choosing among models using the validation set approach and cross-validation

set.seed(1)
train <- sample(c(TRUE, FALSE), nrow(Hitters), rep = TRUE)
test = (!train) # Test is everything not in train

#Apply regsubsets() to the training set in order to perform best subset selection.
reg.fit.best <- regsubsets(Salary~., data=Hitters[train, ], nvmax = 19)
#We subset the Hitters df directly in the call in order to access only the training subset
# of the data, using the expression Hitters[train,].
test.mat <- model.matrix(Salary~., data = Hitters[test,])
dim(test.mat)
test.mat
#Use model.matrix() to build X matrix from data. 

#Run a loop. For each size i, extract the coeeffs from regfit.best for best
#model of that size, multiplying them into the appropriate columns of the test
#model matrix to form the predictions and compute the test MSE.
val.errors <- rep(NA, 19)
val.errors
for (i in 1:19) {
  coefi <- coef(reg.fit.best, id = i)
  pred <- test.mat[, names(coefi)] %*% coefi
  val.errors[i] <- mean((Hitters$Salary[test] - pred)^2)
}
which.max(reg.summary$adjr2)
points(11, reg.summary$adjr2[11], col = "red", cex = 2, pch=20)
plot(reg.summary$cp, xlab = "num of vars", ylab = "Cp", type = "l")
######################CODE ABOVE DOES NOT WORK WHAT DO I DO????########################################

#Function mimics above
predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}

regfit.best <- regsubsets(Salary~., data = Hitters, nvmax = 19)
regfit.best
coef(regfit.best, 10)
#Now we try to choose among the models of different sizes using cross-validation
#Perform best subset selection within each of the k training sets
#subsetting makes it easy
#1. Create a vector that allocates each observation to one of k=10 folds

k <- 10
set.seed(1)
folds <- sample(1:k, nrow(Hitters), replace = TRUE)
cv.errors <- matrix(NA, k, 19, dimnames = list(NULL, paste(1:19)))

#For loop performs cross-validation. In jth fold, elements of folds that equal
#j are in test set. rest is in the training set. 
#Use predict() for each model size using new predict method.
#compute the test erros on the appropriate subset,
#store them in the appropriate slot in the matrix cv.errors

for(j in 1:k) {
  best.fit <- regsubsets(Salary~., data = Hitters[folds!=j,],
                         nvmax = 19)
  for(i in 1:19) {
    pred <- predict(best.fit, Hitters[folds==j, ], id = i)
    cv.errors[j, i] = mean((Hitters$Salary[folds==j] - pred)^2)
  }
}
#In the above, we see that the best ten-variable model on the full data set
# has a different set of variables than the best ten-variable model on the 
#training set.
#We will use the glmnet package in order to perform ridge regression and lasso
#glmnet() used to fit ridge+. Need x matrix and y vector


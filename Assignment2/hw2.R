library(tidyverse)
library(glmnet)
library(Matrix)
library(rsample)
library(ISLR)
p <- 200
n <- 800
x <- replicate(p, rnorm(n = n, mean = 0, sd = 1))
j = 1:p
beta <- j^-1
beta_t <- beta %>% as.matrix()
epsilon <- rnorm(n = n, mean = 0, sd = 1) %>% as.matrix()

y = x %*% beta_t + epsilon

df <- data.frame("y"= y, x) 

psi = 0.05 #95% confidence interval
samples <-df %>% bootstraps(1000)

##Part b for ridge regression
#a for part b
get_beta1_lm <- function(data){
  x_mat <- model.matrix(y~.,data)[,-1]
  y <- as.matrix(data['y'])
  lm_model <- lm(y ~ x_mat)
  beta_1_estim <- coef(lm_model)[2] 
  return(beta_1_estim)
}

estim_lm <- samples$splits %>% 
  map(.,~as.data.frame(.)) %>% 
  map(.,~get_beta1_lm(.)) %>%
  simplify()
estim_lm

lm_confint <- estim_ridge %>% 
  as.data.frame() 
colnames(ridge_confint)[1] = "estimate"  
lm_confint <- lm_confint %>%
  summarise(conf.low = quantile(estimate, psi / 2),
            median = median(estimate),
            conf.high = quantile(estimate, 1 - psi / 2))                                       

lm_confint



x_mat = model.matrix(y~.,df)[,-1]
y = as.matrix(df['y'])

best_lambda_ridge <- (cv.glmnet(x_mat, y, alpha = 0))$lambda.min

best_lambda_lasso <- (cv.glmnet(x_mat, y, alpha = 1))$lambda.min

get_beta1_ridge = function(data){
  x_mat = model.matrix(y~.,data)[,-1]
  y = as.matrix(data['y'])
  ridge_model = glmnet(x_mat, y, alpha = 0, lambda = best_lambda_ridge)
  coef_ridge <- coef(ridge_model)
  beta_1_estim <- coef_ridge[2, 1]
  return(beta_1_estim) 
}


get_beta1_lasso = function(data){
  x_mat = model.matrix(y~.,data)[,-1]
  y = as.matrix(data['y'])
  lasso_model = glmnet(x_mat, y, alpha = 1, lambda = best_lambda_lasso)
  coef_lasso <- coef(lasso_model)
  beta_1_estim <- coef_lasso[2, 1]
  return(beta_1_estim) 
}


estim_ridge <- samples$splits %>% 
  map(.,~as.data.frame(.)) %>% 
  map(.,~get_beta1_ridge(.)) %>%
  simplify()
estim_ridge

ridge_confint <- estim_ridge %>% 
  as.data.frame() 
colnames(ridge_confint)[1] = "estimate"  
ridge_confint <- ridge_confint %>%
  summarise(conf.low = quantile(estimate, psi / 2),
            median = median(estimate),
            conf.high = quantile(estimate, 1 - psi / 2))                                       

ridge_confint


estim_lasso <- samples$splits %>% 
  map(.,~as.data.frame(.)) %>% 
  map(.,~get_beta1_lasso(.)) %>%
  simplify()
estim_lasso

lasso_confint <- estim_lasso %>% 
  as.data.frame() 
colnames(lasso_confint)[1] = "estimate"  
lasso_confint <- lasso_confint %>%
  summarise(conf.low = quantile(estimate, psi / 2),
            median = median(estimate),
            conf.high = quantile(estimate, 1 - psi / 2))                                       
lasso_confint

#In this exercise, we will predict the number of applications received using
#the other variables in the College data set

#a). Split the data set into a training set and a test set.
data(College)
college_data <- College %>% as.data.frame()
set.seed(1)
train <- sample(c(TRUE, FALSE), nrow(College), rep = TRUE)
test <- (!train)
college_train <- College[train, ]
college_test <- College[test, ]
#using sample

#b). Fit a linear model using least squares on the training set, and report
#the test error obtained
lm_fit <- lm(Apps ~ ., data = college_train)
lm_fit_predict <- predict(lm_fit, college_test)
lm_fit_predict
test_error = mean((lm_fit_predict - college_test[, 'Apps'])^2)
test_error
#c). Fit a ridge regression model on the training set, with lambda chosen by
#cross validation. Report the test error obtained
train_mat <- model.matrix(Apps ~ ., data = college_train)
lambda_min <- cv.glmnet(train_mat, college_train[, 'Apps'], alpha = 0)$lambda.min
ridge_fit <- glmnet(train_mat, ) 
test_mat <- model.matrix(Apps ~ ., data = college_test)
#c-d Glmnet
#cv.glmnet()
#gmlnet()

#e-f. PCR -PLS straight from the book



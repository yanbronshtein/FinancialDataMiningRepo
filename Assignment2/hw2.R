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
#college_data <- College %>% as.data.frame()
set.seed(1)
smp_siz <- dim(College)[1] / 2

train <- sample(seq_len(nrow(College)),size = smp_siz)  # Randomly identifies therows equal to sample size ( defined in previous instruction) from  all the rows of Smarket dataset and stores the row number in train_ind
test <- -train
data_train <- College[train,] #creates the training dataset with row numbers stored in train_ind
data_test <- College[test,]

#b). Fit a linear model using least squares on the training set, and report
#the test error obtained
lm_model <- lm(Apps ~ ., data = data_train)
lm_predict <- predict(lm_model, data_test)
lm_predict
test_error_least_square = mean((data_test[, 'Apps'] - lm_predict)^2)
test_error_least_square
#c). Fit a ridge regression model on the training set, with lambda chosen by
#cross validation. Report the test error obtained
x_mat_train <- model.matrix(Apps ~ ., data = data_train)[, -1]
x_mat_test <- model.matrix(Apps ~ ., data = data_test)[, -1]

lambda_min_ridge <- cv.glmnet(x_mat_train, data_train[, 'Apps'], alpha = 0)$lambda.min
lambda_min_ridge
ridge_model <- glmnet(x_mat_train, data_train[, 'Apps'], lambda = lambda_min_ridge, alpha = 0) 
ridge_predict <- predict(ridge_model, newx = x_mat_test, s = lambda_min_ridge)

test_error_ridge = mean((data_test[, 'Apps'] - ridge_predict)^2)
test_error_ridge

lambda_min_lasso <- cv.glmnet(x_mat_train, data_train[, 'Apps'], alpha = 1)$lambda.min
lambda_min_lasso
lasso_model <- glmnet(x_mat_train, data_train[, 'Apps'], lambda = lambda_min_lasso, alpha = 1) 
lasso_predict <- predict(lasso_model, newx = x_mat_test, s = lambda_min_lasso)

test_error_lasso = mean((data_test[, 'Apps'] - lasso_predict)^2)
test_error_lasso

#cv.glmnet()
#gmlnet()

#e-f. PCR -PLS straight from the book



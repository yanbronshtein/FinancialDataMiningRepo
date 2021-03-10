library(tidyverse)
library(boot)
library(glmnet)
library(Matrix)
library(rsample)
p <- 200
n <- 800
x <- replicate(p, rnorm(n = n, mean = 0, sd = 1))
j = 1:p
beta <- j^-1
beta_t <- beta %>% as.matrix()
epsilon <- rnorm(n = n, mean = 0, sd = 1) %>% as.matrix()

y = x %*% beta_t + epsilon

df <- data.frame("y"= y, x) 

boot_fn_lq <- function(data,index) {
  return(coef(lm(y~.,data=data,subset=index)))
}
boot_res_lq <- boot(data = df, statistic = boot_fn_lq, R = 1000)
boot_res_lq
boot.ci(boot.out = boot_res_lq)

##Part b for ridge regression
boot_fn_ridge <- function(data,index) {
  d <- data[index, ] 
  x_mat = model.matrix(y~.,d)[,-1]
  #x_mat = as.matrix(d[,-1])
  y = as.numeric(d["y"])
  #print(y)
  #grid = 10 ^ seq(10, -2, length = 100)
  #return(coef(glmnet(x_mat, y, alpha = 0, lambda = grid)))
  temp <- coef(glmnet(x_mat, y, alpha = 0))
  cat(typeof(temp))

  return(coef(glmnet(x_mat, y, alpha = 0)))

}
#boot_res_ridge <- boot(data = df, statistic = boot_fn_ridge, R = 5)
#boot_res_ridge

#x_mat = model.matrix(y~.,df)[,-1]
#y = as.matrix(df['y'])
#print(y)
#grid = 10 ^ seq(10, -2, length = 100)
#temp <- coef(glmnet(x_mat, y, alpha = 0))
#cat("type of temp", class(temp))
#str(temp)

get_beta1_ridge = function(data){
  x_mat = model.matrix(y~.,data)[,-1]
  y = as.matrix(data['y'])
  best_lambda <- (cv.glmnet(x_mat, y, alpha = 0))$lambda.min
  ridge_model = glmnet(x_mat, y, alpha = 0, lambda = best_lambda)
  coef_ridge <- coef(ridge_model)
  beta_1_estim <- coef_ridge[2, 1]
  return(beta_1_estim) 
}

get_beta1_lasso = function(data){
  x_mat = model.matrix(y~.,data)[,-1]
  y = as.matrix(data['y'])
  best_lambda <- (cv.glmnet(x_mat, y, alpha = 1))$lambda.min
  lasso_model = glmnet(x_mat, y, alpha = 1, lambda = best_lambda)
  coef_lasso <- coef(lasso_model)
  beta_1_estim <- coef_lasso[2, 1]
  return(beta_1_estim) 
}


psi = 0.05 #95% confidence interval
samples <-df %>% bootstraps(1000)
beta1_estim_ridge <- samples$splits %>% 
  map(.,~as.data.frame(.)) %>% 
  map(.,~get_beta1_ridge(.)) %>%
  simplify()
beta1_estim_ridge

beta1_estim_ridge_confint <- beta1_estim_ridge %>% 
  as.data.frame() 
colnames(beta1_estim_ridge_confint)[1] = "estimate"  
beta1_estim_ridge_confint <- beta1_estim_ridge_confint %>%
  summarise(conf.low = quantile(estimate, psi / 2),
            median = median(estimate),
            conf.high = quantile(estimate, 1 - psi / 2))                                       

beta1_estim_ridge_confint


beta1_estim_lasso <- samples$splits %>% 
  map(.,~as.data.frame(.)) %>% 
  map(.,~get_beta1_lasso(.)) %>%
  simplify()
beta1_estim_lasso

beta1_estim_lasso_confint <- beta1_estim_lasso %>% 
  as.data.frame() 
colnames(beta1_estim_lasso_confint)[1] = "estimate"  
beta1_estim_lasso_confint <- beta1_estim_lasso_confint %>%
  summarise(conf.low = quantile(estimate, psi / 2),
            median = median(estimate),
            conf.high = quantile(estimate, 1 - psi / 2))                                       
beta1_estim_lasso_confint




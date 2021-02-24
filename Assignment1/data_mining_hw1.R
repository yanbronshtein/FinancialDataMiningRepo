library(leaps)
library(tidyverse)
library(gridExtra)
library(extraDistr)
set.seed(2021) #Make sure results are reproducible
n <- 1000
p <- 10000
my_data <- data.frame(replicate(p, rsign(n)))
row_sums <- apply(my_data, 1, sum)
epsilon <- rnorm(1)
Y <-row_sums + epsilon
my_data <- cbind(my_data, Y)
lambdas <- c(0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10, 20, 50, 100)
X_mat <- model.matrix(Y~poly(X, 10)-1, data = reg_df) #Create a model matrix

ridge <- glmnet(X_mat, Y, alpha = 0, lambda = lambdas) #Use glmnet with updated lambda

###############



#Generate rademacher

###############################################################################
set.seed(2021) #Make sure results are reproducible
X <- rnorm(n = 100)
epsilon <- rnorm(100)

#b). Generate response vec Y of length n=100
beta <- c(1.3, -2.4, 5.6, 5.2)
Y <- beta[1] + beta[2] * X + beta[3] * X^2 + beta[4] * X^3 + epsilon

reg_df <- (X = X, Y = Y)

#Use regsubsets() to perform best subset selection to choose best model containing
#predictors X_1,...,X_10. 
regfit.full <- regsubsets(Y~poly(X,10), data=reg_df, nvmax = 10)
reg.summary <- summary(regfit.full)

plot_t <- tibble(Coefficients = 1:10, 
                 R_squared = reg.summary$rsq, 
                 Adj_R_squared = reg.summary$adjr2,
                 Cp = reg.summary$cp,
                 BIC = reg.summary$bic
                 )


p1 <- ggplot(data = plot_t, mapping = aes(x = Coefficients, y = R_squared)) +
  scale_x_continuous(breaks = seq(1,10,1)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = which.max(plot_t$R_squared), linetype="dotted", 
             color = "magenta", size=1.5) +
  labs(
    title = "R^2 v.s num of coeffs"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

p2 <- ggplot(data = plot_t, mapping = aes(x = Coefficients, y = Cp)) +
  scale_x_continuous(breaks = seq(1,10,1)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = which.min(plot_t$Cp), linetype="dotted", 
             color = "magenta", size=1.5) +
  labs(
    title = "Cp v.s num of coeffs"
  ) +
  theme(plot.title = element_text(hjust = 0.5)) 


p3 <- ggplot(data = plot_t, mapping = aes(x = Coefficients, y = BIC)) +
  scale_x_continuous(breaks = seq(1,10,1)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = which.min(plot_t$BIC), linetype="dotted", 
             color = "magenta", size=1.5) +
  labs(
    title = "BIC v.s num of coeffs"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

p4 <- ggplot(data = plot_t, mapping = aes(x = Coefficients, y = Adj_R_squared)) +
  scale_x_continuous(breaks = seq(1,10,1)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = which.max(plot_t$Adj_R_squared), linetype="dotted", 
             color = "magenta", size=1.5) +
  labs(
    title = "Adj_R_squared v.s num of coeffs"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(p1, p2, p3, p4, nrow=2)

#(d) Repeat (c), using forward stepwise selection and also using backwards
#stepwise selection. How does your answer compare to the
#results in (c)?



####Forward#####################

regfit.fwd <- regsubsets(Y~poly(X,10), data=reg_df, nvmax = 10, method = "forward")
regfit.fwd.summary <- summary(regfit.fwd)


plot_fwd_t <- tibble(Coefficients = 1:10, 
                 R_squared = regfit.fwd.summary$rsq, 
                 Adj_R_squared = regfit.fwd.summary$adjr2,
                 Cp = regfit.fwd.summary$cp,
                 BIC = regfit.fwd.summary$bic
)

p1 <- ggplot(data = plot_fwd_t, mapping = aes(x = Coefficients, y = R_squared)) +
  scale_x_continuous(breaks = seq(1,10,1)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = which.max(plot_fwd_t$R_squared), linetype="dotted", 
             color = "magenta", size=1.5) +
  labs(
    title = "Fwd Stepwise:R^2 v.s num of coeffs"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

p2 <- ggplot(data = plot_fwd_t, mapping = aes(x = Coefficients, y = Cp)) +
  scale_x_continuous(breaks = seq(1,10,1)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = which.min(plot_fwd_t$Cp), linetype="dotted", 
             color = "magenta", size=1.5) +
  labs(
    title = "Fwd Stepwise:Cp v.s num of coeffs"
  ) +
  theme(plot.title = element_text(hjust = 0.5)) 


p3 <- ggplot(data = plot_fwd_t, mapping = aes(x = Coefficients, y = BIC)) +
  scale_x_continuous(breaks = seq(1,10,1)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = which.min(plot_fwd_t$BIC), linetype="dotted", 
             color = "magenta", size=1.5) +
  labs(
    title = "Fwd Stepwise:BIC v.s num of coeffs"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

p4 <- ggplot(data = plot_fwd_t, mapping = aes(x = Coefficients, y = Adj_R_squared)) +
  scale_x_continuous(breaks = seq(1,10,1)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = which.max(plot_fwd_t$Adj_R_squared), linetype="dotted", 
             color = "magenta", size=1.5) +
  labs(
    title = "Fwd Stepwise:Adj_R_squared v.s num of coeffs"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(p1, p2, p3, p4, nrow=2)



regfit.bwd <- regsubsets(Y~poly(X,10), data=reg_df, nvmax = 10, method = "backward")
regfit.bwd.summary <- summary(regfit.bwd)

plot_bwd_t <- tibble(Coefficients = 1:10, 
                     R_squared = regfit.bwd.summary$rsq, 
                     Adj_R_squared = regfit.bwd.summary$adjr2,
                     Cp = regfit.bwd.summary$cp,
                     BIC = regfit.bwd.summary$bic
)

p1 <- ggplot(data = plot_bwd_t, mapping = aes(x = Coefficients, y = R_squared)) +
  scale_x_continuous(breaks = seq(1,10,1)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = which.max(plot_bwd_t$R_squared), linetype="dotted", 
             color = "magenta", size=1.5) +
  labs(
    title = "bwd Stepwise:R^2 v.s num of coeffs"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

p2 <- ggplot(data = plot_bwd_t, mapping = aes(x = Coefficients, y = Cp)) +
  scale_x_continuous(breaks = seq(1,10,1)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = which.min(plot_bwd_t$Cp), linetype="dotted", 
             color = "magenta", size=1.5) +
  labs(
    title = "bwd Stepwise:Cp v.s num of coeffs"
  ) +
  theme(plot.title = element_text(hjust = 0.5)) 


p3 <- ggplot(data = plot_bwd_t, mapping = aes(x = Coefficients, y = BIC)) +
  scale_x_continuous(breaks = seq(1,10,1)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = which.min(plot_bwd_t$BIC), linetype="dotted", 
             color = "magenta", size=1.5) +
  labs(
    title = "bwd Stepwise:BIC v.s num of coeffs"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

p4 <- ggplot(data = plot_bwd_t, mapping = aes(x = Coefficients, y = Adj_R_squared)) +
  scale_x_continuous(breaks = seq(1,10,1)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = which.max(plot_bwd_t$Adj_R_squared), linetype="dotted", 
             color = "magenta", size=1.5) +
  labs(
    title = "bwd Stepwise:Adj_R_squared v.s num of coeffs"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(p1, p2, p3, p4, nrow=2)

#(e) Now fit a lasso model to the simulated data, again using $X,X^2,...,X^{10}$ as predictors. 
#Use cross-validation to select the optimal
#value of $\lambda$. Create plots of the cross-validation error as a function
#of $\lambda$. Report the resulting coefficient estimates, and discuss the
#results obtained.
library(glmnet)
X_mat <- model.matrix(Y~poly(X, 10)-1, data = reg_df) #Create a model matrix
cv.out <- cv.glmnet(x = X_mat, y = Y, alpha = 1) #Perform cross validation for lambda
plot(cv.out) #Plot cross-validation as a function of lambda
best_lambda <- cv.out$lambda.min #Extract the optimal(minimal) lambda
lasso <- glmnet(X_mat, Y, alpha = 1, lambda = best_lambda) #Use glmnet with updated lambda
coef(lasso) #Print the coefficients


#f). Now regenerate a response vector $Y$ according to the model
#$$Y = \beta_0 + \beta_7X^7 + \epsilon$$ and perform best subset selection 
#and the lasso. Discuss the results obtained.


beta <- c(beta, 4.2, -3.1,4.8, -2.1)
Y = beta[1] + beta[8]*X^7 + epsilon
reg_df_2 <- tibble(X=X,Y=Y)
reg.fit2 <- regsubsets(Y ~ poly(X, 10), data = reg_df_2, nvmax = 10)
regfit2.summary <- summary(reg.fit2) 

plot2_t <- tibble(Coefficients = 1:10, 
                 R_squared = regfit2.summary$rsq, 
                 Adj_R_squared = regfit2.summary$adjr2,
                 Cp = regfit2.summary$cp,
                 BIC = regfit2.summary$bic
)


X_mat2 <- model.matrix(Y~poly(X, 10)-1, data = reg_df_2) #Create a model matrix
cv.out2 <- cv.glmnet(x = X_mat2, y = Y, alpha = 1) #Perform cross validation for lambda
#plot(cv.out2) #Plot cross-validation as a function of lambda
best_lambda2 <- cv.out$lambda.min #Extract the optimal(minimal) lambda
lasso2 <- glmnet(X_mat2, Y, alpha = 1, lambda = best_lambda2) #Use glmnet with updated lambda
coef(lasso2) #Print the coefficients





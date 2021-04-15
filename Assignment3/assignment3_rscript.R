library(ISLR)
library(tidyverse)
library(GGally)
library(class)
library(broom)
library(boot)
library(splines)
library(glm2)
library(MASS)
auto_t <- Auto %>% as_tibble()

median <- median(auto_t$mpg)
auto_t <- auto_t %>% 
  mutate(mpg01 = ifelse(mpg > median, 1, 0))

auto_t <- dplyr::select(auto_t, -mpg)
auto_t <- dplyr::select(auto_t, -name)

ggcorr(auto_t, label = TRUE, palette = "RdBu")
#top four: Weight, Horsepower, displacement, cylinders

par(mfrow=c(2,2))

boxplot(cylinders ~ mpg01, data = auto_t, main = "Cylinders vs mpg01")
boxplot(displacement ~ mpg01, data = auto_t, main = "Displacement vs mpg01")
boxplot(horsepower ~ mpg01, data = auto_t, main = "Horsepower vs mpg01")
boxplot(weight ~ mpg01, data = auto_t, main = "Weight vs mpg01")

#c). Split the data
set.seed(2021)
train <- sample(seq(nrow(auto_t)), size = 0.75 * nrow(auto_t))  
test <- -train
data_train <- auto_t[train,]
data_test <- auto_t[test,]

#(d).Perform LDA on the training data in order to predict mpg01 using the variables 
#that seemed most associated with mpg01 in (b). What is the test error of the
#model obtained?
lda_model <- 
  lda(mpg01 ~ cylinders + displacement + horsepower + weight, data = data_train)

lda_pred <- predict(lda_model, data_test)
lda_class <- lda_pred$class

test_error_lda = mean(lda_class != data_test$mpg01)
test_error_lda


#(e). Perform QDA on the training data in order to predict mpg01 using the variables
#that seemed most associated with mpg01 in #(b). 
#What is the test error of the model obtained?
qda_model <- 
  qda(mpg01 ~ cylinders + displacement + horsepower + weight, data = data_train)

qda_pred <- predict(qda_model, data_test)
qda_class <- qda_pred$class

test_error_qda = mean(qda_class != data_test$mpg01)
test_error_qda


#(f). Perform logistic regression on the training data in order to predict mpg01
#using the variables that seemed most associated with mpg01 using the variables
#that seemed most associated with mpg01 in (b). What is the test error of the
#model obtained?
glm_model <-  
  glm(mpg01 ~ cylinders + displacement + horsepower + weight, 
      data = data_train, family = binomial)
glm_pred <- round(predict(glm_model, data_test, type = "response"))

test_error_glm <- mean(glm_pred != data_test$mpg01)
test_error_glm

#(g). Perform KNN on the training data, with several values of K, in order to
# predict mpg01. Use only the variables that seemed most associated with mpg01 
#in (b). What test errors do you obtain? Which value of K seems to perform the
#best on this data set?
vars <- c("cylinders", "displacement", "horsepower", "weight")
scaled_data = scale(auto_t) #Scale the data

#Generate new train and test indexes
set.seed(1234)
new_train <- sample(1:nrow(auto_t), 392 * 0.75, rep = FALSE)
new_test <- -new_train

#Selected only the best data for 4 variables
training_data_scaled = scaled_data[new_train, vars]  
#Selected only the best data for 4 variables
testing_data_scaled = scaled_data[new_test, vars]
train_mpg01 <- auto_t$mpg01[new_train] #Save the mpg01 values used for training
test_mpg01 <- auto_t$mpg01[new_test] #Save the mpg01 values used for training

error_list <- NULL
knn_pred <- NULL
#Create a for loop
for (i in 1:nrow(testing_data_scaled)) {
  set.seed(5678)
  knn_pred <- knn(training_data_scaled, testing_data_scaled, train_mpg01, k = i)
  error_list[i] <- mean(test_mpg01 != knn_pred)
  print(error_list[i])
}

min_error <- min(error_list)
cat("The min value for knn error is: ", min_error, " for k = ", which(error_list==min_error))



#Problem 9 Pg 299 ISLR
#This question uses the variables dis(the weighted mean of distances to five 
#Boston employment centers) and nox (nitrogen oxides concentration in parts per
#10 million) from the Boston data. We will treat dis as the predictor and nox as
#the response. 

#(a). Use the poly() function to fit a cubic polynomial regression to predict
#nox using dis. Report the regression output, and plot the resulting data and
#polynomial fits.

boston_t <- Boston %>% as_tibble()
lm_boston <- lm(nox ~ poly(dis, 3), data = boston_t)
tidy_lm_boston <- tidy(lm_boston)
tidy_lm_boston

ggplot(data = boston_t, mapping = aes(x = dis, y = nox)) + 
  geom_point() + 
  stat_smooth(method = "lm", formula = y ~ poly(x, 3))
#(b). Plot the polynomial fits for a range of different polynomial degrees
#(say, from 1 to 10), and report the associated residual sum of squares.
rss_list <- rep(NA, 10)
i = 0
for (i in seq_len(10)) {
  lm_boston <- lm(nox ~ poly(dis, i), data = boston_t)
  rss_list[i] = sum(lm_boston$residuals^2)
}
rss_list
par(mfrow=c(1,1))

plot(seq_len(10), rss_list, xlab = "Degree", ylab = "RSS", type = "l")


#(c). Perform cross-validation or another approach to select the optimal degree
#for the polynomial, and explain your results.
deltas <- rep(NA, 10)
for (i in seq_len(10)) {
  fit <- glm(nox ~ poly(dis, i), data = boston_t)
  deltas[i] <- cv.glm(Boston, fit, K = 10)$delta[1]
}

plot(1:10, deltas, type="b", xlab="Degree", ylab="Test MSE")
#(d). Use the bs() function to fit a regression spline to predict nox using dis.
#Report the output for the fit using four degrees of freedom. How did you choose
#the knots? Plot the resulting fit.
#4 degrees of freedom
lm_spline <- lm(nox ~ bs(dis, df = 4), data = boston_t)
summary(lm_spline)

attr(bs(boston_t$dis, df = 4), "knots")
#Only 1 knot was chosen at the 50th percentile.
#(e). Now fit a regression spline for range of degrees of freedom, and plot the 
#resulting fits and report the resulting RSS. Describe the results obtained.
dis_lims <- range(boston_t$dis)
dis_grid <- seq(from = dis_lims[1], to = dis_lims[2], by = 0.1)
lm_spline_pred <- predict(lm_spline, list(dis = dis.grid))
plot(nox ~ dis, data = boston_t)
lines(dis_grid, lm_spline_pred, col = "blue", lwd = 2)

rss_list <- rep(NA, 10)
for (i in 3:10) {
  fit <- lm(nox ~ bs(dis, df = i), data = boston_t)
  rss_list[i] <- sum(fit$residuals^2)
}
rss_list
plot(3:10, rss_list[3:10], xlab = "Degrees of freedom", ylab = "RSS", type = "l")
#(f). Perform cross-validation or another approach in order to select the best
#degrees of freedom for a regression spline on this data. Describe your results.
deltas_2 <- rep(NA, 10)
for (i in 3:10) {
  glm_spline <- glm(nox ~ (bs(dis, df = i)), data = boston_t)
  deltas_2[i] <- cv.glm(Boston, glm_spline, K = 10)$delta[1]
}

plot(3:10, deltas_2[3:10], type="b", xlab="Degrees of Freedom", ylab="Test MSE")
# The best degree of freedom is 8


library(tidyverse)
library(boot)

p <- 200
n <- 800
x <- replicate(p, rnorm(n = n, mean = 0, sd = 1))
j = 1:p
beta <- j^-1
beta_t <- beta %>% as.matrix()
epsilon <- rnorm(n = n, mean = 0, sd = 1) %>% as.matrix()

y = x %*% beta_t + epsilon

df <- tibble("y"= y, x) 

boot_fn_lq <- function(data,index) {
  return(coef(lm(y~.,data=data,subset=index)))
}
boot_res <- boot(data = df, statistic = boot_fn_lq, R = 1000)
#summary(lm(y~.,data=df))$coef
boot_res
boot.ci(boot.out = boot_res)

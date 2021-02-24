#Principal components regression pcr can be performed using the pcr()
#function, which is part of the pls library. We now apply PCR to the Hitters
#data, in order to predict Salary, Again ensure that the missing values
#have been removed from the data

library(pls)
set.seed(2)
pcr.fit <- pcr(Salary~., data = Hitters, scale=TRUE,
               validation="CV")

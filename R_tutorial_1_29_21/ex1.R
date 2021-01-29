#Let's create a vector
x <- c(1, 2, 3, 4)
x

#Lets access value of the vector
x[0] #Wrong starts at 1 index
x[1]
x[1:3] #Access first 3

# Lets get the length of the vector
length(x)

y <- c(3, 5, 7, 9)
y

# lets add x and y
x + y


# Get sum of vector itself
sum(x)


# let's create a matrix. The second argument is for filling
# Order of arguments does not matter if you specify the parameter
my_matrix = matrix(data = c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2, byrow = TRUE)
#dimnames = list("Brody", "Thotty"))
my_matrix

# Let's access these elements
my_matrix[1,1]

#Lets access everything in the first row
my_matrix[1,]

#Let's access everything in the first col
my_matrix[,1]


#Let's get the dimensions of the matrix
length(my_matrix) #Wrong
#Correct is to use the dimensions
dim(my_matrix)

#sum of first col
sum(my_matrix[,1])


#Get sum of each row of a matrix
apply(my_matrix, 1, sum) #Apply sum function to each row of x

#Get sum of cols of matrix
apply(my_matrix, 2, sum)


my_second_matrix <- matrix(c(2, 3, 4, 5), 2, 2)

product <- my_matrix %*% my_second_matrix



#Now let's use a package
library(ISLR)
auto_dataset <- Auto
View(auto_dataset) #Use this to inspect the package
vec2 <- c(1, 2, 3, 5, 5, 5)

# Calculate mean of x
x_vec2 <- mean(vec2)
std_vec2 <- sd(vec2)


## need to use = instead of <- 
standardize = function(my_input)
{
  # Calculate the mean and standard 
  mean = mean(my_input)
  sd = sd(my_input)
  return ((my_input-mean)/sd)
  
}

output <- standardize(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
output


# Let's use the auto dataset some more
auto_dataset$acceleration

# There is upward trend.showing that increased accelartion leads to greater mpg
plot(auto_dataset$acceleration, auto_dataset$mpg) 

# Using attach
attach(auto_dataset)
acceleration
mpg
plot(acceleration, mpg)

# Nicer formattting of the plot
plot(auto_dataset$acceleration, 
     auto_dataset$mpg,
     xlab = "acceleration",
     ylab = "mpg",
     main = "acceleration vs. mpg") 
## Help
?plot #Does not work as well
help(plot, package = base) # Works better
plot(table(rpois(100, 5)), 
     type = "h", 
     col = "red", 
     lwd=20, 
     main = "rpois(100, lambda =5")


#Simulate standard normal distribution
my_dist = rnorm(10, 10, 2)
my_dist



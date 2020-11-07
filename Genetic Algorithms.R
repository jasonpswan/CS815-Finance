# Question 1

library(GA)
f <- function(x) -(x^2)/10+3*x
# Set the function up based on what is stated in the slides
lbound <- 0
ubound <- 31
# Graph is bounded between 0 & 31 according to the slides
curve(f, from = lbound, to = ubound, n = 1000)
# Produces the curve based on the function
library(GA)
GA1<- ga(type = "real-valued", fitness = f, lower = c(th=lbound), upper = ubound)
# Iterates through to find the optimum solution
summary(GA1)
# Summarises the settings of GA & produces the results of GA
plot(GA1)
# Outputs a graphical representation of the GA


# Question 2

f2 <- function(x) sum(x)
# Creates the fitness function for Question 2.
# As the objective is to have a vector of all 1s the sum option is best,

GA2<- ga(type = "binary", nBits = 25,  fitness = f2, popSize = 250, pmutation=0.01, maxiter = 100) 
# Runs the Genetic Algorithm for Question 2
summary(GA2)
# Outputs the results of t=the GA for Q2


# Question 3

item = c("Pocket Knife", "Beans", "Potatoes", "Onions", "Sleeping Bag", "Rope", "Compass")
# Creates a list of the items which can potentially be added to the Knapsack
survivalpoints = c(10, 20, 15, 2, 30, 10, 30)
# Creates a list of survival points associated with the items to be carried.
weight = c(1, 5, 10, 1, 7, 5, 1)
# Creates a list of weights associated with the items to be carried.
dataset <- data.frame(item, survivalpoints, weight)
# Creates a dataset consisting of the three lists created earlier.
weightlimit <- 20
# Sets the total weight limit for the Knapsack.

f3 <- function(x) {  
  current_solution_survivalpoints <- x %*% dataset$survivalpoints
  current_solution_weight <- x %*% dataset$weight
  if (current_solution_weight > weightlimit) 
    return(0)
  else return(current_solution_survivalpoints)
}
# Sets up the evaluation function for the Knapsack problem (GA)

GA3<- ga(type = "binary", nBits = 7, 
         fitness = f3, popSize = 250, 
         pmutation=0.01, maxiter = 100) 
# Sets up the Genetic Algorithm based on the GA model
summary(GA3)
# Generates the results of the GA for Q3

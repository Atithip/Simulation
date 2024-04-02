###### Popsicle Stand #####
# Simulate a 62-day season many times to understand the expected sales distribution.

# Define parameters
n = 62 # number of days in a season
x = 1000 # number of times to simulate a full season

# Initialize vector(s) to hold outcomes of interest from the simulation
total.sales <- rep(0,x)

set.seed(123) # Set seed to replicate the values generated in this code.

# Outer loop: Simulate a full season many times. 
for (j in 1:x) { # be sure to use a different iterator value in the outer and inner loops.
  # Simulate weather on each of the 62 days
  unif.vec <- runif(n, 0, 1)
  weather.vec <- ifelse(unif.vec <= 0.5, "Sunny",
                        ifelse(unif.vec <= 0.8, "Cloudy",
                               "Rainy"))
  # Initialize a set of vectors for traffic and sales 
  traffic.vec <- rep(0,n)
  sales.vec <- rep(0,n)
  
  # Inner loop 1: Create traffic vector within each simulated season.
  for (i in 1:n) {
    traffic.vec[i] <- max(0,round(ifelse(weather.vec[i]=="Sunny",rnorm(1,300,40),
                                   ifelse(weather.vec[i] == "Cloudy", rnorm(1,200,50),
                                          rnorm(1,50,15)))))
  }
  # Inner loop 1: Create sales vector within each simulated season.
  for (i in 1:n) {
    sales.vec[i] <- ifelse(weather.vec[i] == "Sunny", rbinom(1,traffic.vec[i],0.1),
                           ifelse(weather.vec[i] == "Cloudy", rbinom(1,traffic.vec[i],0.07),
                                  rbinom(1,traffic.vec[i],0.01)))
  }
  # Before simulating another season, record the total sales units from this run of the simulation.
  total.sales[j] <- sum(sales.vec)
}

# Average sales units per season 
mean(total.sales)
# Probability of selling more than 1000 units
sum(total.sales>=1000)/x
# Visualize the distribution of simulated sales
hist(total.sales, prob = TRUE, 
     xlab = "Units Sold", 
     ylab = "Probability", 
     main = "Distribution of Simulated Sales",
     col = "steelblue3")

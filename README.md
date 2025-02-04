# SALES SIMULATION
## Understanding Sales Distribution Over a 62-Day Season

This project simulates the sales distribution of a business over a 62-day season, using randomized weather patterns and traffic data to predict sales performance. The goal is to understand how different weather conditions (Sunny, Cloudy, Rainy) affect customer traffic and sales, by simulating the season multiple times and analyzing the results.

The simulation models customer traffic for each day based on weather patterns and then calculates sales as a function of the traffic, using binomial distributions. This is done over 62 days, with the process repeated multiple times to generate a distribution of potential total sales for each season.

## Objective
The primary objectives of this project are:

1. To simulate a realistic sales season by factoring in variable weather conditions, traffic, and customer behavior.
2. To analyze the variability in sales across multiple simulations and understand the distribution of potential sales outcomes.
3. To calculate the probability of hitting specific sales targets (e.g., selling more than 1000 units in a season).
4. To visualize the distribution of sales outcomes and interpret the results.

## Methodology
1. Simulating Weather:
For each day in the 62-day season, the weather is randomly assigned from a uniform distribution with three possible outcomes: Sunny, Cloudy, and Rainy.
The probability of each weather type is as follows:
Sunny: 50%
Cloudy: 30%
Rainy: 20%
2. Traffic Generation:
The traffic on each day is generated based on the weather. The traffic is simulated using a normal distribution:
Sunny: Mean traffic of 300, standard deviation of 40.
Cloudy: Mean traffic of 200, standard deviation of 50.
Rainy: Mean traffic of 50, standard deviation of 15.
Traffic is capped at zero (i.e., no negative traffic).
3. Sales Generation:
Sales are generated based on the traffic for the day, with different probabilities of conversion depending on the weather:
Sunny: 10% of the traffic converts into sales.
Cloudy: 7% of the traffic converts into sales.
Rainy: 1% of the traffic converts into sales.
This is modeled using a binomial distribution, where the number of sales is the result of a random sampling process based on the traffic.
4. Simulation and Analysis:
The entire simulation is run 1000 times to capture the variability of the season. For each run, the total sales for the 62 days are recorded.
After simulating 1000 seasons, the following statistics are calculated:
The average number of units sold over the 1000 simulations.
The probability of selling more than 1000 units.
A histogram of the total sales distribution.

## Results and Insights
1. Average Sales: The mean number of units sold over all simulations gives a clear picture of what a typical season might look like under the given conditions.
2. Sales Distribution: By visualizing the distribution of total sales, we can see the range of possible outcomes and assess how likely extreme sales outcomes (high or low) are.
3. Probability of Exceeding Sales Targets: The probability of selling more than 1000 units can be calculated to understand the likelihood of meeting specific sales goals.

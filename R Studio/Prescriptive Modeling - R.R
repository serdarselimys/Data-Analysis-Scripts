## Reset environment command
rm(list=ls())

library(optimizeR)
library(data.table)
library(ggplot2)
library(fitdistrplus)
library(MASS)

##Part1-1
# Defining the constants
D <- 15000    # Annual demand in units
C <- 80       # Cost per unit in dollars
h <- 0.18     # Holding cost rate (18%)
S <- 220      # Ordering cost per order in dollars

##Part1-2
# Calculating the holding cost per unit per year
H <- h * C

# Calculating the Weekly demand
d_w <- D / 52

# Calculating the optimum weekly demand
Q_w_star <- sqrt((2 * d_w * S) / H)

# Calculating the adjusted order quantity according to the company's policy
Q_w <- 2 * Q_w_star

# Annual Ordering Cost
annual_ordering_cost <- (D / Q_w) * S

# Annual Holding Cost
annual_holding_cost <- (Q_w / 2) * H * 52

# Total Inventory Cost
total_inventory_cost <- annual_ordering_cost + annual_holding_cost


# ##Part1-3 Creating a data table to find an approximate order quantity that results in the smallest total cost. 

Q_seq <- seq(1, 1000, by=1)

# Function to calculate total inventory cost
total_cost <- function(Q) {
  ordering_cost <- (D / Q) * S
  holding_cost <- (Q / 2) * H
  return(ordering_cost + holding_cost)
}

# Calculate the total inventory cost for each potential order quantity
TC <- sapply(Q_seq, total_cost)

# Create a data frame to store the results
df <- data.frame("OrderQuantity" = Q_seq, "TotalCost" = TC)

## Part1-4&6
min_order_Q <- which.min(TC)

## Part1-5 Plotting the Total Cost versus the Order Quantity
ggplot(df, mapping = aes (x=Q_seq, y=TC)) + geom_line() +   labs(title = "Total Costs vs Order Quantity", x = "Order Quantity", y = "Total Costs")

##Part1-7
## What-if Sensitivity Analysis.
# Define the ranges for the sensitivity analysis
h_values <- seq(0.1, 0.90, by=0.1)
S_values <- seq(100, 300, by=20)

# creating a matrix to store results
sensitivity_h_S <- matrix(nrow=length(h_values), ncol=length(S_values))

# calculating total inventory cost given H and S
total_cost_sensitivity <- function(h, S, D, C, Q_seq) {
  H <- h * C
  TC <- sapply(Q_seq, function(Q) {
    ordering_cost <- (D / Q) * S
    holding_cost <- (Q / 2) * H
    return(ordering_cost + holding_cost)
  })
  return(min(TC))
}

# Calculating the sensitivity analysis for holding cost rate vs ordering cost
for (i in 1:length(h_values)) {
  for (j in 1:length(S_values)) {
    sensitivity_h_S[i, j] <- total_cost_sensitivity(h_values[i], S_values[j], D, C, Q_seq)
  }
}

# Creating and Printing the sensitivity matrix
rownames(sensitivity_h_S) <- h_values
colnames(sensitivity_h_S) <- S_values

print(sensitivity_h_S)


## Part 2
## Reset environment command
rm(list=ls())

library(optimizeR)
library(data.table)
library(ggplot2)
library(fitdistrplus)
library(MASS)
library(triangle)
library(EnvStats)

# Defining constants
C <- 80       # Cost per unit in dollars
h <- 0.18     # Holding cost rate (18%)
S <- 220      # Ordering cost per order in dollars

# Setting the seed for random generated values
set.seed(42)  # Setting seed for reproducibility

# Setting the number of simulations
n_simulations <- 1000

# calculating the total cost given annual demand
calculate_total_cost <- function(D) {
  # Calculate holding cost per unit per year
  H <- h * C
  
  # calculating the order quantity and total cost
  Q_star <- sqrt((2 * D * S) / H)
  Q <- 2 * Q_star
  ordering_cost <- (D / Q) * S
  holding_cost <- (Q / 2) * H * 52  # Annual holding cost
  
  # calculating the total inventory cost
  total_cost <- ordering_cost + holding_cost
  
  return(total_cost)
}


# Defining triangular distribution parameters for annual demand
min <- 13000    # Minimum
max <- 17000    # Maximum
mode <- 15000    # Mode

# Generating samples of annual demand from triangular distribution
demand_samples <- rtriangle(n_simulations, a = min, b = max, c = mode) 

# Calculating the total cost for each sample
total_costs <- sapply(demand_samples, calculate_total_cost)

hist(total_costs)

#Part 2-1 Estimating the expected minimum total cost and constructing 95% confidence interval
min_total_cost <- min(total_costs)
mean_min_total_cost <- mean(total_costs)
std_err_min_total_cost <- sd(total_costs) / sqrt(n_simulations)
alpha <- 0.05
z_value <- qnorm(1 - alpha/2)

# Constructing confidence interval
ci_min_total_cost <- mean_min_total_cost + c(-1, 1) * z_value * std_err_min_total_cost
ci_min_total_cost

ks.test(total_costs, "pnorm", mean = mean(total_costs), sd = sd(total_costs))

# Part 2-2 Estimating the expected order quantity and constructing 95% confidence interval
order_quantities <- sapply(demand_samples, function(D) 2 * sqrt((2 * D * S) / (h * C)))

mean_order_quantity <- mean(order_quantities)
std_err_order_quantity <- sd(order_quantities) / sqrt(n_simulations)

hist(order_quantities)

ks.test(order_quantities, "pnorm", mean = mean(order_quantities), sd = sd(order_quantities))

# Construct confidence interval
ci_order_quantity <- mean_order_quantity + c(-1, 1) * z_value * std_err_order_quantity

ks.test(order_quantities, "pnorm", mean = mean(order_quantities), sd = sd(order_quantities))

# Part 2-3 Estimating the expected annual number of orders and constructing 95% confidence interval
annual_orders <- sapply(demand_samples, function(D) D / (2 * sqrt((2 * D * S) / (h * C))))

hist(annual_orders)

mean_annual_orders <- mean(annual_orders)
std_err_annual_orders <- sd(annual_orders) / sqrt(n_simulations)

# Constructing confidence interval
ci_annual_orders <- mean_annual_orders + c(-1, 1) * z_value * std_err_annual_orders

ks.test(annual_orders, "pnorm", mean = mean(annual_orders), sd = sd(annual_orders))
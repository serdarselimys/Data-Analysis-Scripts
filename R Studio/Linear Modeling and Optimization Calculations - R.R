## Reset environment command
rm(list=ls())
library(lpSolve)

## Part 1 Transportation Problem.
# Setting up the cost matrix
costs <- matrix(c(12, 15, 17, 14,9, 10, 13, 20, 11, 17, 16, 19, 7, 14, 12, 22, 16, 18), ncol=3, byrow=TRUE)

# Set up the waste generation constraints (source constraints)
plantcap <- c(45, 26, 42, 53, 29, 38)
plant.signs <- c("=", "=", "=", "=", "=", "=")

# Setting up the  disposal capacity constraint values
disposalcap <- c(65, 80, 105)
disposal.signs <- c("<=", "<=", "<=")

##Implementing the equation
lptrans <- lp.transport(costs, "min", plant.signs, plantcap, disposal.signs, disposalcap)

lptrans$crhs
## The solution matrix
lptrans$solution
## Minimized transportation cost
lptrans$objval


## Part 2 Investment Allocations.

library(quadprog)



Q <- matrix(c(0.001,	0.0003,	-0.0003,	0.00035,	-0.00035,	0.0004,0.0003,	0.009,	0.0004,	0.0016,	-0.0016,	0.0006, -0.0003,	0.0004,	0.008, 0.0015, -0.0055, -0.0007, 0.00035, 0.0016, 0.0015, 0.012, -0.0005, 0.0008, -0.00035, -0.0016, -0.0055, -0.0005, 0.012, -0.0008, 0.0004, 0.0006, -0.0007, 0.0008, -0.0008, 0.005), ncol = 6, byrow = TRUE)
Q

c <- c(0, 0, 0, 0, 0, 0)


# Step 2: Defining Constraints

Const1 <- c(1, 1, 1, 1, 1, 1) # the equality constraint should be first
Const2 <- c(0.07, 0.12, 0.11, 0.14, 0.14, 0.9)
Const3 <- c(1, 0, 0, 0, 0, 0)
Const4 <- c(0, 1, 0, 0, 0, 0)
Const5 <- c(0, 0, 1, 0, 0, 0)
Const6 <- c(0, 0, 0, 1, 0, 0)
Const7 <- c(0, 0, 0, 0, 1, 0)
Const8 <- c(0, 0, 0, 0, 0, 1)
A <- matrix(c(Const1, Const2, Const3, Const4, Const5, Const6, Const7, Const8), ncol=6, byrow=TRUE)

# Constraints' right-hand sides

b <- c(1, 0.11, 0, 0, 0, 0, 0 ,0)

# Solving the model

qp <- solve.QP(Dmat=Q, dvec=c, Amat=t(A), bvec=b, meq=1, factorized = TRUE)

qp$solution
qp$value

##part2b
baseline_returns <- seq(0.10, 0.135, by = 0.005)

results <- matrix(NA, nrow = length(baseline_returns), ncol = 2, dimnames = list(NULL, c("Risk (r)", "Return (e)")))

# Iterating over each baseline return value
for (i in seq_along(baseline_returns)) {
  # Update the constraint b to current baseline return
  b[2] <- baseline_returns[i]
  
  # Solve the quadratic programming problem
  qp <- solve.QP(Dmat = Q, dvec = c, Amat = t(A), bvec = b, meq = 1, factorized = TRUE)
  
  # Storing results
  results[i, ] <- c(qp$value, baseline_returns[i])
}

# Plotting e versus r
plot(results[, 2], results[, 1], type = "b", pch = 19, col = "blue",
     xlab = "Expected Portfolio Return (e)", ylab = "Minimized Risk (r)",
     main = "Minimized Risk (r) vs Expected Portfolio Return (e)")
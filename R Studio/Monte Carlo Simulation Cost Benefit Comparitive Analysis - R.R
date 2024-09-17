install.packages("triangulr")
library("triangulr")

set.seed(100)

K <- 10^3

# total revenue lost per year (without maintenance plan)
RL1 <- vector()

# number of breakdowns per year (without maintenance plan)
BR1 <- vector()


# total revenue lost per year (with maintenance plan)
RL2 <- vector()

# number of breakdowns per year (with maintenance plan)
BR2 <- vector()

for (j in 1:K) {
  
  # First simulation for the cost without maintenance plan:
  
  # simulating the time between breakdowns according to the semi-triangular
  # distribution between 0 and 5 weeks.
  x1 <- rtri(40, min=0, max=5, mode=5)
  
  #r1 <- runif(40)
  #x1 <-5*sqrt(r1)
  
  # calculating the cumulative times between breakdowns
  x2 <- cumsum(x1)
  
  # Stop when the cumulative time reaches one year (52 weeks).
  x3 <- x2[which(x2<=52)]
  
  # length(x3) = number of breakdowns 
  
  
  # simulating the number of days of repair:
  
  days1 <- c(1, 2, 3, 4)
  prob1 <- c(0.4, 0.35, 0.2, 0.05)
  
  #vector x4 consisting of days of repair for each breakdown.
  
  x4 <- sample (x=days1, length(x3), replace = T, prob=prob1)
  
  
  # simulating the revenue lost per day of breakdown:
  
  x5 <- runif(length(x4), min=2000, max=2600) # length(x4)= number of breakdowns 

  
  # Calculating the total revenue lost per breakdown
  x6 <- x4*x5
  
  # For each simulation, calculate the total revenue lost and place it as 
  # an element of RL1. Note that RL1 has K elements--each element is the outcome
  # of one simulation.
  
  RL1[j] <- sum(x6)
  BR1[j] <- length(x6)
  
  # Next, simulate the case with the maintenance plan simulating the time between
  # breakdowns according to the semi-triangular distribution between 0 and 8 weeks.
  
  
  y1 <- rtri(40, min=0, max=8, mode=8)
  
  # calculating the cumulative times between breakdowns
  y2 <- cumsum(y1)
  
  # stop when the cumulative time reaches one year (52 weeks)
  y3 <- y2[which(y2<=52)]
  
  # simulating the number of days of repair:
  
  days2 <- c(1, 2, 3)
  prob2 <- c(0.45, 0.35, 0.2)
  cumprob2 <- cumsum(prob2)
  
  #vector y4 consisting of days of repair for each breakdown
  
  y4 <- sample (x=days2, length(y3), replace = T, prob=prob2)
  
  
  # simulating the revenue lost per day of breakdown:
  
  y5 <- runif(length(y4), min=2000, max=2600)
  
  
  
  # Calculating the total revenue lost per breakdown
  y6 <- y4*y5
  
  # For each simulation, calculate the total revenue lost and place it as 
  # an element of RL1. Note that RL1 has K elements--each element is the outcome
  # of one simulation.
  
  RL2[j] <- sum(y6) + 25000
  BR2[j] <- length(y6)
  
}

# Calculating the averages and standard deviations of total revenue lost of 1000 simulations

# without maintenance plan:
AVG1 <- mean(RL1)
SD1 <- sd(RL1)

# with maintenance plan:
AVG2 <- mean(RL2)
SD2 <- sd(RL2)


# Calculating 95% confidence interval for each case:
noPlan.CI.Left <- AVG1-qnorm(0.975)*SD1/sqrt(K)  
noPlan.CI.Right <- AVG1+qnorm(0.975)*SD1/sqrt(K)  

withPlan.CI.Left <- AVG2-qnorm(0.975)*SD2/sqrt(K)  
withPlan.CI.Right <- AVG2+qnorm(0.975)*SD2/sqrt(K)  


# Displaying the outputs in data frames:

A1 <- c("Without Plan", "With Plan")
A2 <- c(AVG1, AVG2)
A3 <- c(noPlan.CI.Left, withPlan.CI.Left)
A4 <- c(noPlan.CI.Right, withPlan.CI.Right)

DF <- data.frame(" "=A1, "Average Revenue Lost" = A2, "95% CI Lower" = A3, "95% CI Upper" = A4)

DF  

# Performing a t-test to test whether the average annual revenue lost for the case without
# maintenance plan is greater than the case with the maintenance plan

t.test(RL1, RL2, alternative="greater")


# plotting histograms and distributions of total annual revenue lost for both cases

# For the case without maintenance plan:
hist(RL1)

# For the case with maintenance plan:
hist(RL2)

# Checking if these distributions are close to normal distribution

install.packages("EnvStats")
library("EnvStats")

gofTest(RL1, distribution = "norm", test="chisq")

help(gofTest) 

# Comparing the simulation results with expected 

#without maintenance plan:
#expected time between breakdown:
# If X is a semi-triangular RV between 0 and a, then expected value of C E(X)=(2/3)a.

E.X <- (2/3)*5


# expected number of breakdowns per year = 52/(2a/3)

E.breakdowns1 <- 52/E.X

E.daysrepair1 <- crossprod(days1, prob1)

E.revenuelostperday1 <- (2000+2600)/2 # If X is uniform RV between a and b, 
                                      #then the expected value of X is (a+b)/2

E.revenuelostperbreakdown1 <-E.daysrepair1*E.revenuelostperday1

E.annualrevenuelost1 <- E.breakdowns1*E.revenuelostperbreakdown1

# With maintenance Plan:

E.Y <- (2/3)*8


# expected number of breakdowns per year = 52/(2a/3)

E.breakdowns2 <- 52/E.Y

E.daysrepair2 <- crossprod(days2, prob2)

E.revenuelostperday2 <- (2000+2600)/2

E.revenuelostperbreakdown2 <-E.daysrepair2*E.revenuelostperday2

E.annualrevenuelost2 <- E.breakdowns2*E.revenuelostperbreakdown2


## Displaying the simulated and the expected results in a table

A1 <- c("Without Plan", "With Plan")
B1 <- c(AVG1, AVG2)
C1 <- c(E.annualrevenuelost1, E.annualrevenuelost2)

DF2 <- data.frame("Total Revenue Lost"=A1, "Simulated Average"=B1, "Expected Average"=C1)

DF2

## Analysis of the annual number of breakdowns:

hist(BR1)
AVG_BR1 <- mean(BR1)
SD_BR1 <- sd(BR1)


hist(BR2)
AVG_BR2 <- mean(BR2)
SD_BR2 <- sd(BR2)

B2 <- c(round(AVG_BR1,1), round(AVG_BR2,1))
C2 <- c(round(SD_BR1,2), round(SD_BR2,2))

DF3 <- data.frame("Breakdowns"=A1, "Simulated_Average"=B2, "Simulated_SD"=C2)

DF3

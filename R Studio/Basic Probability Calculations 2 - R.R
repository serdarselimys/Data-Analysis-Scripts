## Reset environment command
rm(list=ls())

## Loading libraries to be used in the project.
library(tidyverse)

library(dplyr)

library(janitor)

library(tidyr)

library(ggplot2)

library(stringr)

library(pacman)

library(palmerpenguins)

library(DescTools)

library(scales)

## Exercise 1 

## Generating a dataframe with all possible outcomes of 7 baseball games.
outcomes <- c("W", "L")
Goutcomes <- expand.grid(rep(list(outcomes), 7))
colnames(Goutcomes) <- paste("Game", 1:7)

Goutcomes <- clean_names(Goutcomes)

Goutcomes$Gcomb <- paste(Goutcomes$game_1, Goutcomes$game_2, Goutcomes$game_3, Goutcomes$game_4, Goutcomes$game_5, Goutcomes$game_6, Goutcomes$game_7, sep = "")

Goutcomes$Wcount <- str_count(Goutcomes$Gcomb, "W")

Goutcomes$'Game1p' <- dplyr::case_when(Goutcomes$game_1 == "W" ~ .65, TRUE ~ .35)
Goutcomes$'Game2p' <- dplyr::case_when(Goutcomes$game_2 == "W" ~ .65, TRUE ~ .35)
Goutcomes$'Game3p' <- dplyr::case_when(Goutcomes$game_3 == "W" ~ .65, TRUE ~ .35)
Goutcomes$'Game4p' <- dplyr::case_when(Goutcomes$game_4 == "W" ~ .65, TRUE ~ .35)
Goutcomes$'Game5p' <- dplyr::case_when(Goutcomes$game_5 == "W" ~ .65, TRUE ~ .35)
Goutcomes$'Game6p' <- dplyr::case_when(Goutcomes$game_6 == "W" ~ .65, TRUE ~ .35)
Goutcomes$'Game7p' <- dplyr::case_when(Goutcomes$game_7 == "W" ~ .65, TRUE ~ .35)

Goutcomes$AllGamesProb <- (Goutcomes$Game1p * Goutcomes$Game2p * Goutcomes$Game3p * Goutcomes$Game4p * Goutcomes$Game5p * Goutcomes$Game6p * Goutcomes$Game7p)

## Summing up the probabilities of combinations where red sox won exactly 5 games.
prob1_result <- sum(Goutcomes[Goutcomes$Wcount == 5, 17])

## Exercise 2 creating a dataframe with each possible outcome and probabilities of these outcomes
prob2_result <- Goutcomes %>% 
  group_by(Wcount) %>% 
  summarise(probability = sum(AllGamesProb))

colnames(prob2_result) <- c("wins", "probability")

## Exercise 3 calculating the probability of red sox winning less than 5 games.
prob3_result <- sum(Goutcomes[Goutcomes$Wcount < 5, 17])

## Exercise 4 calculating the probability of red sox winning between 3 and 5 games inclusively.
prob4_result <- sum(Goutcomes[Goutcomes$Wcount >= 3 & Goutcomes$Wcount <= 5 , 17])

## Exercise 5 calculating the probability of red sox wining more than 4 games.
prob5_result <- sum(Goutcomes[Goutcomes$Wcount > 4, 17])

## Exercise 6 calculating the expected value of the number of wins for the red Sox in a 7 game series.
prob2_result$Ex_val <- prob2_result$wins*prob2_result$probability

prob6_result <- sum(prob2_result$Ex_val)

## Exercise 7 Calculating the theoretical variance of the number of wins for the Red Sox in a 7- game series. 

prob7_result <- var(prob2_result$wins)

prob7_result_b <- var(prob2_result$wins*prob2_result$probability)

## Exercise 8 and 9 Generating a vector with 1000 random values for the number of wins by the red Sox in a 7 game series and calculating its mean
set.seed(10)
prob9_result <- mean(vector1 <- sample(0:7, size=1000, replace=TRUE, prob=prob2_result$probability))

## Exercise 10 calculating the sample variance of the 1000 random values generated for the number of wins by the red sox in a 7 game series.
prob10_result <- var(vector1)

## Exercise 11 calculating the the probability that an employee will receive exactly 6 calls in the next hour.
prob11_result <- dpois(6, lambda=7) 

## Exercise 12 calculating the probability that an employee will receive 40 or fewer calls in the next 8 hours.
prob12_result <- ppois(q=40, lambda = 56, lower.tail = T)

## Exercise 13 calculating the probability that 5 employees working 8 hour shifts will meet the quota of 275 or more call during their shift.
Call_Counts_13 <- c(270:280)
P_Call_Counts_13 <- ppois(q=270:280, lambda = 280, lower.tail = F)
P_Call_Counts_13 <- label_percent(accuracy=0.01)(P_Call_Counts_13)

df_13 <- data.frame(Call_Counts_13,P_Call_Counts_13)
  
prob13_result <- ppois(q=275, lambda = 280, lower.tail = F)

##Exercise 14 calculating the probability when 1 employee is sick and for remaining number of employees to meet the quota.
Call_Counts_14 <- c(270:280)
P_Call_Counts_14 <- ppois(q=270:280, lambda = 224, lower.tail = F)
P_Call_Counts_14 <- label_percent(accuracy=0.01)(P_Call_Counts_14)

df_14 <- data.frame(Call_Counts_14,P_Call_Counts_14)

prob14_result <- ppois(q=275, lambda = 224, lower.tail = F)

##Exercise 15 calculating the number of calls needed for one employee working 8 shift to be considered in the top 10% of days with call colume wise.

## Exercise 15 step 1 creating a dataframe with number of calls from 0 to 100 with their corresponding probabilities.
CallCount <- c(0:100)
P_CallCounts <- (ppois(q=0:100, lambda = 56, lower.tail = T))
P_CallCounts <- label_percent(accuracy=0.01)(P_CallCounts)

##Exercise 15 step 2 identifying the minimum number of call that has a probability of over 90 percent.
df_x <- data.frame(CallCount,P_CallCounts)
mincallcount <- min(df_x$CallCount[df_x$P_CallCounts >= 90])

##Exercise 15 step 3 checking if the answer calculated above satisfies the conditions of the exercise
prob15_result_test <- ppois(q=mincallcount, lambda = 56, lower.tail = T)
prob15_result_test2 <- ppois(q=mincallcount-1, lambda = 56, lower.tail = T)

prob15_result <- mincallcount

## Exercise 16 generating 1,000 random values for the number of calls for a single employee during an 8-hour shift. 
set.seed(15)
calls_per_hour <- 7
calls_8_hours <- rpois(1000, lambda = calls_per_hour * 8)

## Exercise 17 calculating the sample mean of the 1,000 random values generating in the previous exercise.

prob17_result <- mean(calls_8_hours)

## Exercise 18 calculating the sample variabce of the random values generated in exercise 16.
prob18_result <- var(calls_8_hours)

##Exercise 19 calculating the the percentage of light bulbs with a lifespan of between 1,800 and 2,200 hours.
prob19_result <- pnorm(2200, mean=2000, sd=100) - pnorm(1800, mean=2000, sd=100)

## Exercise 20 calculating the percentage of light bulbs with a life span of more than 2,500 hours.

prob20_result <- 1 - pnorm(2500, mean=2000, sd=100)

## Exercise 21 calculating the maximum number of hours in a light bulb's life span for it to fall into bottom 10% / defective category.
prob21_result <- round(qnorm(0.10, mean=2000, sd=100),digits = 0)

## Exercise 22 generating 10,000 random values for the life spans of manufactured light bulbs. 
set.seed(25)
vector3 <- rnorm(10000, mean=2000, sd=100)

## Exercise 23  calculating the population mean for the random values generated in the previous exercise.
prob23_result <- round(mean(vector3), digits = 2)

## Exercise 24  calculating the population standard deviation for the random values generated in exercise 22.
prob24_result <- sd(vector3)

## Exercise 25 generating 1,000 different samples from the random values, where each sample contains 100 values and computing the mean for each of the 100 values.
set.seed(1)
N = 1000
results = replicate(1000, sample(x=vector3, size=100, replace=TRUE))

prob25_result <- colMeans(results)

## Exercise 26 creating a histogram for the results of the previous exercise.
prob26_result <- hist(prob25_result)

## Exercise 27 computing the mean of the values from exercise 26.
prob27_result <-mean(prob25_result)


## Exercise 28 Exploring the distribution of flipper length of the Adélie penguin and identifying its distribution type with statistical evidence.
data(package = 'palmerpenguins')

## Loading the dataframe into a dataframe and performing data prep processes for the analysis.
df <- data.frame(penguins)

adeliedf <- subset(penguins, species == "Adelie")

adelie_vec <- adeliedf$flipper_length_mm

adelie_vec <- na.omit(adelie_vec)

## Extracting statistical figures for the variabele for the analysis.

count_adelie_vec <- length(adelie_vec)
  
mean_adelie_vec <- round(mean(adelie_vec), digits= 0)

mode_adelie_vec <- round(Mode(adelie_vec), digits = 0)

median_adelie_vec <- round(median(adelie_vec), digits=0)

sd_adelie_vec <- round(sd(adelie_vec), digits=0)

min_adelie_vec <- min(adelie_vec)

max_adelie_vec <- max(adelie_vec)

## Creating a histogram for visually inspecting the distribution.

plhist_1 <- hist(adelie_vec, main="Histogram of Adelie Penguins' Flipper length", xlab="Flipper Length in mm", ylab="Frequency")

##Creating a second and third histogram for visually inspecting the distribution with different bin sizes.

plhist_2 <- hist(adelie_vec, breaks = seq(from=160, to=220, by=7), main="Histogram of Adelie Penguins' Flipper length", xlab="Flipper Length in mm", ylab="Frequency")

plhist_3 <- hist(adelie_vec, breaks = seq(from=160, to=220, by=3.5), main="Histogram of Adelie Penguins' Flipper length", xlab="Flipper Length in mm", ylab="Frequency")

## Calculating the percantege of values at different distances from the mean in distribution.

plarg_1 <- round(pnorm(210, mean=190, sd=7) - pnorm(190, mean=190, sd=7), digits = 3)

plarg_2 <- round(pnorm(190, mean=190, sd=7) - pnorm(172, mean=190, sd=7), digits= 3)

plarg_3 <- round(pnorm(210, mean=190, sd=7) - pnorm(200, mean=190, sd=7), digits = 3)

plarg_4 <- round(pnorm(180, mean=190, sd=7) - pnorm(172, mean=190, sd=7), digits = 3)


## Exercise 29 Exploring the relationship between the flipper length and beak depth of the gentoo penguins with evidence and justification for the identified relationship.

## data prep.

gentoodf <- subset(penguins, species == "Gentoo")
gentoodf <- gentoodf[c('bill_depth_mm', 'flipper_length_mm')]
gentoodf <- na.omit(gentoodf)

## Visualization 1 plain scatter chart

gnt_plot1 <- plot(gentoodf$flipper_length_mm, gentoodf$bill_depth_mm, main="Scatter Plot of Gentoo Penguins Bill Depth vs Flipper Lenght", xlab="Flipper Length in mm", ylab="Bill Depth in mm")

## Visualization 2 scatter chart with regression line.

gnt_plot2 <- plot(gentoodf$flipper_length_mm, gentoodf$bill_depth_mm, main="Scatter Plot of Gentoo Penguins Bill Depth vs Flipper Lenght", xlab="Flipper Length in mm", ylab="Bill Depth in mm")
abline(lm(gentoodf$bill_depth_mm ~ gentoodf$flipper_length_mm, data = gentoodf), col = "red")

## Correlation Calculation.
gentoo_Corr_Coef <- cor(gentoodf$bill_depth_mm, gentoodf$flipper_length_mm)

p_load(testthat)
testthat::test_file("project6_tests.R")

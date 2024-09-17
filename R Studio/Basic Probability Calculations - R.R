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

## Exercise 1 Loading the dataset into Rstudio
df <- read.csv("ball-dataset.csv")

## Exercise 2 creating a dataframe for counts of colors.
freq_color <- df %>% 
  group_by(color) %>% 
  summarise(counts = n())

## Exercise 3 creating a dataframe for counts of labels.
freq_label <- df %>% 
  group_by(label) %>% 
  summarise(counts = n())

## Exercise 4 Bar chart for count of colors with different colors.

plot1 <- ggplot(freq_color, aes(x = color, y = counts))
plot1 + geom_col(fill = c("blue", "green", "red", "yellow")) + geom_text(aes(label = counts), vjust = -.5) + ggtitle("Color Counts of Balls") + xlab("Colors") + ylab("Count")

## Exercise 5 Bar chart for count of labels with different colors.
plot2 <- ggplot(freq_label, aes(x = label, y = counts))
plot2 + geom_col(fill = c("blue", "orange", "black", "magenta","turquoise")) + geom_text(aes(label = counts), vjust = -.5) + ggtitle("Label Counts of Balls") + xlab("Labels") + ylab("Count")

## Exercise 6 calculating the probability of drawing a green ball.

cgb <- sum(df$color == "green")
ctb <- sum(freq_color$counts)
  
prob6_result <- cgb/ctb

## Exercise 7 calculating the probability of drawing a blue or a red ball.

cbb <- sum(df$color == "blue")
crb <- sum(df$color == "red")
ctb <- sum(freq_color$counts)

prob7_result <- (cbb + crb)/ctb

## Exercise 8 calculating the probability of drawing a ball with a label of A or C.

cab <- sum(df$label == "A")
ccb <- sum(df$label == "C")

clb <- sum(freq_label$counts)

prob8_result <- (cab + ccb)/clb

## Exercise 9 calculating the probability of drawing a yellow ball with a D.

cydb <- sum(df$label == "D" & df$color == "yellow") 
ctb <- sum(freq_color$counts)

prob9_result <- cydb/ctb

## Exercise 10 calculating the probability of drawing a yellow ball or a ball with a D.

cyodb <- sum(df$label == "D" | df$color == "yellow") 
ctb <- sum(freq_color$counts)

prob10_result <- cyodb/ctb

## Exercise 11 calculating the probability of drawing a blue ball followed by a red ball without replacement.

crb <- sum(df$color == "red")
cbb <- sum(df$color == "blue")

ctb2 <- ctb-1

prob11_result <- (cbb/ctb)*(crb/ctb2)

prob11_result <- round(prob11_result, digits = 7)

## Exercise 12 calculating the probability of drawing four green balls in a row without replacement.

cgb1 <- sum(df$color == "green")

cgb2 <- cgb1 - 1

cgb3 <- cgb2 - 1

cgb4 <- cgb3 - 1

ctb2

ctb3 <- ctb2 -1

ctb4 <- ctb3 -1

eprob1 <- cgb1 / ctb

eprob2 <- cgb2 / ctb2

eprob3 <- cgb3 / ctb3

eprob4 <- cgb4 / ctb4

prob12_result <- (eprob1*eprob2*eprob3*eprob4)

prob12_result <- round(prob12_result, digits = 9)

## Exercise 13 calculating the probability of drawing a red ball followed by a ball with a B without replacement.

## Exercise 13 a. calculating the probabilities for drawing a red ball.

crb <- sum(df$color == "red")

e1p <- crb / ctb

## Exercise 13 b. calculating the probability of drawing a ball with a b after taken out a red ball from the original sample set.
second_sample_set <- (sum(df$color == "red")-1) + sum(df$color == "green") + sum(df$color == "blue") + sum(df$color == "yellow")

cBb <- sum(df$label == "B")

e2p <- cBb / second_sample_set

## Exercise 13 c. calculating the probability of 1st event followed by the second event.
prob13_result <- e1p * e2p

prob13_result <- round(prob13_result, digits = 7)

## Exercise 14 creating a formula to calculate factorial of a given number. (Challenge)

## Exercise 14 a. using the function included in R basic syntax
my_factorial <- function(x){
  factorial(x)
}

## Exercise 14 b. creating a function to calculate factorial with creation of a vector and multiplying all values included in it.
my_factorial2 <- function(x){
  prod(seq.int(x, 1))
}

## Exercise 15 creating a dataframe that contains all possible outcomes of flipping the coin four times.

coin_outcomes <- data.frame(first=c('H','H','H','H','H','H','H','H','T','T','T','T','T','T','T','T'),
                             second=c('H','H','H','H','T','T','T','T','H','H','H','H','T','T','T','T'),
                             third=c('H','H','T','T','H','H','T','T','H','H','T','T','H','H','T','T'),
                             fourth=c('H','T','H','T','H','T','H','T','H','T','H','T','H','T','H','T'))



## Exercise 16 adding a column to the dataframe with the probability of each 4 coin flips occurrences. 

## Adding probabilities of unfair coun flips into the dataframe.
coin_outcomes$'1prob' <- dplyr::case_when(coin_outcomes$first == "H" ~ .6, TRUE ~ .4)
coin_outcomes$'2prob' <- dplyr::case_when(coin_outcomes$second == "H" ~ .6, TRUE ~ .4)
coin_outcomes$'3prob' <- dplyr::case_when(coin_outcomes$third== "H" ~ .6, TRUE ~ .4)
coin_outcomes$'4prob' <- dplyr::case_when(coin_outcomes$fourth== "H" ~ .6, TRUE ~ .4)

## Calculating the probabilities for the rows after 4 coin flips.
coin_outcomes$coin_probs <- c(coin_outcomes$'1prob'*coin_outcomes$'2prob'*coin_outcomes$'3prob'*coin_outcomes$'4prob')

## Adding the coin flip outcomes into a single string variables.
coin_outcomes$comb <- paste(coin_outcomes$first, coin_outcomes$second, coin_outcomes$third, coin_outcomes$fourth, sep = "")

## Counting outcome of coin flips in each string variable for later calculations.
coin_outcomes$Hcount <- str_count(coin_outcomes$comb, "H")
coin_outcomes$Tcount <- str_count(coin_outcomes$comb, "T")

## Calculating the four in a row coin flip outcomes using the counts of outcomes in each quartet.
coin_outcomes$Pcoin_probs <- (.6 ** coin_outcomes$Hcount)*(.4 ** coin_outcomes$Tcount)

## Exercise 17 calculating the probability of each of the 5 possible outcomes.
num_heads_prob <- coin_outcomes %>% 
  group_by(Hcount) %>% 
  summarise(probability = sum(coin_probs))

## Exercise 18 Method 1. manually calculating the probability of each combination of outcomes with 3 heads and summing up the results

HHHT  = .6*.6*.6*.4

THHH = .4*.6*.6*.6

HHTH = .6*.6*.4*.6

HTHH = .6*.4*.6*.6

prob18_result = HHHT + THHH + HHTH + HTHH

## Exercise 18 Method 2. summing up the probabilities of all rows with 3 Heads in 4 four flips.

prob18_result2 <- sum(coin_outcomes[coin_outcomes$Hcount == 3, 13])

## Exercise 19 Method 1. manually calculating the probability of each combination of outcomes with 2 and 4 heads and summing up the results

HHTT = .6*.6*.4*.4

HTTH = .6*.4*.4*.6

THHT = .4*.6*.6*.4

THTH = .4*.6*.4*.6

TTHH = .4*.4*.6*.6

HTHT = .6*.4*.6*.4

FH = .6*.6*.6*.6

prob19_result = HHTT + HTTH + THTH + TTHH + HTHT + THHT + FH

## Exercise 19 Method 2. summing up the probabilities of all rows with 2 Heads or 4 heads in 4 four flips.

prob19_result2 <- sum(coin_outcomes[coin_outcomes$Hcount == 2 | coin_outcomes$Hcount == 4, 13])

## Exercise 20 calculating the probability of an outcome of less than or equal to three heads.

## Exercise 20 method 1 manually calculating the probability of each combination of outcomes.

## Exercise 20 method 1a manually calculating the probability of each combination of outcomes with 3 heads.

HHHT = .6*.6*.6*.4

HTHH = .6*.4*.6*.6

HHTH = .6*.6*.4*.6

THHH = .4*.6*.6*.6

ThreeH = HHHT + HTHH + HHTH + THHH

## Exercise 20 method 1b manually calculating the probability of each combination of outcomes with 2 heads.

HHTT = .6*.6*.4*.4

HTTH = .6*.4*.4*.6

THHT = .4*.6*.6*.4

THTH = .4*.6*.4*.6

TTHH = .4*.4*.6*.6

HTHT = .6*.4*.6*.4

twoH = HHTT + HTTH + THHT + THTH + TTHH + HTHT

## Exercise 20 method 1c manually calculating the probability of each combination of outcomes with 1 heads.

TTTH = .4*.4*.4*.6

HTTT = .6*.4*.4*.4

THTT = .4*.6*.4*.4

TTHT = .4*.4*.6*.4

oneH = TTTH + HTTT + THTT + TTHT

## Exercise 20 method 1d manually calculating the probability of a combination of outcomes with 0 heads.

noH = .4*.4*.4*.4

## Exercise 20  Method 1e manually summing up the probabilities of all rows with 3 Heads or less.
prob20_result = ThreeH + twoH + oneH + noH

## Exercise 20 Method 2.summing up the probabilities of all rows with 3 Heads or less.

prob20_result2 <- sum(coin_outcomes[coin_outcomes$Hcount <= 3, 13])

## Exercise 21 Creating a Bar chart for probabilities of the unfair coin.

plot3 <- ggplot(num_heads_prob, aes(x = Hcount, y = probability))
plot3 + geom_col(fill = "turquoise") + geom_text(aes(label = probability), vjust = -.4) + ggtitle("Probability Distribution of Heads 4 Flips") + xlab("Number of Heads") + ylab("Probability")

## Soccer Games Challenge Exercise

## Generating a dataframe with all possible outcomes of 10 soccer games.
outcomes <- c("W", "L")
Goutcomes <- expand.grid(rep(list(outcomes), 10))
colnames(Goutcomes) <- paste("Game", 1:10)

Goutcomes <- clean_names(Goutcomes)

## Inserting the Outcome probabilities of home games into the dataframe.
Goutcomes$'Game1op' <- dplyr::case_when(Goutcomes$game_1 == "W" ~ .75, TRUE ~ .25)
Goutcomes$'Game3op' <- dplyr::case_when(Goutcomes$'game_3' == "W" ~ .75, TRUE ~ .25)
Goutcomes$'Game5op' <- dplyr::case_when(Goutcomes$'game_5' == "W" ~ .75, TRUE ~ .25)
Goutcomes$'Game7op' <- dplyr::case_when(Goutcomes$'game_7' == "W" ~ .75, TRUE ~ .25)
Goutcomes$'Game9op' <- dplyr::case_when(Goutcomes$'game_9' == "W" ~ .75, TRUE ~ .25)


## Inserting the Outcome probabilities of away games into the dataframe.
Goutcomes$'Game2op' <- dplyr::case_when(Goutcomes$'game_2' == "W" ~ .5, TRUE ~ .5)
Goutcomes$'Game4op' <- dplyr::case_when(Goutcomes$'game_4' == "W" ~ .5, TRUE ~ .5)
Goutcomes$'Game6op' <- dplyr::case_when(Goutcomes$'game_6' == "W" ~ .5, TRUE ~ .5)
Goutcomes$'Game8op' <- dplyr::case_when(Goutcomes$'game_8' == "W" ~ .5, TRUE ~ .5)
Goutcomes$'Game10op' <- dplyr::case_when(Goutcomes$'game_10' == "W" ~ .5, TRUE ~ .5)

## Calculating the outcome of all 10 game combinations.
Goutcomes$LeaugeProb <- (Goutcomes$'Game1op' * Goutcomes$'Game2op' * Goutcomes$'Game3op' * Goutcomes$'Game4op' * Goutcomes$'Game5op' * Goutcomes$'Game6op' * Goutcomes$'Game7op' * Goutcomes$'Game8op' * Goutcomes$'Game9op' * Goutcomes$'Game10op')

## Combining the outcome of Home games in one variable.
Goutcomes$Hcomb <- paste(Goutcomes$game_1, Goutcomes$game_3, Goutcomes$game_5, Goutcomes$game_7, Goutcomes$game_9, sep = "")

## Combining the outcome of away games in one variable.
Goutcomes$Acomb <- paste(Goutcomes$game_2, Goutcomes$game_4, Goutcomes$game_6, Goutcomes$game_8, Goutcomes$game_10, sep = "")

## Counting the number of wins in home games from all outcomes
Goutcomes$HWcount <- str_count(Goutcomes$Hcomb, "W")
## Counting the number of wins in away games from all outcomes
Goutcomes$AWcount <- str_count(Goutcomes$Acomb, "W")
## Counting the number of loses in home games from all outcomes
Goutcomes$HLcount <- str_count(Goutcomes$Hcomb, "L")
## Counting the number of loses in away games from all outcomes
Goutcomes$ALcount <- str_count(Goutcomes$Acomb, "L")

## Calculating the probabilities of all home game outcomes.
Goutcomes$Houtcome_probs <- (.75 ** Goutcomes$HWcount)*(.25 ** Goutcomes$HLcount)
## Calculating the probabilities of all away game outcomes
Goutcomes$Aoutcome_probs <- (.5 ** Goutcomes$AWcount)*(.5 ** Goutcomes$ALcount)

## Calculating the probabilities of all game outcomes.
Goutcomes$Alloutcome_probs <- (Goutcomes$Houtcome_probs)*(Goutcomes$Aoutcome_probs)

## Solutions for the exercise 22. and 23.
## Combining out possible outcomes in one string variable for all games.
Goutcomes$AOcomb <- paste(Goutcomes$Hcomb, Goutcomes$Acomb, sep = "")

## Counting the number of wins in all outcome combinations.
Goutcomes$AOCWcount <- str_count(Goutcomes$AOcomb, "W")
## Counting the number of loses in all outcome combinations.
Goutcomes$AOCLcount <- str_count(Goutcomes$AOcomb, "L")

## Exercise 22 calculating the probability of wining exactly 10 games.
prob22_result <- sum(Goutcomes[Goutcomes$AOCWcount == 10, 21])

## Exercise 23 calculating the probability of wining more than 1 game.

prob23_result <- sum(Goutcomes[Goutcomes$AOCWcount > 1, 21])

## Exercise 23 method 2 subtracting the probability of wining less than or equal to 1 game from the probability of all possible outcomes.

prob23_result2 <- sum(Goutcomes$LeaugeProb) - sum(Goutcomes[Goutcomes$AOCWcount <= 1, 21])

## Exercise 24 calculating the number of different ways we could pick five games at random and have three home games and two away games.

## Exercise 24a creating a dataframe with all possible types of a total of 5 games.
outcomes2 <- c("H", "A")
HvsA <- expand.grid(rep(list(outcomes2), 5))
colnames(HvsA) <- paste("GameType", 1:5)

HvsA <- clean_names(HvsA)

## Exercise 24b combining all possible types of games in one string variable.

HvsA$comb <- paste(HvsA$game_type_1, HvsA$game_type_2, HvsA$game_type_3, HvsA$game_type_4, HvsA$game_type_5,HvsA$game_type_6, HvsA$game_type_7, HvsA$game_type_8, HvsA$game_type_9, HvsA$game_type_10, sep = "")

## Exercise 24b counting the types of games in all possible combinations.
HvsA$comb_h_count <- str_count(HvsA$comb, "H")
HvsA$comb_a_count <- str_count(HvsA$comb, "A")

## Exercise 24c marking the combinations of game types with 3 home games and 2 away games. 
HvsA$filtered <- dplyr::case_when(HvsA$comb_h_count == 3 & HvsA$comb_a_count == 2 ~ 1, TRUE ~ 0)

## Counting the number of marked combination by sum function.
prob24_result <- sum(HvsA$filtered)


## Testing the results

p_load(testthat)
testthat::test_file("project5_tests.R")

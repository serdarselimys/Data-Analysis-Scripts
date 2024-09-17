## Reset environment command
rm(list=ls())

## Serdar Selim Yesildag - 28/April/24 - ALY6015, Week 3, Assignment 3.

## Loading libraries to be used in the project.
library(tidyverse)

library(dplyr)

library(tidyr)

library(ggplot2)

library(scales)

library(lubridate)

library(car)

library(psych)

library(DescTools)

library(leaps)

library(ISLR)

## Loading the dataset to R studio
df <- College

## Extracting summary statistics for the College Data set.
summary(df)

## Splitting dataset into training and testing sections.
trainIndex <- sort(sample(x = nrow(df), size = nrow(df) * 0.7))
sample_train <- df[trainIndex,]
sample_test <- df[-trainIndex,]

## Converting Boolean Yes, No variable to 1 and 0 for regression model. 
sample_train <- sample_train %>%
  mutate(Private = ifelse(Private == "No",0,1))

sample_test <- sample_test %>%
  mutate(Private = ifelse(Private == "No",0,1))

# Choosing a Best subset with reg subsets.

best_subset = regsubsets(Private ~ ., data = sample_train, nbest=3, method="exhaustive")
reg.summary <- summary(best_subset)
reg.summary
names(reg.summary)

with(summary(best_subset), data.frame(rsq, adjr2, cp, rss, bic, outmat))

summary(best_subset)$adjr2
summary(best_subset)$bic

## Creating the first logistic regression model from train dataset.

model1 <- glm(Private ~ F.Undergrad + Outstate + PhD, family = binomial, data = sample_train)
model1

summary(model1)

##further variable selection methodologies

## Variable Selection Backward, Forward and both directions.

Fitbase <- glm(Private ~ 1, family = binomial, data = sample_train)
summary(Fitbase)

Fitall <- glm(Private ~ ., family = binomial, data = sample_train)
summary(Fitall)

## Backward
step(Fitall, direction = "backward")

## forward
step(Fitbase, direction = "forward", scope = formula(Fitall))

## both directions
step(Fitbase, direction = "both", scope = formula(Fitall))

## Exercise 4.
predict.glm(model1, sample_test, type="response")

actual_labels <- sample_train$Private

# Convert predicted probabilities to predicted class labels
predicted_labels <- ifelse(predict(model1, sample_train, type="response") > 0.5, 1, 0) 

# Creating a confusion matrix for the train dataset.
confusion_matrix <- table(actual_labels, predicted_labels)
print(confusion_matrix)

## Model Marks
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy

precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
precision

recall <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
recall

specificity <- confusion_matrix[1, 1] / sum(confusion_matrix[1, ])
specificity

## Exercise 6

# Creating a confusion matrix for the train dataset.
test_actual_labels <- sample_test$Private

test_predicted_labels <- ifelse(predict(model1, sample_test, type="response") > 0.5, 1, 0) 

confusion_matrix2 <- table(test_actual_labels, test_predicted_labels)
print(confusion_matrix2)

## confusion_matrix 2 metrics.
accuracy2 <- sum(diag(confusion_matrix2)) / sum(confusion_matrix2)
accuracy2

precision2 <- confusion_matrix2[2, 2] / sum(confusion_matrix2[, 2])
precision2

recall2 <- confusion_matrix2[2, 2] / sum(confusion_matrix2[2, ])
recall2

specificity2 <- confusion_matrix2[1, 1] / sum(confusion_matrix2[1, ])
specificity2

## ROC Plot and AUC Calculation

library(pROC)
predicted2 <-predict(model1, sample_test, type="response")
auc(sample_test$Private, predicted2)
rocX2=roc(sample_test$Private, predicted2)
plot(rocX2, col="blue", lwd=3, main="ROC curve")
plot(smooth(rocX2), add=TRUE, col="black")
plot(rocX2,col="blue", print.auc=TRUE, auc.polygon=TRUE,
     grid.col=c("green", "red"), 
     auc.polygon.col="lightblue")



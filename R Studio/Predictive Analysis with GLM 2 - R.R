## Reset environment command
rm(list=ls())

## Serdar Selim Yesildag - 05/May/24 - ALY6015, Week 4, Assignment 4.

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

library(Metrics)

library(glmnet)

set.seed(42)

## Loading the dataset to R studio
df <- College

## Extracting summary statistics for the College Data set.
summary(df)

## Converting categorical variable private to Boolean 
df <- df %>%
  mutate(Private = ifelse(Private == "No",0,1))

## Splitting dataset into training and testing sections.
trainIndex <- sort(sample(x = nrow(df), size = nrow(df) * 0.7))
sample_train <- df[trainIndex,]
sample_test <- df[-trainIndex,]

## Converting variable values into matrices for training dataset
y <- sample_train %>% select(Grad.Rate) %>% scale(center = TRUE, scale = FALSE) %>% as.matrix()
X <- sample_train %>% select(-Grad.Rate) %>% as.matrix()

## Converting variable values into matrices for test dataset
yt <- sample_test %>% select(Grad.Rate) %>% scale(center = TRUE, scale = FALSE) %>% as.matrix()
Xt <- sample_test %>% select(-Grad.Rate) %>% as.matrix()

## Performing 10-fold cross-validation to select lambda  
lambdas_ridge_try <- 10^seq(-3, 5, length.out = 100)

## Setting alpha = 0 to implement ridge regression
ridge_cv <- cv.glmnet(X, y, alpha = 0, lambda = lambdas_ridge_try,
                      standardize = TRUE, nfolds = 10)

Ridgeplot <- plot(ridge_cv)

## Extracting Best cross-validated lambda
loglambda_ridge_opt <- round(log(ridge_cv$lambda.min),3)
loglambda_ridge_opt

lambda_ridge_min_opt <- round(ridge_cv$lambda.min,3)
lambda_ridge_min_opt

lambda_ridge_1se_opt <- round(ridge_cv$lambda.1se, 3)
lambda_ridge_1se_opt

## Ridge model and fit statistics

## Fit Final Model  and Display regression coefficients (Train Dataset)
model_ridge_cv <- glmnet(X, y, alpha = 0, lambda = lambda_ridge_min_opt, standardize = TRUE)
ridge_coef=round(coef(model_ridge_cv),3)
ridge_coef

## Predicting values based on ridge model.
y_hat_ridge_cv <- predict(model_ridge_cv, X)

## Defining R-Squared function.

Rsquare <- function(preds,actual){ 
  return(1- sum((preds - actual) ^ 2)/sum((actual - mean(actual))^2))
}

## Placing fit statistics to dataframe.
FitStats_ridge_train=data.frame(RMSE = round(rmse(y_hat_ridge_cv, y),3),
                          Rsquare =round(Rsquare(y_hat_ridge_cv, y),3))
FitStats_ridge_train


## Fitting the model to test dataset.

y_hat_ridge_test <- predict(model_ridge_cv, Xt)

FitStats_ridge_test=data.frame(RMSE = round(rmse(y_hat_ridge_test, yt),3),
                          Rsquare =round(Rsquare(y_hat_ridge_test, yt),3))
FitStats_ridge_test

## Lasso Regression

## Performing 10-fold cross-validation to select lambda  
lambdas_lasso_try <- 10^seq(-3, 5, length.out = 100)

## Setting alpha = 1 implements lasso regression.

lasso_cv <- cv.glmnet(X, y, alpha = 1, lambda = lambdas_lasso_try,
                      standardize = TRUE, nfolds = 10)

## Plotting cross-validation results
Lassoplot <- plot(lasso_cv)

## Extracting optimum minimum and 1 s.e. values.
loglambda_lasso_opt <- round(log(lasso_cv$lambda.min),3)
loglambda_lasso_opt

lambda_lasso_min_opt <- round(lasso_cv$lambda.min,3)
lambda_lasso_min_opt

lambda_lasso_1se_opt <- round(lasso_cv$lambda.1se, 3)
lambda_lasso_1se_opt


## Fitting lasso Model and Display regression coefficients. (Alpha = 1)

model_lasso_cv <- glmnet(X, y, alpha = 1, lambda = lambda_lasso_min_opt, standardize = TRUE)

lasso_coef=round(coef(model_lasso_cv),3)
lasso_coef

## Fitting the model to train dataset and extracting Statistics/ performance metrics.

y_hat_lasso_train <- predict(model_lasso_cv, X)

FitStats_lasso_train=data.frame(RMSE = round(rmse(y_hat_lasso_train, y),3),
                          Rsquare =round(Rsquare(y_hat_lasso_train, y),3))
FitStats_lasso_train

## Test dataset fit results

y_hat_lasso_test <- predict(model_lasso_cv, Xt)

FitStats_lasso_test=data.frame(RMSE = round(rmse(y_hat_lasso_test, yt),3),
                          Rsquare =round(Rsquare(y_hat_lasso_test, yt),3))
FitStats_lasso_test

## Ridge Lasso Comparision

FitStatsRL<-rbind(FitStats_ridge_test, FitStats_lasso_test)
rownames(FitStatsRL)<-c("Ridge","Lasso")
FitStatsRL

## Step wise regression

hist(df$Grad.Rate)

Fitbase <- glm(Grad.Rate ~ 1, family = gaussian, data = sample_train)
summary(Fitbase)

Fitall <- glm(Grad.Rate ~ ., family = gaussian, data = sample_train)
summary(Fitall)


## Method 3 forward feature both directions (Stepwise)
step(Fitbase, direction = "both", scope = formula(Fitall))

Stepwise <- glm(formula = Grad.Rate ~ Outstate + Top25perc + perc.alumni + 
                Personal + Apps + P.Undergrad + Private + Room.Board + Expend + 
                PhD + Terminal, family = gaussian, data = sample_train)
summary(Stepwise)

m3predictions <- predict(Stepwise, sample_test)

# Calculate RMSE
model3rmse <- rmse(m3predictions, sample_test$Grad.Rate)
model3rmse

# Calculate R-squared
model3rsquared <- Rsquare(m3predictions, sample_test$Grad.Rate)
model3rsquared

FitStats_model3 = data.frame(RMSE = model3rmse, Rsquare = model3rsquared)
FitStats_model3

##Comparison
FitStats<-rbind(FitStats_ridge_test, FitStats_lasso_test, FitStats_model3)
rownames(FitStats)<-c("Ridge","Lasso", 'Stepwise')
FitStats

## Additional Coefficients comparison table.
ModelCoef<-cbind(round(coef(model_ridge_cv),5),round(coef(model_lasso_cv),5),round(coef(model3),5))
colnames(ModelCoef)<-c("Ridge","Lasso", "Model3")
ModelCoef


## Reset environment command
rm(list=ls())

## Serdar Selim Yesildag - 17/May/24 - ALY6015, Intermediate Analytics Week 6, Final Project Analysis.

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

library(crosstable)

library(ISLR)

library(pROC)

library(Metrics)

library(glmnet)

df <- read.csv("FPDF.csv")

summary(df)

view(df)

## Initial Analysis Section.

## Question 1: Is the mean effective time to recovery lower for patients who are on actual treatment compared to placebo group patients? 
## T-test, 95% Confidence interval.

## NUll Hypothesis H0: There is no difference in the mean effect time between the "Treatment" and "Placebo" groups.
## NUll Hypothesis H1: There is a difference in the mean effect time between the "Treatment" and "Placebo" groups.

TP <- df$EffectTime.days.[df$Arm == "Treatment"]
PP <- df$EffectTime.days.[df$Arm == "Placebo"]

# Perform a t-test
Q1t_test_result <- t.test(TP, PP, alternative = "two.sided", conf.int = 0.95)
Q1t_test_result

## P-value 0.5019, Fail to reject the null hypothesis. 

## Creating table for ANOVA Plot.
plotdata<-df %>% 
  group_by(Arm) %>%
  summarize(n=n(),mean=mean(EffectTime.days.), sd=sd(EffectTime.days.), ci=qt(0.95,df=n-1)*sd/sqrt(n))
view(plotdata)

## ANOVA Plot
Q1plot <- ggplot(plotdata, 
                 aes(x=Arm,y=mean,group=1))+
  geom_point(size=3, color="red")+
  geom_line(linetype="dashed",color="darkgrey")+
  geom_errorbar(aes(ymin=mean-ci,ymax=mean+ci),width=0.5)+
  theme_bw()+
  labs(x="Threatment Arm", y="Mean Days to Recovery", title="Mean Plot with 95%CI")
Q1plot

## Question 2: Are the satisfaction scores higher for treatment patients compared to placebo patients?
## NUll Hypothesis H0: There is no difference in the mean satisfaction scores between the "Treatment" and "Placebo" groups.
## NUll Hypothesis H1: There is a difference in the mean satisfaction scores between the "Treatment" and "Placebo" groups.

Q2TP <- df$SatisfactionScore[df$Arm == "Treatment"]
Q2PP <- df$SatisfactionScore[df$Arm == "Placebo"]

# Perform a t-test
Q2t_test_result <- t.test(Q2TP, Q2PP, alternative = "two.sided", conf.int = 0.95)
Q2t_test_result

## Conclusions
## P-value 0.09267, Fail to reject the null hypothesis. (Marginal and would be rejected at 90%)

## Creating table for ANOVA Plot.
plotdata2<-df %>% 
  group_by(Arm) %>%
  summarize(n=n(),mean=mean(SatisfactionScore), sd=sd(SatisfactionScore), ci=qt(0.95,df=n-1)*sd/sqrt(n))
view(plotdata2)

## ANOVA Plot
Q2plot <- ggplot(plotdata2, 
                 aes(x=Arm,y=mean,group=1))+
  geom_point(size=3, color="red")+
  geom_line(linetype="dashed",color="darkgrey")+
  geom_errorbar(aes(ymin=mean-ci,ymax=mean+ci),width=0.5)+
  theme_bw()+
  labs(x="Threatment Arm", y="Mean Satisfaction Score", title="Mean Plot with 95%CI")
Q2plot

## Question 3:What is the relationship between patients’ gender, age, and BMI with the effective recovery time? 

Q3_df <- subset(df, select = c(Arm, Sex, Age.years., BMI, EffectTime.days.))
view(Q3_df)

## Splitting Data frame into subsets of Male and Female patients
fdf <- subset(Q3_df, Sex == "Female")
mdf <- subset(Q3_df, Sex == "Male")

## Splitting gender data frames into subsets of placebo and treatment patients and removing categorical variables.
tfdf <- subset(fdf, Arm == "Placebo")
tfdf <- subset(tfdf, select = c(Age.years., BMI, EffectTime.days.))
pfdf <- subset(fdf, Arm == "Treatment")
pfdf <- subset(pfdf, select = c(Age.years., BMI, EffectTime.days.))
tmdf <- subset(mdf, Arm == "Placebo")
tmdf <- subset(tmdf, select = c(Age.years., BMI, EffectTime.days.))
pmdf <- subset(mdf, Arm == "Treatment")
pmdf <- subset(pmdf, select = c(Age.years., BMI, EffectTime.days.))

## Creating Correlation plots for subsets of patients by gender and treatment arm.

tfdfcplot <- corPlot(tfdf, cex = 1.2, main = "Treatment Female Patients")
tfdfcplot

pfdfcplot <- corPlot(pfdf, cex = 1.2, main = "Placebo Female Patients")
pfdfcplot

tmdfcplot <- corPlot(tmdf, cex = 1.2, main = "Treatment Male Patients")
tmdfcplot

pmdfcplot <- corPlot(pmdf, cex = 1.2, main = "Placebo Male Patients")
pmdfcplot


## Question 4: Is the rate of relapse different for patients in different age groups OR sex category?

Q4df <- subset(df, Arm == "Treatment")
view(Q4df)
summary(Q4df)

## Creating categorical variables based on patients ages.

Q4df$Agecat <- dplyr::case_when(Q4df$Age.years. > 20 & Q4df$Age.years. <= 39 ~ '20s and 30s', 
                                Q4df$Age.years. >= 40 & Q4df$Age.years. <= 59 ~ '40s and 50s',
                                Q4df$Age.years. >= 60 & Q4df$Age.years. <= 70 ~ '60 and Above', 
                                TRUE ~ 'NA')

view(Q4df)

## Question 4a: Gender and outcome

## Creating count and share tables for gender and outcome.
Gender_table <- table(Q4df$OutcomeStatus, Q4df$Sex)
Gender_table

## Chi-Square Test
Q4aChiresult <- chisq.test(Gender_table)
Q4aChiresult

## Fishers Test
Q4bfisher_result <- fisher.test(Gender_table)
Q4bfisher_result

## Question 4b: Age Category and outcome
## Creating count and share tables for Age Category and outcome.
Age_table <- table(Q4df$OutcomeStatus, Q4df$Agecat)
Age_table

## Chi-Square Test
Q4bChiresult <- chisq.test(Age_table)
Q4bChiresult

## Fishers Test
Q4bfisher_result <- fisher.test(Age_table)
Q4bfisher_result

## Question 4c: Is the rate of relapse different for patients is dependent on study arm category?
## Creating counts table for arm and outcome.
Arm_table <- table(df$Arm, df$OutcomeStatus)
Arm_table

## Chi-Square Test
Q5Chiresult <- chisq.test(Arm_table)
Q5Chiresult

## Fishers Test
Q5fisher_result <- fisher.test(Arm_table)
Q5fisher_result

## Additional Analysis for first Draft Section.

## Linear Regression Analysis.

## Question 3b: Advanced regression analysis between the variables investigated in question 3.

## Converting categorical variable private to Boolean 
df <- df %>%
  mutate(Arm = ifelse(Arm == "Placebo",0,1))

df <- df %>%
  mutate(Sex = ifelse(Sex == "Male",0,1))

## Sub-setting DF for Treatment Patients.

df <- subset(df, Improvement.Outcome == "Complete")

df <- subset(df, select = -Improvement.Outcome)
df <- subset(df, select = -ID)

df <- subset(df, Arm == 1)

df <- subset(df, select = -Arm)

view(df)

## Checking for normality Assumption. Poisson!!!
hist(df$EffectTime.days.)

## Splitting dataset into training and testing sections.
trainIndex <- sort(sample(x = nrow(df), size = nrow(df) * 0.7))
sample_train <- df[trainIndex,]
sample_test <- df[-trainIndex,]

## Converting variable values into matrices for training dataset
y <- sample_train %>% select(EffectTime.days.) %>% scale(center = TRUE, scale = FALSE) %>% as.matrix()
X <- sample_train %>% select(-EffectTime.days.) %>% as.matrix()

## Converting variable values into matrices for test dataset
yt <- sample_test %>% select(EffectTime.days.) %>% scale(center = TRUE, scale = FALSE) %>% as.matrix()
Xt <- sample_test %>% select(-EffectTime.days.) %>% as.matrix()

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
lambdas_lasso_try <- 10^seq(-2, 7, length.out = 100)

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

## Stepwise regression

hist(df$EffectTime.days.)

Fitbase <- glm(EffectTime.days. ~ 1, family = gaussian, data = sample_train)
summary(Fitbase)

Fitall <- glm(EffectTime.days. ~ ., family = gaussian, data = sample_train)
summary(Fitall)


## Method 3 forward feature both directions (Stepwise)
step(Fitbase, direction = "both", scope = formula(Fitall))

Stepwise <- glm(formula = EffectTime.days. ~ BMI, family = gaussian, data = sample_train)

summary(Stepwise)

Stepwise$coefficients

m3predictions <- predict(Stepwise, sample_test)

# Calculate RMSE
model3rmse <- rmse(m3predictions, sample_test$EffectTime.days.)
model3rmse

# Calculate R-squared
model3rsquared <- Rsquare(m3predictions, sample_test$EffectTime.days.)
model3rsquared

FitStats_model3 = data.frame(RMSE = model3rmse, Rsquare = model3rsquared)
FitStats_model3

##Comparison
FitStats<-rbind(FitStats_ridge_test, FitStats_lasso_test, FitStats_model3)
rownames(FitStats)<-c("Ridge","Lasso", 'Stepwise')
FitStats

## Logistic Regression Analysis.

## Question 5: What variables contribute to relapse rate for Patients on treatment.

# Choosing a Best subset with reg subsets.

best_subset = regsubsets(OutcomeStatus ~ ., data = df, nbest=3, method="exhaustive")
reg.summary <- summary(best_subset)
reg.summary
names(reg.summary)

with(summary(best_subset), data.frame(rsq, adjr2, cp, rss, bic, outmat))

summary(best_subset)$adjr2
summary(best_subset)$bic

## Creating the first logistic regression model from train dataset.

model1 <- glm(OutcomeStatus ~ Age.years., family = binomial, data = df)
model1

summary(model1)


## Predicting Outcome status with the logistic model created.
predict.glm(model1, df, type="response")

actual_labels <- df$OutcomeStatus

# Convert predicted probabilities to predicted class labels
predicted_labels <- ifelse(predict(model1, df, type="response") > 0.5, 1, 0) 

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

## Two Way ANOVA testing.

## Question 6: Is there a difference between patients in difference age and gender groups in mean recovery time for patients on treatment?
summary(df)

## Creating categorical variables based on patients ages.

df$Agecat <- dplyr::case_when(df$Age.years. > 20 & df$Age.years. <= 29 ~ '20s',
                              df$Age.years. >= 30 & df$Age.years. <= 39 ~ '30s',
                              df$Age.years. >= 40 & df$Age.years. <= 49 ~ '60s',
                                df$Age.years. >= 50 & df$Age.years. <= 59 ~ '50s',
                                df$Age.years. >= 60 & df$Age.years. <= 70 ~ '60 and Above', 
                                TRUE ~ 'NA')

view(df)

anova_result <- aov(EffectTime.days. ~ Agecat*Sex , data = df)
anova_result
summary(anova_result)

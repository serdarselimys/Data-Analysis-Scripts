## Reset environment command
rm(list=ls())

## Serdar Selim Yesildag - 18/June/24 - ALY6050, Week 3, Assignment 3.

library(readxl)
library(ggplot2)
library("janitor")
library("aTSA")
library("smooth")
library("TTR")
library(forecast)
library(Metrics)
library("EnvStats")

# importing the dataset
stock_df <- read_xlsx(path = "ALY6050_Module3Project_Data.xlsx")

# Using the clean_name function to standardize the names in your data frame.
stock_df<-clean_names(stock_df)

# Getting the column names (or variable's names) in the given data frame. 
colnames(stock_df)

## Exercise 1a
Apple_Plot <- ggplot(stock_df, mapping = aes(x = period, y = aapl_apple_inc)) + 
  geom_line() + 
  labs(
    title = "Apple Inc. Stock Price Over Time",  
    x = "Time Period",                          
    y = "Stock Price (AAPL)"                    
  )

Apple_Plot

##Honeywell
Honeywell_Plot <- ggplot(stock_df, mapping = aes (x=period, y=hon_honeywell_inc)) + geom_line() +
  labs(
  title = "Honeywell Inc. Stock Price Over Time",  
  x = "Time Period",                          
  y = "Stock Price (HON)"                    
)
Honeywell_Plot

## Exercise 1b

# Define alpha values
alpha_values <- c(0.15, 0.35, 0.55, 0.75)

# Initialize empty dataframe to store results
Apple_results_df <- data.frame(
  Alpha = numeric(length(alpha_values)),
  Predicted_Value = numeric(length(alpha_values)),
  RMSE = numeric(length(alpha_values))
)

# Loop through each alpha value
for (i in seq_along(alpha_values)) {
  alpha <- alpha_values[i]
  
  # Perform exponential smoothing
  Apple <- expsmooth(stock_df$aapl_apple_inc, alpha = alpha, lead=1)  
  
  # Extract predicted value and RMSE
  PRED <- Apple$pred
  RMSE <- Apple$accurate["RMSE"]
  
  # Store results in dataframe
  Apple_results_df[i, "Alpha"] <- alpha
  Apple_results_df[i, "Predicted_Value"] <- PRED
  Apple_results_df[i, "RMSE"] <- RMSE
}

# Print the results dataframe
print(Apple_results_df)

##Honeywell Calc

# Initialize empty dataframe to store results
Hon_results_df <- data.frame(
  Alpha = numeric(length(alpha_values)),
  Predicted_Value = numeric(length(alpha_values)),
  RMSE = numeric(length(alpha_values))
)

# Loop through each alpha value
for (i in seq_along(alpha_values)) {
  alpha <- alpha_values[i]
  
  # Perform exponential smoothing
  Honey <- expsmooth(stock_df$hon_honeywell_inc, alpha = alpha, lead=1)  
  
  # Extract predicted value and RMSE
  PRED <- Honey$pred
  RMSE <- Honey$accurate["RMSE"]
  
  # Store results in dataframe
  Hon_results_df[i, "Alpha"] <- alpha
  Hon_results_df[i, "Predicted_Value"] <- PRED
  Hon_results_df[i, "RMSE"] <- RMSE
}

# Print the results dataframe
print(Hon_results_df)

## Exercise 1c
help("expsmooth")
aes <- expsmooth(stock_df$aapl_apple_inc, trend=1, alpha=0.55, beta=0.15, lead=1)

aes$pred #predicted value

aes$accurate["RMSE"]

# Vector of beta values
beta_values <- c(0.15, 0.25, 0.45, 0.85)

# Initialize empty dataframe to store results
Apple_results_df <- data.frame(beta = numeric(0), pred = numeric(0), RMSE = numeric(0))

# Loop through each beta value
for (beta in beta_values) {
  # Run exponential smoothing with given beta
  aes <- expsmooth(stock_df$aapl_apple_inc, trend = 2, alpha = 0.55, beta = beta, lead = 1)
  
  # Extract predicted value
  pred_value <- aes$pred
  
  # Extract RMSE value
  rmse_value <- aes$accurate["RMSE"]
  
  # Append results to dataframe
  Apple_results_df <- rbind(Apple_results_df, data.frame(beta = beta, pred = pred_value, RMSE = rmse_value))
}

# Print the results dataframe
print(Apple_results_df)

##Honeywell

Honey_results_df <- data.frame(beta = numeric(0), pred = numeric(0), RMSE = numeric(0))

# Loop through each beta value
for (beta in beta_values) {
  # Run exponential smoothing with given beta
  aes <- expsmooth(stock_df$hon_honeywell_inc, trend = 2, alpha = 0.55, beta = beta, lead = 1)
  
  # Extract predicted value
  pred_value <- aes$pred
  
  # Extract RMSE value
  rmse_value <- aes$accurate["RMSE"]
  
  # Append results to dataframe
  Honey_results_df <- rbind(Honey_results_df, data.frame(beta = beta, pred = pred_value, RMSE = rmse_value))
}

# Print the results dataframe
print(Honey_results_df)

## Part 2a

##Apple
subsetdf <-stock_df[stock_df$period >= 1 & stock_df$period <= 100, ]

Apple <- WMA(subsetdf$aapl_apple_inc, n=3, wts=c(0.2, 0.3, 0.5))

ApplePlot1 <-ggplot(subsetdf, aes(x=period)) + geom_line(aes(y=aapl_apple_inc), color="darkred") +
  geom_line(aes(y=Apple), color="steelblue") + 
  labs(
    title = "Apple Inc. Stock Price Over Time",  
    x = "Time Period",                          
    y = "Stock Price (AAPL)"                    
  )
ApplePlot1

Apple_res= subsetdf$aapl_apple_inc - Apple
Apple_res

Apple_res <- na.omit(Apple_res)

Apple_rmse <- sqrt(mean(Apple_res^2))
Apple_rmse

##HoneyWell
Honey <- WMA(subsetdf$hon_honeywell_inc, n=3, wts=c(0.2, 0.3, 0.5))
Honey

HoneyPlot <- ggplot(subsetdf, aes(x=period)) + geom_line(aes(y=hon_honeywell_inc), color="darkred") +
  geom_line(aes(y=Honey), color="steelblue") + 
  labs(
    title = "Honeywell Inc. Stock Price Over Time",  
    x = "Time Period",                          
    y = "Stock Price (HON)"                    
  )
HoneyPlot

honey_res= subsetdf$hon_honeywell_inc - Honey
honey_res

honey_res <- na.omit(honey_res)

honey_rmse <- sqrt(mean(honey_res^2))
honey_rmse

## Part 2b
rmse(stock_df$aapl_apple_inc[4:101], Apple[3:100])
rmse(stock_df$hon_honeywell_inc[4:101], Honey[3:100])

## Part 3a

## Apple
apple_linear_model = lm(aapl_apple_inc ~ period, data=stock_df)
summary(apple_linear_model) 

apple_pre=predict(apple_linear_model, newdata = stock_df)

p <- as.data.frame(1:257)
colnames(p) <- 'period'

apple_new_pre <- predict(apple_linear_model, newdata = p)

apple_res= stock_df$aapl_apple_inc - apple_pre

apple_res <- na.omit(apple_res)

apple_rmse <- sqrt(mean(apple_res^2))
apple_rmse

## Honey
honey_linear_model = lm(hon_honeywell_inc ~ period, data=stock_df)
summary(apple_linear_model) 

honey_pre=predict(honey_linear_model, newdata = stock_df)

p <- as.data.frame(1:257)
colnames(p) <- 'period'

honey_new_pre <- predict(honey_linear_model, newdata = p)

honey_res= stock_df$hon_honeywell_inc - honey_pre
honey_res

honey_res <- na.omit(honey_res)

honey_rmse <- sqrt(mean(honey_res^2))
honey_rmse

## Part 3b

## Apple
gofTest(apple_res, distribution = "norm", test="chisq")
hist(apple_res, main="Apple Stock Residuals", xlab = "Residuals")

## Honey
gofTest(honey_res, distribution = "norm", test="chisq")
hist(honey_res, main="Honeywell Stock Residuals", xlab = "Residuals")
## Reset environment command
rm(list=ls())

## Serdar Selim Yesildag - 31/March/24 - ALY6010, Final Project.

## Loading libraries to be used in the project.
library(tidyverse)

library(dplyr)

library(janitor)

library(tidyr)

library(ggplot2)

library(stringr)

library(DescTools)

library(scales)

library(lubridate)

library(ggpubr)

library(crosstable)

library(readxl)

library(psych)

library(jtools)

library(bruceR)



## Loading the dataset into R studio.
df <- read.csv("tx_dataset.csv")

## inspecting dataframe to identify variable that wont be utilized in later analysis.
head(df)

## Extract the subset of variables from the dataframe that will be used in the analysis.
df = subset(df, select = -c(Trip.ID,Trip.End.Timestamp, Taxi.ID, Pickup.Census.Tract,Dropoff.Census.Tract,Pickup.Centroid.Latitude, Pickup.Centroid.Longitude,Dropoff.Centroid.Longitude, Dropoff.Centroid.Longitude,Dropoff.Centroid..Location, Pickup.Centroid.Location, Dropoff.Centroid.Latitude, Pickup.Community.Area, Dropoff.Community.Area))

## Inspecting the list of remaining columns in the dataset.
colnames(df)

## Applying formatting to column names for ease of scripting.
df <- clean_names(df)

## Sampling the dataset to 6500 rows for reducing the dataset file size for the assignment as recommended by the instructor.
set.seed(10)
df2 <- df[sample(nrow(df), 6500, replace = FALSE),]

## Formatting the Datetime stamp variable for ease of scripting and information extraction to be performed later in the analysis.
df2$trip_start_timestamp <- mdy_hms(df2$trip_start_timestamp)

## Inspecting the sample dataset with summary command.
summary(df2)

## Dropping N/A variables from the dataset.
df2 <- na.omit(df2)

## Saving the subset dataframe to a csv file to uploading to canvas platform with assignment delivery.
write.csv(df2, "C:\\Users\\Xerd-R\\Desktop\\NorthEastern\\Classes\\ALY 6010\\Module 6\\Yesidag_Final_Project\\Yesildag_Final_Project\\Taxidf_m2.csv", row.names=TRUE)

## Extracting day information from date variable.
df2$day <- weekdays(as.Date(df2$trip_start_timestamp))

## Categorizing day of the week into week and weekend day groups
df2$day_type <- ifelse(df2$day %in% c('Sunday', 'Saturday'), 'Weekend', 'Weekday')

## Separating the datetime stamp variable into two date and time variables for categorization of time variable.
df2 <- separate(df2, trip_start_timestamp, c("date", "time"), sep = " ")

## Dropping N/A variables created during the date time column separation operation above.
df2 <- na.omit(df2)

## Extracting the hour of stop from date variable.
df2$hourofday <- as.numeric(substr(df2$time, 1, 2))

## Placing hours of the day into time of the day categories in a new variable.
df2$timeofday <- dplyr::case_when(df2$hourofday > 6 & df2$hourofday < 12 ~ 'morning', 
                                  df2$hourofday >= 12 & df2$hourofday < 18 ~ 'afternoon', 
                                  df2$hourofday >= 18 & df2$hourofday <= 22 ~ 'evening', 
                                  TRUE ~ 'night')

## Assigning factors to Times of the day values. 
df2 <- df2 %>% 
  mutate(timeofday=factor(timeofday, 
                          levels=c("morning", "afternoon", "evening", "night"), 
                          ordered=T)) %>%   arrange(timeofday)

## Assigning factors to day type values. 
df2 <- df2 %>% 
  mutate(day_type=factor(day_type, 
                         levels=c("Weekday", "Weekend"), 
                         ordered=T)) %>%   arrange(day_type)

## Assigning factors to days of the week values. 
df2 <- df2 %>% 
  mutate(day=factor(day, 
                    levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), 
                    ordered=T)) %>%   arrange(day)

## Additional Filters for entries suspect of data entry errors.
df2 <- df2[df2$trip_miles > 0,]
df2 <- df2[df2$trip_seconds > 60,]
df2 <- df2[df2$fare > 0,]

## Creating a new variable for distance covered in each trip with trip miles variable values rounded.
df2$distance <- round(df2$trip_miles, digits = 0)

## Creating a new variables for trip time length in minutes by dividing the trip length variable in seconds to 60.
df2$trip_mins <- round((df2$trip_seconds)/60, digits = 0)

##Creating a new variables for combining payment methods other than Cash into one.
df2$pay_method <- dplyr::case_when(df2$payment_type == 'Cash' ~ 'Cash', TRUE ~ 'Other')

## Milestone 1

## Calculating and storing statistical values of interest in to their respective vectors.
mean_triptime_m = mean(df2$trip_mins)
median_triptime_m = median(df2$trip_mins)
sd_triptime_m = sd(df2$trip_mins)
min_triptime_m = min(df2$trip_mins)
max_triptime_m = max(df2$trip_mins)
range_triptime_m = max(df2$trip_mins) - min(df2$trip_mins)

mean_farecost_USD = mean(df2$fare)
median_farecost_USD = median(df2$fare)
sd_farecost_USD = sd(df2$fare)
min_farecost_USD = min(df2$fare)
max_farecost_USD = max(df2$fare)
range_farecost_USD = max(df2$fare) - min(df2$fare)

mean_distance = mean(df2$distance)
median_distance = median(df2$distance)
sd_distance = sd(df2$distance)
min_distance = min(df2$distance)
max_distance = max(df2$distance)
range_max_distance = max(df2$distance) - min(df2$distance)

## Storing calculated values into their respective vectors and producing a dataframe.
statistic <- c('Trip_Time', 'Fare_Cost', 'Trip_Distance')
mean <- c(mean_triptime_m, mean_farecost_USD, mean_distance)
median <- c(median_triptime_m, median_farecost_USD, median_distance)
sd <- c(sd_triptime_m, sd_farecost_USD, sd_distance)
min <- c(min_triptime_m, min_farecost_USD, min_distance)
max <- c(max_triptime_m, max_farecost_USD, max_distance)
range <- c(range_triptime_m, range_farecost_USD, range_max_distance)

##Producing a dataframe with the vectors containing the basic statistical values.
df_mm <- data.frame(statistic, mean, median, sd, min, max, range)

df_mm$mean <- round(df_mm$mean, digits=2)
df_mm$sd<- round(df_mm$sd, digits=2)

## Saving the statistical informaiton dataframe to a csv file to uploading to canvas platform with assignment delivery.
write.csv(df_mm, "C:\\Users\\Xerd-R\\Desktop\\NorthEastern\\Classes\\ALY 6010\\Module 6\\Yesidag_Final_Project\\Yesildag_Final_Project\\Stats.csv", row.names=TRUE)

## Setting IQR filters for eliminating outliers.

## Distance Variable Filters
dist_quintiles <- quantile(df2$distance)
dist_quintiles

LDist <- 1 - 1.5*(IQR(df2$distance))
LDist
HDist <- 13 + 1.5*(IQR(df2$distance))
HDist

## Fare Variable Filters
fare_quintiles <- quantile(df2$fare)
fare_quintiles

Lfare <- 9 - 1.5*(IQR(df2$fare))
Lfare
Hfare <- 35.25 + 1.5*(IQR(df2$fare))
Hfare

## Trip mins Variable Filters
mins_quintiles <- quantile(df2$trip_mins)
mins_quintiles

Lmins <- 9 - 1.5*(IQR(df2$trip_mins))
Lmins
Hmins <- 28 + 1.5*(IQR(df2$trip_mins))
Hmins

##Visualizations

## Box plot for variables of interest.

# Vector for filtered Trip Minutes values
IQRTripmins <- df2$trip_mins[df2$trip_mins < Hmins & df2$trip_mins > Lmins]

# Box plot of Trip Minutes
minbox <- ggplot(data = NULL, aes(x = "", y = IQRTripmins)) + 
  geom_boxplot() +                                      
  labs(title = "Box Plot of Trip Lengths in Minutes (1.5 IQR)", 
       y = "Trip Length (Minutes)",
       x = NULL)
minbox

# Vector for filtered Trip fare values
IQRfare <- df2$fare[df2$fare < Hfare & df2$fare > Lfare]

# Box plot of Trip Fare Values
farebox <- ggplot(data = NULL, aes(x = "", y = IQRfare)) + 
  geom_boxplot() +                                      
  labs(title = "Box Plot of Fare Costs in USD (1.5 IQR)", 
       y = "Trip Fare Costs (USD)",
       x = NULL)
farebox

# Vector for filtered Trip distance values

IQRdistance <- df2$distance[df2$distance < HDist & df2$distance > LDist]

# Box plot of Trip distance Values
distbox <- ggplot(data = NULL, aes(x = "", y = IQRdistance)) + 
  geom_boxplot() +                                      
  labs(title = "Box Plot of Trip Length In Distance in Miles (1.5 IQR)", 
       y = "Trip Lenght in Distance (Miles)",
       x = NULL)
distbox

## filtering the dataset for outliers set at 1.5 IQR for creating scatter charts.

IQRdf <- df2[df2$trip_mins >= Lmins & df2$trip_mins < Hmins & df2$fare >= Lfare & df2$fare < Hfare & df2$distance < HDist & df2$distance > LDist,]

## Scatter plot / Trip time vs fare costs.
minfarescat <- ggplot(IQRdf, aes(x = trip_mins, y = fare)) +
  geom_point(color = "red") +
  labs(title = "Trip Length in Minutes vs Trip Fare Cost",
       x = "Trip Length in Mins",
       y = "Trip Fare Cost in USD")
minfarescat

## Scatter plot / trip time vs trip distance.
mindistscat <- ggplot(IQRdf, aes(x = trip_mins, y = distance)) +
  geom_point(color = "red") +
  labs(title = "Trip Length in Minutes vs Trip Distance in Miles",
       x = "Trip Length in Mins",
       y = "Trip Distance in Miles")
mindistscat

## Scatter plot / fare costs vs trip distance.
faredistscat <- ggplot(IQRdf, aes(x = fare, y = distance)) +
  geom_point(color = "red") +
  labs(title = "Trip Length in Costs vs Trip Distance in Miles",
       x = "Trip Fare Cost in USD",
       y = "Trip Distance in Miles")
faredistscat

## Box plot for number of trips within days of the week.

Box_TripsDays <- ggplot(IQRdf, aes(x = day, y = fare)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Trip Fare Costs for Different Days of the Week",
       x = "Days of the Week",
       y = "Trip Fare Costs in USD")
Box_TripsDays

## Box plot for number of trips within times of the day.

Box_Tripstimes <- ggplot(IQRdf, aes(x = timeofday, y = fare)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Trip Fare Costs for Different Times of the Day",
       x = "Times of Day",
       y = "Trip Fare Costs in USD")
Box_Tripstimes

## Stacked bar chart frequency table creation.
freg_count_by_DayandDaytime <- df2 %>%
  group_by(day, timeofday) %>%
  summarise(Count = n()) %>%
  mutate(freq = Count / sum(Count))

freg_count_by_DayandDaytime <- freg_count_by_DayandDaytime %>% 
  mutate(timeofday=factor(timeofday, 
                          levels=c("morning", "afternoon", "evening", "night"), 
                          ordered=T)) %>%   arrange(timeofday)

freg_count_by_DayandDaytime <- freg_count_by_DayandDaytime %>% 
  mutate(day=factor(day, 
                    levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), 
                    ordered=T)) %>%   arrange(day)

freg_count_by_DayandDaytime$freq <- round(freg_count_by_DayandDaytime$freq, digits=2)

## Stacked Bar Chart for Frequencies of Taxi Trips by Day of the Week and Time of Day.

StacketPlot1 <- ggplot(freg_count_by_DayandDaytime, aes(fill=timeofday, y=freq, x=day)) + 
  geom_bar(position="fill", stat="identity", color='black',width=0.9)
StacketPlot1

StacketPlot1 <- StacketPlot1 +geom_text(aes(label = freq), size = 3, hjust = 0.5, vjust = 3, position = "stack", color = "red")

StacketPlot1 <- StacketPlot1 + ggtitle("Frequencies of Taxi Trips by Day of the Week and Time of Day") + xlab("Day of the Week") + ylab("Frequincies")

StacketPlot1

## Milestone 2

## Question 1. Are the average taxi trip fare costs different on weekdays vs weekends. (Two Sided, Two Sample T-test)

## Visual inspection for outliers via boxplot.
ggplot(IQRdf, aes(x=day_type, y=fare)) +
  geom_boxplot()

## filtering the dataset for outliers set at 1.5 IQR in Weekday Fares.

Weekdayfares <- IQRdf$fare[IQRdf$day_type == "Weekday"]

wdf_quintiles <- quantile(Weekdayfares)
wdf_quintiles

Lwdf <- 8.50 - 1.5*(IQR(Weekdayfares))
Lwdf
Hwdf <- 33 + 1.5*(IQR(Weekdayfares))
Hwdf

Weekdayfares <- Weekdayfares[Weekdayfares >= Lwdf]
Weekdayfares <- Weekdayfares[Weekdayfares <= Hwdf]

boxplot(Weekdayfares, main="Weekday Fare Values (filtered)")

## filtering the dataset for outliers set at 1.5 IQR in Weekend Fares.

Weekendfares <- IQRdf$fare[IQRdf$day_type == "Weekend"]

wnf_quintiles <- quantile(Weekendfares)
wnf_quintiles

Lwnf <- 10 - 1.5*(IQR(Weekendfares))
Lwnf
Hwnf <- 37.25 + 1.5*(IQR(Weekendfares))
Hwnf

Weekendfares <- Weekendfares[Weekendfares >= Lwnf]
Weekendfares <- Weekendfares[Weekendfares <= Hwnf]

boxplot(Weekendfares, main="Weekend day Fare Values (filtered)")

## Hypothesis testing.

## Null Hypothesis Average Trip Fare costs are same for weekdays and Weekends.
## Alternative Hypothesis Average Trip Fare costs are NOT same for weekdays and Weekends.

Ttest1 = t.test(x= Weekendfares, y = Weekdayfares, alternative = "two.sided", conf.int = 0.95)
Ttest1$p.value

## Question 2.	Are the average taxi trips costs paid with cash lower than the payments made with other methods.  (One Sided, Two Sample T-test)

## Visual inspection for outliers via box plot.
ggplot(IQRdf, aes(x=pay_method, y=fare)) +
  geom_boxplot()

## Creating vectors for each category values and filtering out the outliers for each vector.

##Cash Payments.
Cash <- IQRdf$fare[IQRdf$pay_method == "Cash"]

Cash_quintiles <- quantile(Cash)
Cash_quintiles

LCash <- 6.5 - 1.5*(IQR(Cash))
LCash
HCash <- 25.75 + 1.5*(IQR(Cash))
HCash

Cash <- Cash[Cash >= LCash]
Cash <- Cash[Cash <= HCash]

boxplot(Cash, main="Cash Payment Values (filtered)")

## Payments made with other methods.
Other <- IQRdf$fare[IQRdf$pay_method == "Other"]

Other_quintiles <- quantile(Other)
Other_quintiles

LOther <- 9.75 - 1.5*(IQR(Other))
LOther
HOther <- 35.50 + 1.5*(IQR(Other))
HOther

Other <- Other[Other >= LOther]
Other <- Other[Other <= HOther]

boxplot(Other, main="Other Payment Values (filtered)")

## Hypothesis testing.

## Null Hypothesis Average Trip Fare cost paid with Cash are lower than other payment methods. 
## Alternative Hypothesis Average Trip Fare cost paid with Cash are not lower than other payment methods.

Ttest2 = t.test(x= Cash, y = Other, alternative = "greater", conf.int = 0.95)
Ttest2$p.value

## Question 3. Is the average cost of taxi fares in the city of Chicago equal, below or above the national average? (36.17 U.S. dollars)

Trip_Total <- IQRdf$trip_total

## Visual inspection for outliers via boxplot.
boxplot(Trip_Total)

## Filtering out the outliers with 1.5 IQR
TT_quintiles <- quantile(Trip_Total)
TT_quintiles

LTT <- 10.75 - 1.5*(IQR(Trip_Total))
LTT
HTT <- 41 + 1.5*(IQR(Trip_Total))
HTT

Trip_Total <- Trip_Total[Trip_Total >= LTT]
Trip_Total <- Trip_Total[Trip_Total <= HTT]

ttplot <- ggqqplot(Trip_Total, ylab = "Trip Total Cost", ggtheme = theme_minimal())
ttplot

##Hypothesis testing

## A. Null Hypothesis: The average taxi trip cost in the City of Chicago is equal to average national taxi trip cost in United States.
## A. Alternative Hypothesis:The average taxi trip cost in the City of Chicago is NOT equal to average national taxi trip cost in United States.
Ttest3a <- t.test(Trip_Total, mu = 36, alternative = "two.sided", conf.int = 0.95)
Ttest3a$p.value

## B. Null Hypothesis: The average cost of taxi trips in the city of Chicago is greater than the average cost of taxi trips in the United States. 
## B. Alternative Hypothesis: The average cost of taxi trips in the city of Chicago is NOT greater than the average cost of taxi trips in the United States.
Ttest3b <- t.test(Trip_Total, mu = 36, alternative = "less", conf.int = 0.95)
Ttest3b$p.value

## C. Null Hypothesis:The average cost of taxi trips in the city of Chicago is less than the average cost of taxi trips in the United States.
## C. Alternative Hypothesis:The average cost of taxi trips in the city of Chicago is NOT less than the average cost of taxi trips in the United States.
Ttest3c <- t.test(Trip_Total, mu = 36, alternative = "greater", conf.int = 0.95)
Ttest3c$p.value

## Question 4. Is the average speed of a taxi ride in the city of Chicago less than the highest legal speed within the city limits by law? (55 miles per hour)

## Creating a new variable containing average speed of taxi trips (Miles Per Hour, MPH)
IQRdf$mph <- round(IQRdf$trip_miles/(IQRdf$trip_mins/60), digits = 0)

## Visual inspection for outliers via boxplot.
boxplot(IQRdf$mph)

MPH <- IQRdf$mph

MPH_quintiles <- quantile(MPH)
MPH_quintiles

LMPH <- 10 - 1.5*(IQR(MPH))
LMPH
HMPH <- 29 + 1.5*(IQR(MPH))
HMPH

MPH <- MPH[MPH >= LMPH]
MPH <- MPH[MPH <= HMPH]

## Visual inspection for outliers via boxplot.
boxplot(MPH, main="Average Speeds of Taxi Trips (filtered)")

## Hypothesis testing.

## Null Hypothesis:The average speed of Taxi trips in the city of Chicago is below the legal speed limit of 55 mph.
## Alternative Hypothesis: The average speed of Taxi trips in the city of Chicago is NOT below the legal speed limit of 55 mph.

Ttest4 <- t.test(MPH, mu = 55, alternative = "greater", conf.int = 0.99)
Ttest4$p.value

## Milestone 3

## Creating a separate Data frame for Correlation Analysis of Filtered Data frame and graph.
Corrdf = subset(IQRdf, select = c(distance, trip_mins, fare, tips))

colnames(Corrdf) <- c("Distance", "Minutes", "Cost", "Tips")

CorrPlot <- corPlot(Corrdf, cex = 1.2, main = "Correlation Values of Variables of Interest (Filtered)")

corr<-round(cor(Corrdf),2)
corr

print_table(corr, title = "Correlation Analysis Table of Chicago Taxi Trips Attributtes", 
            row.names = c("Distance", "Trip Duration", "Fare Cost", "Tip Ammounts"), 
            col.names = c("Distance", "Trip Duration", "Fare Cost", "Tip Ammounts"))



## Creating a separate Data frame for Correlation Analysis of Unfiltered Data frame and graph.
Corrdf2 = subset(df2, select = c(distance, trip_mins, fare, tips))

colnames(Corrdf2) <- c("Distance", "Minutes", "Cost", "Tips")

CorrPlot <- corPlot(Corrdf2, cex = 1.2, main = "Correlation Values of Variables of Interest (Unfiltered)")

corr2<-round(cor(Corrdf2),2)
corr2

print_table(corr2, title = "Correlation Analysis Table of Chicago Taxi Trips Attributtes", 
            row.names = c("Distance", "Trip Duration", "Fare Cost", "Tip Ammounts"), 
            col.names = c("Distance", "Trip Duration", "Fare Cost", "Tip Ammounts"))


## Simple Linear Regression Analysis.

## Model 1 - tips and fare
Model1 <- glm(tips ~ fare, family = "gaussian", data = Corrdf)
summary(Model1)

Model1g=ggplot(Corrdf, aes(x=fare, y=tips)) +
  labs(title = "Scatter Chart of Trip Fare Costs and Tip Ammounts with Regression Line",
       x = "Trip Fare Costs in USD",
       y = "Tip Ammounts in USD")+
  geom_point(size=2, color="red", shape="circle")+
  geom_smooth(method = lm, color="green", fill="gold", se=TRUE)+
  theme_bw()
Model1g

Model1$coefficients

## Model 2 - tips and distance
Model2 <- glm(tips ~ distance, family = "gaussian", data = Corrdf)
summary(Model2)

Model2$coefficients

Model2g=ggplot(Corrdf, aes(x=distance, y=tips)) +
  labs(title = "Scatter Chart of Trip Distance and Tip Ammounts with Regression Line",
       x = "Trip Length in Miles",
       y = "Tip Ammounts in USD")+
  geom_point(size=2, color="red", shape="circle")+
  geom_smooth(method = lm, color="green", fill="gold", se=TRUE)+
  theme_bw()
Model2g

## Model 3 - tips and Trip Duration
Model3 <- glm(tips ~ trip_mins, family = "gaussian", data = Corrdf)
summary(Model3)

Model3$coefficients

Model3g=ggplot(Corrdf, aes(x=trip_mins, y=tips)) +
  labs(title = "Scatter Chart of Trip Minutes and Tip Ammounts with Regression Line",
       x = "Trip Length in Minutes",
       y = "Tip Ammounts in USD")+
  geom_point(size=2, color="red", shape="circle")+
  geom_smooth(method = lm, color="green", fill="gold", se=TRUE)+
  theme_bw()
Model3g




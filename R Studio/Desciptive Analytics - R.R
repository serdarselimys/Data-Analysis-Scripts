## Reset environment command
rm(list=ls())

## Loading libraries to be used in the project.
library(tidyverse)

library(dplyr)

library(lubridate)

library(janitor)

library(tidyr)

library(ggplot2)

## Loading the dataset into Rstudio
df <- read.csv("MTV.csv")

## modifying the column names for analysis with Rstudio.
df <- clean_names(df)

## Listing column names for dataset exploration.
colnames(df)

## Extracting summary statistical information of variables contained in the dataset.
summary(df)

## Deleting the columns that wont be utilized during the analysis.
df = subset(df, select = -c(seq_id,description, search_disposition, search_outcome, search_reason_for_stop, search_type, search_arrest_reason, contributed_to_accident, geolocation))

## Converting date variables to correct format for analysis with Rstudio.
df$date_of_stop <- mdy(df$date_of_stop)

## Extracting the year from the date variable for each row and inserting into a new column named year.
df$year_of_stop <- as.numeric(format(df$date_of_stop, "%Y"))

## Extracting the month from the date variable and inserting into a new column.
df$month_of_stop <- months(as.Date(df$date_of_stop))

## Creating a new variable with month and year of the stop combined.
df$Year_Month_of_stop <- paste(df$month_of_stop, df$year_of_stop)

## Extracting the day information from date variable.
df$day <- weekdays(as.Date(df$date_of_stop))

## Creating a new column with type of the day information.
df$day_type <- ifelse(df$day %in% c('Sunday', 'Saturday'), 'Weekend', 'Weekday')

##Extracting the hour of stop from time variable.
df$hourofday <- as.numeric(substr(df$time_of_stop, 1, 2))

##Creating a new variable by converting hour of the day variable into time of the day string variable.
df$timeofday <- dplyr::case_when(df$hourofday > 6 & df$hourofday < 12 ~ 'morning', 
                 df$hourofday >= 12 & df$hourofday < 18 ~ 'afternoon', 
                 df$hourofday >= 18 & df$hourofday <= 22 ~ 'evening', 
                 TRUE ~ 'night')

## identifying number of NA's contained in columns. 
df %>% summarise_all(funs(sum(is.na(.))))

## Further decreasing the dataframe size by eliminating columns that wont be utilized in the next phase of operations.
df = subset(df, select = c(date_of_stop,year_of_stop, month_of_stop, Year_Month_of_stop, day, day_type, timeofday, accident, personal_injury, property_damage, fatal, alcohol, vehicle_type, year, color,violation_type, race, gender, dl_state))

## Checking if the dataframe still contains variables with NA values. 
df %>% summarise_all(funs(sum(is.na(.))))

## Deleting the rows containing NA variables.
df <- na.omit(df)

##Dropping rows from the current year, 2024, for data completeness purposes.
df <- subset(df, year_of_stop!="2024")

##Dropping rows for accident records from the dataset.
df <- subset(df, accident!="Yes")

## Creating a new variable for grouping pullover outcomes
df$Outcome <- dplyr::case_when(df$violation_type == 'Warning' ~ 'Warning', 
                              TRUE ~ 'Citation')

## Tabulations and Visualizations for exploratory analysis.

### Date and Time variables exploratory analysis.

## Creating dataframe for total number of pullovers by date for line graph (Distribution of occurrences by year).
count_by_year <- df %>% 
  group_by(year_of_stop) %>% 
  summarise(NoP = n())

plot(count_by_year$year_of_stop, count_by_year$NoP, type = "b", main = "Total Number of Pullovers by Years.", xlab = "Year", ylab="Number of Pullovers")

## Creating dataframe for total number of pullovers by days of the week.
count_by_DoW <- df %>% 
  group_by(day) %>% 
  summarise(NoP = n())

## Reordering the days of the weeks table for the visualization.
count_by_DoW <- count_by_DoW %>% 
  mutate(day=factor(day, 
                         levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), 
                         ordered=T)) %>% arrange(day)

## Creating a bar chart for total number of pullovers by the day of the week (Distribution of occurrences by day of the week).
barplot(count_by_DoW$NoP, main="Total Number of Pullovers by Days of the Week",
        xlab="Days of the week", names.arg=count_by_DoW$day)

## Creating dataframe for average(Mean) number of pullovers by days of the week.
count_by_DoS_DoW <- df %>% 
  group_by(date_of_stop, day) %>% 
  summarise(Count = n())

m_Pullover_DoW <- count_by_DoS_DoW %>% group_by(day) %>% summarise(ave_pullover=mean(Count))

m_Pullover_DoW$ave_pullover <- round(m_Pullover_DoW$ave_pullover, digits = 0)

m_Pullover_DoW <- m_Pullover_DoW %>% 
  mutate(day=factor(day, 
                    levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), 
                    ordered=T)) %>% arrange(day)

##Creating a bar chart for average number of pullovers by day of the week.

barplot(m_Pullover_DoW$ave_pullover, main="Average Number of Pullovers by Days of the Week",
        xlab="Days of the week", names.arg=m_Pullover_DoW$day)

## Creating a dataframe for distribution of total number of pullovers in week days vs weekend days.
count_by_ToD <- df %>% 
  group_by(day_type) %>% 
  summarise(NoP = n())

##Creating a bar chart for visualizing the total number of pullovers by week days vs weekend days.
barplot(count_by_ToD$NoP, main="Total Number of Pullovers by Weekdays vs Weekend Days",
        xlab="Weekdays vs Weekend Days", names.arg=count_by_ToD$day_type)

## Creating a dataframe for calculating average(mean) number of pullovers in week days vs weekend days.

count_by_DoS_ToD <- df %>% 
  group_by(date_of_stop, day_type) %>% 
  summarise(Count = n())

m_Pullover_ToD <- count_by_DoS_ToD %>% group_by(day_type) %>% summarise(ave_pullover=mean(Count))

m_Pullover_ToD$ave_pullover <- round(m_Pullover_ToD$ave_pullover, digits = 0)

## Creating a dataframe with sum of all pullovers by months.

count_by_MoY <- df %>% 
  group_by(month_of_stop) %>% 
  summarise(NoP = n())

count_by_MoY <- count_by_MoY %>% 
  mutate(month_of_stop=factor(month_of_stop, 
                    levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"), 
                    ordered=T)) %>%   arrange(month_of_stop)

##Creating a bar chart for visualizing the total number of pullovers by months.
barplot(count_by_MoY$NoP, main="Total Number of Pullovers by Months",
        xlab="Months", names.arg=count_by_MoY$month_of_stop)

## Creating a dataframe with average(mean) number of pullovers for each month of a year.
count_by_DoS_MoY <- df %>% 
  group_by(year_of_stop, month_of_stop) %>% 
  summarise(Count = n())

m_Pullover_MoY <- count_by_DoS_MoY %>% group_by(month_of_stop) %>% summarise(ave_pullover=mean(Count))

m_Pullover_MoY$ave_pullover <- round(m_Pullover_MoY$ave_pullover, digits = 0)

m_Pullover_MoY <- m_Pullover_MoY %>% 
  mutate(month_of_stop=factor(month_of_stop, 
                              levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"), 
                              ordered=T)) %>%   arrange(month_of_stop)

##Creating a bar chart for visualizing the average(mean) number of pullovers by months.
barplot(m_Pullover_MoY$ave_pullover, main="Average Number of Pullovers by Months",
        xlab="Months", names.arg=m_Pullover_MoY$month_of_stop)

## Creating a dataframe for calculating total number of pullovers by time of the day.

count_by_TioD <- df %>% 
  group_by(timeofday) %>% 
  summarise(NoP = n())

count_by_TioD <- count_by_TioD %>% 
  mutate(timeofday=factor(timeofday, 
                              levels=c("morning", "afternoon", "evening", "night"), 
                              ordered=T)) %>%   arrange(timeofday)



## Creating a dataframe with average(mean) number of pullovers for each time of the day.
count_by_DoS_TioD <- df %>% 
  group_by(date_of_stop, timeofday) %>% 
  summarise(Count = n())

m_Pullover_TioD <- count_by_DoS_TioD %>% group_by(timeofday) %>% summarise(ave_pullover=mean(Count))

m_Pullover_TioD$ave_pullover <- round(m_Pullover_TioD$ave_pullover, digits = 0)

m_Pullover_TioD <- m_Pullover_TioD %>% 
  mutate(timeofday=factor(timeofday, 
                          levels=c("morning", "afternoon", "evening", "night"), 
                          ordered=T)) %>%   arrange(timeofday)

## Creating a Bar plot for visualizing the average number of pullovers by Time of the Day
barplot(m_Pullover_TioD$ave_pullover, main="Average Number of Pullovers by Time of the Day",
        xlab="Time of the Day", names.arg=m_Pullover_TioD$timeofday)

## Driver attributes perspective exploratory analysis.

## Creating a dataframe with total number of pullovers and relative frequencies by driver ethnicity
count_by_DrEth <- df %>% 
  group_by(race) %>% 
  summarise(NoP = n())

##Adding 2022 census demographic distributions for Montgomery Country, Maryland

count_by_DrEth$census22 <- dplyr::case_when(count_by_DrEth$race == "ASIAN"  ~ 0.16, 
                                            count_by_DrEth$race == "WHITE"  ~ 0.41, 
                                            count_by_DrEth$race == "HISPANIC"  ~ 0.20,
                                            count_by_DrEth$race == "NATIVE AMERICAN"  ~ 0.009,
                                            count_by_DrEth$race == "BLACK"  ~ 0.20,
                                            count_by_DrEth$race == "OTHER"  ~ 0.021,
                                            TRUE ~ 0)
## Counts by driver ethnicity and outcome.
count_by_DrEth_Out <- df %>% 
  group_by(race, Outcome) %>% 
  summarise(Count = n())

##Calculating relative frequencies of pullover outcomes based on ethnic backround.
rfreg_count_by_DrEth_Out <- df %>%
  group_by(race, Outcome) %>%
  summarise(Count = n()) %>%
  mutate(freq = Count / sum(Count))

## Adding relative frequency of pullovers by driver ethnicity.
count_by_DrEth$Percentage <-  count_by_DrEth$NoP / sum(count_by_DrEth$NoP)

count_by_DrEth$Percentage <- round(count_by_DrEth$Percentage, digits = 3)

## Creating a dataframe with total number of pullovers and relative frequencies by driver gender.
count_by_DrGen <- df %>% 
  group_by(gender) %>% 
  summarise(NoP = n())

##Calculating relative frequencies of pullover outcomes based on gender.
rfreg_count_by_DrGen_Out <- df %>%
  group_by(gender, Outcome) %>%
  summarise(Count = n()) %>%
  mutate(freq = Count / sum(Count))

## Adding relative frequency of pullovers by driver gender.
count_by_DrGen$Percentage <-  count_by_DrGen$NoP / sum(count_by_DrGen$NoP)

## Creating a dataframe with total number of pullovers and relative frequencies by driver origin.
df$Dr_Cat <- dplyr::case_when(df$dl_state == 'MD' ~ 'Resident', 
                                   TRUE ~ 'Visitor')
## Creating a dataframe with total number of pullovers and relative frequencies by driver origin category.
count_by_Dr_Cat <- df %>% 
  group_by(Dr_Cat) %>% 
  summarise(NoP = n())


## Exploratory analysis by Car Attributes perspective.

## Creating a dataframe with total number of pullovers by car color.
count_by_CarColor <- df %>% 
  group_by(color) %>% 
  summarise(NoP = n())

## Cleaning remaining null and N/A variables in Color Categories
count_by_CarColor$color <- dplyr::case_when(count_by_CarColor$color == "N/A" ~ 'Other', 
                                            count_by_CarColor$color == "" ~ 'Other', 
                                            TRUE ~ count_by_CarColor$color)

##Adding relative frequencies of pullovers by car color to dataframe
count_by_CarColor$Percentage <-  count_by_CarColor$NoP / sum(count_by_CarColor$NoP)

## Crating a table for top 10 most pulled over car colors.
count_by_CarColort10 <- count_by_CarColor %>%
  arrange(desc(NoP)) %>%
  slice(1:10) 

count_by_CarColort10$Percentage <- round(count_by_CarColort10$Percentage, digits = 3)

## Ordering values in dataframe.

count_by_CarColort10 <- count_by_CarColort10[order(count_by_CarColort10$Percentage, decreasing = TRUE),]

## Creating a dataframe with total number of pullovers by car age.

## Calculating Car Age at the time of pullover
df$car_age <- df$year_of_stop - df$year

## Creating a new category for car age categories.

df$car_age_cat <- dplyr::case_when(df$car_age > -1 & df$car_age < 3 ~ '0 to 3', 
                                 df$car_age >= 3 & df$car_age < 8 ~ '3 to 8', 
                                 df$car_age >= 8 & df$car_age <= 15 ~ '8 to 15', 
                                 TRUE ~ '15+')
## Creating a dataframe with total number of pullovers by Car Age Category.
count_by_car_age_cat <- df %>% 
  group_by(car_age_cat) %>% 
  summarise(NoP = n())

## adding relative frequencies to the table of pullovers by car age category.

count_by_car_age_cat$share <- count_by_car_age_cat$NoP / sum(count_by_car_age_cat$NoP)

count_by_car_age_cat <- count_by_car_age_cat %>% 
  mutate(car_age_cat=factor(car_age_cat, 
                            levels=c("0 to 3", "3 to 8", "8 to 15", "15+"), 
                            ordered=T)) %>%   arrange(car_age_cat)

count_by_car_age_cat$share <- round(count_by_car_age_cat$share, digits = 3)

## Calculating share of outcomes by car age category. 
rfreg_count_by_Carage_Out <- df %>%
  group_by(car_age_cat, Outcome) %>%
  summarise(Count = n()) %>%
  mutate(freq = Count / sum(Count))

rfreg_count_by_Carage_Out <- rfreg_count_by_Carage_Out %>% 
  mutate(car_age_cat=factor(car_age_cat, 
                          levels=c("0 to 3", "3 to 8", "8 to 15", "15+"), 
                          ordered=T)) %>%   arrange(car_age_cat)

##Pullover Outcome Counts and Frequencies

count_by_out <- df %>% 
  group_by(Outcome) %>% 
  summarise(NoP = n())

count_by_out$freq <- count_by_out$NoP / sum(count_by_out$NoP)

### Visualizations for PPT.

## Number of Pullovers by Year.
plot1<-ggplot(count_by_year,
             aes(year_of_stop,NoP,fill=factor(ifelse(year_of_stop >="2020","Highlighted","Normal")))) + 
  geom_bar(stat = "identity", show.legend = FALSE)+
  geom_text(aes(label = signif(NoP)), nudge_y = 20000)
plot1
plot1+ ggtitle("Total Number of Pullovers by Years between 2012 and 2023") + xlab("Years") + ylab("Total Number of Pullovers")

##Average number of pullovers by time of the day.
plot2<-ggplot(m_Pullover_TioD,
             aes(timeofday,ave_pullover,fill=factor(ifelse(timeofday=="night","Highlighted","Normal")))) + 
  geom_bar(stat = "identity", show.legend = FALSE)+
  geom_text(aes(label = signif(ave_pullover)), nudge_y = 10)
plot2

plot2+ ggtitle("Average Number of Pullovers by Time of the Day") + xlab("Average Number of Pullovers") + ylab("Time of the Day")

## Average number of pullovers by day of the week.
plot3<-ggplot(m_Pullover_DoW,
             aes(day,ave_pullover,fill=factor(ifelse(day=="Tuesday","Highlighted","Normal")))) + 
  geom_bar(stat = "identity", show.legend = FALSE)+
  geom_text(aes(label = signif(ave_pullover)), nudge_y = 30)
plot3

plot3+ ggtitle("Average Number of Pullovers by Day of the Week") + xlab("Day of the Week") + ylab("Average Number of Pullovers")

## Average number of pullovers by type of Weekday.
plot4<-ggplot(m_Pullover_ToD,
             aes(day_type,ave_pullover,fill=factor(ifelse(day_type=="Weekday","Highlighted","Normal")))) + 
  geom_bar(stat = "identity", show.legend = FALSE, color='black',width=0.9)+
  geom_text(aes(label = signif(ave_pullover)), nudge_y = 30)
plot4

plot4+ ggtitle("Average Number of Pullovers by Weekend vs Week Days") + xlab("Day type") + ylab("Average Number of Pullovers")


## Average number of pullovers by months.
plot5<-ggplot(m_Pullover_MoY,
             aes(month_of_stop,ave_pullover,fill=factor(ifelse(month_of_stop=="March","Highlighted","Normal")))) + 
  geom_bar(stat = "identity", show.legend = FALSE, color='black',width=0.9)+
  geom_text(aes(label = signif(ave_pullover)), nudge_y = 250)
plot5

plot5+ ggtitle("Average Number of Pullovers by Months") + xlab("Months") + ylab("Average Number of Pullovers")

## Stacked Bar Charts 

## Stacked bar chart for pullover outcome vs race frequencies.
plot6 <- ggplot(rfreg_count_by_DrEth_Out, aes(fill=Outcome, y=Count, x=race)) + 
  geom_bar(position="fill", stat="identity", color='black',width=0.9)
plot6+ ggtitle("Frequency of Citations vs Warnings by Ethnic Background") + xlab("Ethnic Background") + ylab("Share of Outcomes")

##Stacked bar chart of pullover outcome vs gender.
plot7 <- ggplot(rfreg_count_by_DrGen_Out, aes(fill=Outcome, y=Count, x=gender)) + 
  geom_bar(position="fill", stat="identity", color='black',width=0.9)
plot7+ ggtitle("Frequency of Citations vs Warnings by Gender") + xlab("Gender") + ylab("Share of Outcomes")

##Stacked bar chart of pullover outcome vs Car Age Cat..
plot8 <- ggplot(rfreg_count_by_Carage_Out, aes(fill=Outcome, y=Count, x=car_age_cat)) + 
  geom_bar(position="fill", stat="identity", color='black',width=0.9)
plot8+ ggtitle("Frequency of Citations vs Warnings by Car Age Category") + xlab("Car Age Category") + ylab("Share of Outcomes")

## Stacked bar chart of pullover outcome.

plot9 <- ggplot(count_by_out, aes(x = "", y = freq, fill = Outcome)) +
  geom_col() + geom_bar(position="fill", stat="identity", color='black',width=0.9)
plot9+ ggtitle("Frequency of Citations vs Warnings") + xlab("All Recorded Pullovers") + ylab("Share of Outcomes")

## Pie chart of Pullovers by Driver Ethnicity vs Census 22 Demographic Frequencies.
## Pie chart of Pullovers by Driver Ethnicity
plot10 <- ggplot(count_by_DrEth, aes(x = "", y = Percentage, fill = race)) +
  geom_col(color = "black") +
  geom_text(aes(label = Percentage),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")
plot10
plot10+ ggtitle("Distribution of Drivers' Ethnic Backgrounds") + xlab("") + ylab("All Pulled Over Drivers")

##Pie chart Census 22 demographic distribution
plot11 <- ggplot(count_by_DrEth, aes(x = "", y = census22, fill = race)) +
  geom_col(color = "black") +
  geom_text(aes(label = census22),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")
plot11

plot11+ ggtitle("Distribution of Ethnicities in Population") + xlab("") + ylab("2022 Census Demographics")

## Visualizations for Car Attributes 
## Bar chart of share of pullovers by Top 10 Car Colors.

plot12<-ggplot(count_by_CarColort10,
              aes(color,Percentage,fill=factor(ifelse(color=="BLACK","Highlighted","Normal")))) + 
  geom_bar(stat = "identity", show.legend = FALSE, color="black",width=0.9)+
  geom_text(aes(label = signif(Percentage)), nudge_y = .01)
plot12

plot12+ ggtitle("Share of Pullovers by Top 10 Car Colors") + xlab("Car Colors") + ylab("Share of Pullovers")

## Bar chart of share of pullovers by Car Age Categories.
plot13<-ggplot(count_by_car_age_cat,
               aes(car_age_cat,share,fill=factor(ifelse(car_age_cat=="8 to 15","Highlighted","Normal")))) + 
  geom_bar(stat = "identity", show.legend = FALSE, color="black",width=0.9)+
  geom_text(aes(label = signif(share)), nudge_y = .02)
plot13

plot13+ ggtitle("Share of Pullovers by Car Age Category") + xlab("Car Age Categories") + ylab("Share of Pullovers")

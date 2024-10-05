#install.packages(c("lubridate", "dplyr", "ggplot2"))

library(lubridate)
library(dplyr)
library(ggplot2)

weather <- read.csv("/cloud/project/campus_weather.csv",
                    na.strings="#N/A")               

#add a new column
weather$dateF <- mdy_hm(weather$Date)

#create a new function

interval <- weather$dateF[-length(weather$dateF)] %--% weather$dateF[-1]
interval

#set up time intervals in a vector of dates

timeInterval <- function(x){
  x[-length(x)] %--% x[-1]
}

timeInterval(weather$dateF)

#for loops 
#i can be any letter or name

for(i in 1:6){
  print(paste("example", i))
}

seqEx <- c(1,4,6)
for(i in seqEx){
  print(paste("example", i))
}

chEx <- character()
for(i in 1:6){
  chEx[i] <- paste("example", i)
}

numEx <- numeric()
for(i in 1:6){
  numEx[i] <- 6*i
}

numEx2 <- numeric()
for(i in 2:6){
  numEx2[i] <- 6*i
}

#In class prompts:
#Prompt 1: Calculate a rolling average of air temperatures over eight 15 min 
#measurements (2 hours) for January of 2022 using a for loop. Make a plot of
#the 15 minute air temperature and the rolling average

#isolate january 2022

weather$month <- month(weather$dateF)
weather$year <- year(weather$dateF)

jan22 <- weather %>%
  filter(month==1 & year == 2022)

mean(jan22$AirTemp[1:8])

rollAveTemp <- numeric()
for(i in 8:nrow(jan22)){
  rollAveTemp[i] <- mean(jan22$AirTemp[(i-7):i])
}

jan22$rollAveTemp <- rollAveTemp

#prompt 2: You want to see if the solar radiation measurements experienced any 
#issues with build up or accumulation on the sensor in May and June of 2021. 
#Make an assessment with your group.

mayjune21 <- weather %>%
  filter(month==5 & year==2021 | month==6 & year==2021) 

ggplot(data=mayjune21, aes(x=dateF, y=SolRad))+
         geom_col(color="royalblue4")+
         theme_classic()
#though there are a few days where the solar radiation is very low, it 
#doesn't seem to be a consistent problem.

#prompt 3: Check for any date time issues using the function created in the
#tutorial. Investigate instances of date time issues. What happens around 
#daylight savings? Are there issues with the time zone assumption?


#check intervals
int_length(weather$dateF[1] %--% weather$dateF[2])

interval_times <- int_length(interval)

interval[interval_times !=900]

#daylight savings: second sunday in march, first sunday in november: this is 
#where the interval problems start and end

#create function for checking irregular intervals that deviate from 900 seconds

timecheck900 <- function(x){
  intervals <- x[-length(x)] %--% x[-1]
  interval_times <- int_length(intervals)
  intervals[interval_times !=900]
}

timecheck900(weather$dateF)

#time zone is assumed to be UTC which is not correct

#Homework 4

#Question 1: 











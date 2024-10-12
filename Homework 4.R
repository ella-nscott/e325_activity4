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

nrow(jan22)

jan22$rollAveTemp <- rollAveTemp

#plot air temp and rolling average
ggplot(data=jan22, aes(x=dateF, y=AirTemp))+
  geom_col(color="royalblue4")+
  theme_classic()

ggplot(data=jan22, aes(x=dateF, y=rollAveTemp))+
  geom_col(color="royalblue4")+
  theme_classic()

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

#Question 1: Exclude any precipitation that occurs when the air temperature 
#is below zero. Check that no precipitation measurements are used if the X and 
#Y level observations are more than 2 degrees. Find amount of missing 
#precipitation data. 

weather$absXLevel <- abs(weather$XLevel)
weather$absYLevel <- abs(weather$YLevel)

weather$precip.QC <- ifelse(weather$AirTemp < 0 | weather$absXLevel > 2 | weather$absYLevel > 2,
                            NA, weather$Precip)

sum(is.na(weather$precip.QC))

#double check number of NA values
test <- weather %>%
  filter(is.na(precip.QC))

#Question 2: Create a data flag that warns a user if the battery voltage falls 
#below 8.5 Volts

weather$BatteryFlag <- ifelse(weather$BatVolt < 8500,
                              1, 0)

#Question 3: Create a function that checks for observations that are in 
#unrealistic data ranges in air temperature and solar radiation.

# Sensor Temp range: -50 to 60 C, (unlikely to be higher than 38 C, lower than -34 C)
# Sensor Solar radiation range: 0–1750 W/m2

checkRange <- function(x){
    Check <- ifelse (x$AirTemp < -34 | x$AirTemp > 38 | x$SolRad < 0 | x$SolRad > 1750,
                       1,0) 
}

mar_april21$QC.Range <- checkRange(mar_april21)


#Question 4: Make a plot of winter air temperatures in Jan - Mar of 2021. 
#Check for persistence issues that might indicate snow accumulation on the sensor

jan_mar_21 <- weather %>%
  filter(month==1 & year==2021 | month==2 & year==2021 | month==3 & year==2021)

ggplot(data=jan_mar_21, aes(x=dateF, y=AirTemp))+
  geom_col(color="royalblue4")+
  theme_classic()+
  labs(x="Date", y="Air Temperature (Celsius)")

#Question 5: You are asked for total daily precipitation in March and April of 
#2021. Use a for loop to exclude (convert to NA) any days that include 
#temperatures less than 35 degrees F on that day or the day prior to ensure that 
#measurements are not likely to be affected by snow accumulation on the sensor.

#C to F 
(35-32)*(5/9)

#isolate march and april 2021
mar_april21 <- weather %>%
  filter(month==3 & year==2021 | month==4 & year==2021)

#create data frame with lowest temp for each day

mar_april21$Day <- yday(mar_april21$dateF)

daily_min_temp <- mar_april21 %>%
  group_by(Day) %>%
  summarise(min_temp = min(AirTemp))

#filter using for loop

Precip.QC.2 <- numeric()
for(i in 2:nrow(daily_min_temp)){
                   Precip.QC.2[i] <- ifelse(daily_min_temp$min_temp[i] < 1.67|daily_min_temp$min_temp[i-1] < 1.67,
                       1, 0)
}

daily_min_temp$Precip.QC.2 <- Precip.QC.2

#apply filter to data frame

QC_mar_april_21 <- full_join(daily_min_temp, mar_april21)

QC_mar_april_21$Precip.QC.2 <- ifelse(QC_mar_april_21$Precip.QC.2 == 1,
                          NA, QC_mar_april_21$Precip)

#Find number of daily observations
sum(is.na(QC_mar_april_21$Precip.QC.2))

#subtract no. NA from no. obs 
5852 - 4124

              #..........................Coursera..........................#
            #.......................Course Project 1.........................#
         #.......................Exploratory Data Analysis.......................#

setwd(choose.dir())
getwd()

household_power <- read.table("household_power_consumption.txt", header=TRUE, sep=";", 
                              na.strings = "?", colClasses = c('character','character','numeric','numeric','numeric','numeric','numeric','numeric','numeric'))
attach(household_power)

## Input: 
#....Base data

## Output:
#....Phases: Database cleaning

#.....# Format Date
#.....# Our overall goal here is simply to examine how household energy usage varies over a 2-day period in February, 2007
#.....# Removing missing values
#.....# Joining the date and time column
#.....# Name the vector
#.....# Remove Date and Time column
#.....# Add DateTime column
#.....# Format dateTime Column

Format_date <- function(base){
  
household_power$Date <- as.Date(household_power$Date, "%d/%m/%Y")

household_power <- subset(household_power,Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2"))

household_power <- household_power[complete.cases(household_power),]

dateTime <- paste(household_power$Date, household_power$Time)

dateTime <- setNames(dateTime, "DateTime")

household_power <- household_power[ ,!(names(household_power) %in% c("Date","Time"))]

household_power <- cbind(dateTime, household_power)

household_power$dateTime <- as.POSIXct(dateTime)

return(household_power)

}



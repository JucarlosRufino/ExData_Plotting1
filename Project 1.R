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

household_power <- Format_date(household_power)

# Create graph 

## Plote 1: Histogram

par(family = "serif")

hist(household_power$Global_active_power, main="Global Active Power", 
     xlab = "Global Active Power (kilowatts)", col="red")

## Plot 2: Global Active Power (kilowatts)

plot(household_power$Global_active_power~household_power$dateTime, type="l",
     ylab="Global Active Power (kilowatts)", xlab="")

## Plot 3: by grupous

with(household_power, {
  plot(Sub_metering_1~dateTime, type="l",
       ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub_metering_2~dateTime,col='Red')
  lines(Sub_metering_3~dateTime,col='Blue')
})
legend("topright", col=c("black", "red", "blue"), lwd=c(1,1,1), 
       c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), cex = 0.8)

## Plot 4: Global Active Power (kilowatts) e Voltage (volt)

par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
with(household_power, {
  plot(Global_active_power~dateTime, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
  plot(Voltage~dateTime, type="l", 
       ylab="Voltage (volt)", xlab="")
  plot(Sub_metering_1~dateTime, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub_metering_2~dateTime,col='Red')
  lines(Sub_metering_3~dateTime,col='Blue')
  legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n",
         legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), cex = 0.5)
  plot(Global_reactive_power~dateTime, type="l", 
       ylab="Global Rective Power (kilowatts)",xlab="")
})


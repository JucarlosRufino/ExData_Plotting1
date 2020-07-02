            #..........................Coursera..........................#
          #.......................Course Project 1.........................#
       #.......................Exploratory Data Analysis.......................#

# Create graph 

household_power <- Format_date(household_power)

## Plote 1: Histogram

par(family = "serif")

hist(household_power$Global_active_power, main="Global Active Power", 
     xlab = "Global Active Power (kilowatts)", col="red")

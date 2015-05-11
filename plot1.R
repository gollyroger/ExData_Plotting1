plot1 <- function() {
  library(dplyr)
  housepower <- read.table("exdata-data-household_power_consumption/household_power_consumption.txt", header = TRUE, sep = ";")
  housepower<-mutate(housepower, Date = as.Date(strptime(housepower[,1], "%d/%m/%Y")))
  housepower <- subset(housepower, Date >= "2007-2-1" & Date <= "2007-2-2")
  housegap <- subset(housepower, Global_active_power != "?")
  housegap <- as.numeric(as.character(housegap$Global_active_power))
  png("plot1.png", width=480, height=480, units="px")
  hist(housegap, plot = TRUE, main = "Global Active Power", xlab = "Global Active Power (kilowatts)", col = "red")
  dev.off() 
}


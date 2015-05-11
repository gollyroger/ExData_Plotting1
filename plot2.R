plot2 <- function() {
  library(dplyr)
  housepower <- read.table("exdata-data-household_power_consumption/household_power_consumption.txt", header = TRUE, sep = ";")
  housepower <- subset(housepower, Global_active_power != "?")
  housepower<-mutate(housepower, Date = as.Date(strptime(housepower[,1], "%d/%m/%Y")))
  housepower <- subset(housepower, Date >= "2007-2-1" & Date <= "2007-2-2")
  housepower$datetime <- as.POSIXct(paste(housepower$Date, housepower$Time), format="%Y-%m-%d %H:%M:%S")
  housepower<-mutate(housepower, Global_active_power = as.numeric(as.character(housepower$Global_active_power))) 

  
  png("plot2.png", width=480, height=480, units="px")
  plot(housepower$datetime, housepower$Global_active_power, type = "n", ylab ="Global Active Power (kilowatts)", xlab = NA)
  lines(housepower$datetime, housepower$Global_active_power, type = "l")
  dev.off() 
}


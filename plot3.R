plot3 <- function() {
  library(dplyr)
  housepower <- read.table("exdata-data-household_power_consumption/household_power_consumption.txt", header = TRUE, sep = ";")
  housepower <- subset(housepower, Sub_metering_1 != "?" & Sub_metering_2 != "?" & Sub_metering_3 != "?")

  housepower<-mutate(housepower, Date = as.Date(strptime(housepower[,1], "%d/%m/%Y")))
  housepower <- subset(housepower, Date >= "2007-2-1" & Date <= "2007-2-2")
  housepower$datetime <- as.POSIXct(paste(housepower$Date, housepower$Time), format="%Y-%m-%d %H:%M:%S")

  housepower<-mutate(housepower, Sub_metering_1  = as.numeric(as.character(housepower$Sub_metering_1))) 
  housepower<-mutate(housepower, Sub_metering_2  = as.numeric(as.character(housepower$Sub_metering_2))) 
  housepower<-mutate(housepower, Sub_metering_3  = as.numeric(as.character(housepower$Sub_metering_3))) 

  yaxislimit <- max(max(housepower$Sub_metering_1),max(housepower$Sub_metering_2),max(housepower$Sub_metering_3))
  
  png("plot3.png", width=480, height=480, units="px")
  plot(housepower$datetime, housepower$Sub_metering_1, type = "n", ylab ="Energy sub metering", xlab = NA, ylim = c(0,yaxislimit))
  lines(housepower$datetime, housepower$Sub_metering_1, type = "l", col = "black")
  lines(housepower$datetime, housepower$Sub_metering_2, type = "l", col = "red")
  lines(housepower$datetime, housepower$Sub_metering_3, type = "l", col = "blue")
  legend("topright", legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), lty=c(1,1), col = c("black","red","blue"))
  dev.off() 
}


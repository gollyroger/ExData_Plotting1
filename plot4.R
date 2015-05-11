plot4 <- function() {
  library(dplyr)
  housepower <- read.table("exdata-data-household_power_consumption/household_power_consumption.txt", header = TRUE, sep = ";")

  housepower<-mutate(housepower, Date = as.Date(strptime(housepower[,1], "%d/%m/%Y")))
  housepower <- subset(housepower, Date >= "2007-2-1" & Date <= "2007-2-2")
  housepower$datetime <- as.POSIXct(paste(housepower$Date, housepower$Time), format="%Y-%m-%d %H:%M:%S")

  ##Creating global active power plot data
  housegap <- subset(housepower, Global_active_power != "?")
  housegap<-mutate(housegap, Global_active_power = as.numeric(as.character(housegap$Global_active_power))) 

  ##Creating sub metering plot data
  housemeter <- subset(housepower, Sub_metering_1 != "?" & Sub_metering_2 != "?" & Sub_metering_3 != "?")
  housemeter<-mutate(housemeter, Sub_metering_1  = as.numeric(as.character(housemeter$Sub_metering_1))) 
  housemeter<-mutate(housemeter, Sub_metering_2  = as.numeric(as.character(housemeter$Sub_metering_2))) 
  housemeter<-mutate(housemeter, Sub_metering_3  = as.numeric(as.character(housemeter$Sub_metering_3))) 
  yaxislimit <- max(max(housemeter$Sub_metering_1),max(housemeter$Sub_metering_2),max(housemeter$Sub_metering_3))
  
  ##Creating voltage plot data
  housevolt <- subset(housepower, Voltage != "?")
  housevolt<-mutate(housevolt, Voltage = as.numeric(as.character(housevolt$Voltage))) 
 
  ##Creating global reactive power plot data
  housegrp <- subset(housepower, Global_reactive_power != "?")
  housegrp<-mutate(housegrp, Global_reactive_power = as.numeric(as.character(housegrp$Global_reactive_power))) 
  
  
  png("plot4.png", width=480, height=480, units="px")
  par(mfrow = c(2,2))
  
  ##Creating gap plot
  plot(housegap$datetime, housegap$Global_active_power, type = "l", ylab ="Global Active Power (kilowatts)", xlab = NA)
  
  ##Creating volt plot
  plot(housevolt$datetime, housevolt$Voltage, type = "l", ylab ="Voltage", xlab = "datetime")
  
  ##Creating meter plot
  plot(housemeter$datetime, housemeter$Sub_metering_1, type = "n", ylab ="Energy sub metering", xlab = NA, ylim = c(0,yaxislimit))
  lines(housemeter$datetime, housemeter$Sub_metering_1, type = "l", col = "black")
  lines(housemeter$datetime, housemeter$Sub_metering_2, type = "l", col = "red")
  lines(housemeter$datetime, housemeter$Sub_metering_3, type = "l", col = "blue")
  legend("topright", legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), lty=c(1,1), col = c("black","red","blue"), bty = "n")

  ##Creating grp plot
  plot(housegrp$datetime, housegrp$Global_reactive_power, type = "l", ylab ="Global_reactive_power", xlab = "datetime")
  
  dev.off() 
}


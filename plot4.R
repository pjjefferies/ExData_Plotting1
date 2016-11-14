readAndCleanElectPowerData <- function(inputCSVFile, minDate, maxDate) {
  time1 <- Sys.time()
  elecPowerData <- read.csv(inputCSVFile,
                            sep=";",
                            header=TRUE,
                            colClasses = c("character", "character",
                                           "numeric", "numeric",
                                           "numeric", "numeric",
                                           "numeric", "numeric",
                                           "numeric"),
                            comment.char="",
                            na.string = c("?")
                            #, nrows = 500     #temporary for debug
                            )
  
  #create combined datetime
  elecPowerData$dateTime <- strptime(paste(elecPowerData$Date,
                                           elecPowerData$Time,
                                           sep=" "),
                                     format = "%d/%m/%Y %H:%M:%S")

  #remove original Date and Time fields
  elecPowerData <- within(elecPowerData, rm(Date, Time))
  
  #filter out dates outside of range
  elecPowerData <- elecPowerData[elecPowerData$dateTime >= minDate, ]
  elecPowerData <- elecPowerData[elecPowerData$dateTime <= maxDate, ]

  #filter-out NA
  elecPowerData <- na.omit(elecPowerData)
}


#Input filestring
inputFile <- "household_power_consumption.txt"

#Use data from 2007-02-01 to 2007-02-02
beginDate <- strptime("2007-02-01 00:00:00",
                      format = "%Y-%m-%d %H:%M:%S")
endDate   <- strptime("2007-02-03 00:00:00",
                      format = "%Y-%m-%d %H:%M:%S")

#Call function to read data, clean and return date between to dates
elecPow <- readAndCleanElectPowerData(inputFile, beginDate, endDate)

#Generate Plot to png file
png(file="plot4.png", width = 480, height = 480)
par(mfrow=c(2,2))
plot(elecPow$dateTime,
     elecPow$Global_active_power,
     col="black", type="l",
     ylab="Global Active Power",
     xlab="",
     cex.axis=1,
     cex.lab=1,
     lwd=1)
plot(elecPow$dateTime,
     elecPow$Voltage,
     col="black", type="l",
     xlab="datetime",
     ylab="Voltage",
     cex.axis=1,
     cex.lab=1,
     lwd=1)
plot(elecPow$dateTime,
     elecPow$Sub_metering_1,
     col="black", type="l",
     ylab="Energy sub metering",
     xlab="",
     cex.axis=1,
     cex.lab=1,
     lwd=1)
lines(elecPow$dateTime,
      elecPow$Sub_metering_2,
      type="l",
      lty=1,
      lwd=1,
      col="red")
lines(elecPow$dateTime,
      elecPow$Sub_metering_3,
      type="l",
      lty=1,
      lwd=1,
      col="blue")
legend("topright", c("Sub_metering_1", "Sub_metering_2",
                     "Sub_metering_3"),
       cex=1,
       col=c("black", "red", "blue"),
       lty=1,
       lwd=1,
       bty="o")
plot(elecPow$dateTime,
     elecPow$Global_reactive_power,
     col="black", type="l",
     ylab="Global_reactive_power",
     xlab="datetime",
     cex.axis=1,
     cex.lab=1,
     lwd=1)
dev.off()
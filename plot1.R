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
endDate   <- strptime("2007-02-02 23:59:59",
                      format = "%Y-%m-%d %H:%M:%S")

#Call function to read data, clean and return date between to dates
elecPow <- readAndCleanElectPowerData(inputFile, beginDate, endDate)

#Generate Plot to png file
png(file="plot1.png", width = 480, height = 480)
hist(elecPow$Global_active_power,
     col="red",
     breaks=12,
     main="Global Active Power",
     xlab="Global Active Power (kilowatts)")
dev.off()
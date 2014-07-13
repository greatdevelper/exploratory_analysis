dnldfile <- function(fileURL, fname) {
  if(!file.exists(fname)) {
    download.file(fileURL, destfile=fname, method="curl")
  }
  fname
}

prepareData <- function() {
  cacheFile <- "plot_data.csv"
  if(file.exists(cacheFile)) {
    tbl <- read.csv(cacheFile)
    tbl$DateTime <- strptime(tbl$DateTime, "%Y-%m-%d %H:%M:%S")
  }
  else {
    fname <- dnldfile("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", "household_power_consumption.zip")
    con <- unz(fname, "household_power_consumption.txt")
    tbl <- read.table(con, header=T, sep=';', na.strings="?", colClasses=c("character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
    #close(con)
    tbl <- tbl[(tbl$Date == "1/2/2007") | (tbl$Date == "2/2/2007"),]
    tbl$DateTime <- strptime(paste(tbl$Date, tbl$Time), "%d/%m/%Y %H:%M:%S")
    write.csv(tbl, cacheFile)
  }
  tbl
}

doPlot4 <- function() {
  tbl <- prepareData()
  png(filename = "plot4.png", width = 480, height = 480, units = "px")
  
  par (mfrow = c(2,2), mar = c(4,4,2,1), oma = c(0,0,2,0))
  with(tbl, {
    plot(DateTime, Global_active_power, xlab="", ylab="Global Active Power", type="l")
    plot(DateTime, Voltage, xlab="datetime", ylab="Voltage", type="l")
    
    cols = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
    plot(DateTime, Sub_metering_1, type="l", xlab="", ylab="Energy sub metering")
    lines(DateTime, Sub_metering_2, type="l", col="red")
    lines(DateTime, Sub_metering_3, type="l", col="blue")
    legend("topright", lty=1, lwd=1, col=c("black","blue","red"), legend=cols, bty="n")
    
    plot(DateTime, Global_reactive_power, xlab="datetime", ylab="Global_reactive_power", type="l")
  })
  dev.off()
}

doPlot4()
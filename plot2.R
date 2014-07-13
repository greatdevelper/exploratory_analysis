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

doPlot2 <- function() {
  tbl <- prepareData()
  png(filename = "plot2.png", width = 480, height = 480, units = "px")
  plot(tbl$DateTime, tbl$Global_active_power, type="l", xlab="", ylab="Global Active Power (kilowatts)")
  dev.off()
}

doPlot2()
## loads  Electric power consumption into R and supsets data
loadData <- function(){
        
        library(lubridate)
        library(dplyr)
        
        ## downloads and unzips file if not exists in the work directory
        if(!file.exists("household_power_consumption.txt")){
                fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
                
                download.file(url = fileUrl, 
                              destfile = "exdata_data_household_power_consumption.zip")
                
                unzip(zipfile = "exdata_data_household_power_consumption.zip")
                file.remove("exdata_data_household_power_consumption.zip")
                rm(fileUrl)
        }
        
        ##reads data into R
        hpc <- read.table(file = "data/household_power_consumption.txt", 
                          header = TRUE, sep = ";")
        
        
        
        
        ## adds new column with date and time using lubridate function 
        hpc <- mutate(.data = hpc, 
                      DateTime = dmy_hms(paste(hpc$Date, hpc$Time)), .after = "Time")
        
        ## You may find it useful to convert the Date and Time variables
        ## to Date/Time classes in R using the strptime()  and as.Date() functions.
        hpc$Date <- dmy(hpc$Date)
        hpc$Time <- hms(hpc$Time)
        
        
        ## We will only be using data from the dates 2007-02-01 and 2007-02-02. 
        ## One alternative is to read the data from just those dates 
        ## rather than reading in the entire dataset and subsetting to those dates.
        
        hpc <- subset(x = hpc, subset = 
                              hpc$Date == "2007-02-01" |
                              hpc$Date == "2007-02-02")
}

## saves jpeg plot4
plot4jpeg <- function(){
        png(filename = "plot4.png",width = 480,height = 480)
        
        par(mfrow=c(2,2),mar=c(4,4,1,2))
        
        plot(x = hpc$DateTime, y = hpc$Global_active_power,
             xlab = "",
             ylab = "Global Active Power (Kilowatts)", type = "l")
        
        plot(x = hpc$DateTime, y = hpc$Voltage, 
             type = "l", xlab = "datetime", ylab = "Voltage")
        
        plot(x = hpc$DateTime, y = hpc$Sub_metering_1,
                xlab = "", ylab = "Energy Sub Meetering", 
                type = "l", col = "black")
        
                lines(x = hpc$DateTime, y = hpc$Sub_metering_2, col= "red")
                lines(x = hpc$DateTime, y = hpc$Sub_metering_3, col= "blue")
        
                legend("topright", lty=1,lwd=2, col = c("black", "red", "blue"),
                        legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
        
        with(hpc, plot(x = DateTime, y = Global_reactive_power, 
                       xlab = "datetime", type = "l"))
        
        dev.off()
}

plot4view <- function(){
        par(mfrow=c(2,2),mar=c(4,4,1,2))
        
        plot(x = hpc$DateTime, y = hpc$Global_active_power,
             xlab = "",
             ylab = "Global Active Power (Kilowatts)", type = "l")
        
        plot(x = hpc$DateTime, y = hpc$Voltage, 
             type = "l", xlab = "datetime", ylab = "Voltage")
        
        plot(x = hpc$DateTime, y = hpc$Sub_metering_1,
             xlab = "", ylab = "Energy Sub Meetering", 
             type = "l", col = "black")
        
                lines(x = hpc$DateTime, y = hpc$Sub_metering_2, col= "red")
                lines(x = hpc$DateTime, y = hpc$Sub_metering_3, col= "blue")
        
                legend("topright", lty=1,lwd=2, col = c("black", "red", "blue"),
                        legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
        
        with(hpc, plot(x = DateTime, y = Global_reactive_power, 
                       xlab = "datetime", type = "l"))
}

## calls function that loads the data if not loaded already
if(!exists("hpc")){
        hpc <- loadData()
}

##view and save plot
plot4view()

plot4jpeg()
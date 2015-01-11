## 4-Quadrant Plot of Results
## ---------
## The following code enerates a 4-Quadrant plot of metering data from the UC Irvine Machine Learning Repository
## Quardant(1,1) - Global Active Power vs. Time
## Quadrant(1,2) - Voltage vs. Time
## Quadrant(2,1) - Energy sub metering_(1,2,3) vs. Time
## Quadrant(2,2) - Global_reactive_power
## Datasource: UC Irvine Machine Learning Repository (Modified by R. Peng)
## https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip

## Retrieve a dataset from a specificed URL, store it in the destination specified by dest.  
##Also handles ZIP Files
getdata <- function(fileUrl, dest, zipped=FALSE) {
    ## param FileURL - specifies the URL of the file to download
    ## param dest - specifies the destination to store the file
    ## param zipped - specifies whether or not the zipped file is a zip file to unzip
  
    ##Check to see if the file to download has already been downloaded, if not, then download it
    if (!file.exists(dest)) {
      ## create a data diretory to store the file
      dir.create("data")
      
      ## Download the file
      download.file(fileUrl, destfile = dest, method = "curl") 
      
      ## If it is a zipped file, unzip it
      if (zipped) {
        unzip(dest, exdir="./data")
      }
    } else {
      ## File has already been downloaded, let the user know you are using the stored copy
      message("Data already downloaded. Using stored Copy")
    }
}

## Load the data into a datatable using the "data.table" package
loaddata <- function(){
  ## Load the necessary "data.table" package
  library(data.table)
  
  ## Download the file from the website using URL provided on the coursera website
  fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
  getdata(fileUrl, "./data/household_power_consumption.zip", TRUE)
  
  ##Read in the text file
  dtMeter <- fread("./data/household_power_consumption.txt", na.strings = "?", header=TRUE)
  
  ## Convert dates from chracters to date class
  dtMeter$Date <- as.Date(dtMeter$Date, "%d/%m/%Y")
  dtMeter <- dtMeter[dtMeter$Date %between% c("2007-02-01","2007-02-02")]
  
  ## Convert times from characters to POSIXct class
  dtMeter$Time <- as.POSIXct(strptime(paste(dtMeter$Date,dtMeter$Time), "%Y-%m-%d %H:%M:%S"))
  
  ## Convert all other values to numerics
  dtMeter$Global_active_power <- as.numeric(dtMeter$Global_active_power)
  dtMeter$Global_reactive_power <- as.numeric(dtMeter$Global_reactive_power)
  dtMeter$Voltage <- as.numeric(dtMeter$Voltage)
  dtMeter$Global_intensity <- as.numeric(dtMeter$Global_intensity)
  dtMeter$Sub_metering_1 <- as.numeric(dtMeter$Sub_metering_1)
  dtMeter$Sub_metering_2 <- as.numeric(dtMeter$Sub_metering_2)
  dtMeter$Sub_metering_3 <- as.numeric(dtMeter$Sub_metering_3)
  dtMeter
}

## Generates XY-Line plot of Global_active_power against Time
plot_global_active_power_vs_time <- function(dtMeter) {
  with(dtMeter, plot(dtMeter$Time, dtMeter$Global_active_power,
                     main = "",
                     ylab = "Global Active Power",
                     xlab = "",
                     type = "l"))
}

## Generates XY-Line plot of Voltage against time
plot_voltage_vs_time <- function(dtMeter){
  with(dtMeter, plot(dtMeter$Time, dtMeter$Voltage,
                     type = "l",
                     xlab = "datetime",
                     ylab = "Voltage"))
}

## Generates XY-Line plot of All 3 Energy Sub Metering data sets against time 
plot_sub_metering_vs_time <- function(dtMeter) {
  ## Setup the initial plot of Sub_metering_1 against time and set the labels
  with(dtMeter, plot(dtMeter$Time, dtMeter$Sub_metering_1, 
                     type = "l", 
                     xlab = "", 
                     ylab = "Energy sub metering"))
  
  ## Add an additional red line plotting Sub_metering_2 against time
  lines(dtMeter$Time, dtMeter$Sub_metering_2, type = "l", col ="red")
  
  ## Add an additional blue line plotting Sub_metering_2 aginast time
  lines(dtMeter$Time, dtMeter$Sub_metering_3, type = "l", col ="blue")
  
  ## Add a legend for each of the variables, with appropriate labels and matching colors
  legend("topright",
         legend = c("Sub_metering_1","Sub_metering_2", "Sub_metering_3"), 
         lty = 1, 
         col = c("black","red","blue"),
         bty = "n")
  
}

## Generates XY-Line plot of Global_reactive_power against time
plot_global_reactive_power_vs_time<- function(dtMeter) {
  with(dtMeter, plot(dtMeter$Time, dtMeter$Global_reactive_power, 
                     type = "l",
                     xlab = "datetime",
                     ylab = "Global_reactive_power"))  
}

## Generates a 4-Quadrant plot of metering data
## Quardant(1,1) - Global Active Power vs. Time
## Quadrant(1,2) - Voltage vs. Time
## Quadrant(2,1) - Energy sub metering_(1,2,3) vs. Time
## Quadrant(2,2) - Global_reactive_power
genplot4 <- function(dtMeter){
  ## Setup a 2x2 grid for the 4-quadrant plot
  par(mfrow = c(2,2))
  ## Plot each of the 4 plots off 
  with(dtMeter, {
    plot_global_active_power_vs_time(dtMeter)
    plot_voltage_vs_time(dtMeter)
    plot_sub_metering_vs_time(dtMeter)
    plot_global_reactive_power_vs_time(dtMeter)
  })
}

## Complete function to retreive the data, generate the plot and store it in a PNG file
plot4 <- function(toPNG = TRUE){
  ## Parameter toPNG is a boolean which determines whether the output will be directed to the PNG device
  
  ## Load the dataset
  dtMeter <- loaddata()
  
  ## If the output should be directed to the PNG device, then open up the device with the correct dimensions
  if(toPNG) { png(file = "plot4.png", bg="transparent", width = 480, height = 480, units ="px")}
  
  ## Generate the Plot
  genplot4(dtMeter)
  
  ## Close the PNG device to save the file
  if(toPNG) { dev.off() } 
  
}

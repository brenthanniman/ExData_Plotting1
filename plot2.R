### Global Active Power vs. Time in Kilowatts
## ---------
## The following code generates a plot of Global Active Power over Time from metering data 
## from the UC Irvine Machine Learning Repository
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
  
  ## Subsets the data to the dates under study
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
                     ylab = "Global Active Power (kilowatts)",
                     xlab = "",
                     type = "l"))
}


## Complete function to retreive the data, generate the plot and store it in a PNG file
plot2 <- function(toPNG = TRUE){
  ## Parameter toPNG is a boolean which determines whether the output will be directed to the PNG device
  
  ## Load the dataset
  dtMeter <- loaddata()
  
  ## If the output should be directed to the PNG device, then open up the device with the correct dimensions
  if(toPNG) { png(file = "plot2.png", bg="transparent", width = 480, height = 480, units ="px")}
  
  ## Generate the Plot
  plot_global_active_power_vs_time(dtMeter)
  
  ## Close the PNG device to save the file
  if(toPNG) { dev.off() } 
  
}

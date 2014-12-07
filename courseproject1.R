#downloading the file used for the purposes of this assignment
if(!file.exists("exdata-data-household_power_consumption.zip")) {
        #storing the download into a temporary file on the system
        temp <- tempfile()
        #downloading the actual file by pointing R to the file URL
        download.file("http://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",temp)
        #unzipping the file
        file <- unzip(temp)
        unlink(temp)
}
#reading the data with R
power <- read.table(file, header=T, sep=";")
#formatting the date of the data
power$Date <- as.Date(power$Date, format="%d/%m/%Y")
#specifying the certain time periods used in this assignment
df <- power[(power$Date=="2007-02-01") | (power$Date=="2007-02-02"),]
df$Global_active_power <- as.numeric(as.character(df$Global_active_power))
df$Global_reactive_power <- as.numeric(as.character(df$Global_reactive_power))
df$Voltage <- as.numeric(as.character(df$Voltage))
#formatting the time 
df <- transform(df, timestamp=as.POSIXct(paste(Date, Time)), "%d/%m/%Y %H:%M:%S")
#specifying which columns of interest are neededed and storing them in a data frame
df$Sub_metering_1 <- as.numeric(as.character(df$Sub_metering_1))
df$Sub_metering_2 <- as.numeric(as.character(df$Sub_metering_2))
df$Sub_metering_3 <- as.numeric(as.character(df$Sub_metering_3))

#plotting the data, changing certain variables such as the title to Global Active
#Power and changing the color of the ploat to red. Specying the width and height
#of the plot as well. Plotting Global Active Power against frequency. Saves the image
#in a file called Plot1.png in the active working directory
plot1 <- function() {
        hist(df$Global_active_power, main = paste("Global Active Power"), col="red", xlab="Global Active Power (kilowatts)")
        dev.copy(png, file="plot1.png", width=480, height=480)
        dev.off()
        cat("Plot1.png has been saved in", getwd())
}
plot1()

#creating plot 2 and setting the variables. Creating a plot that maps Global active power against
#specific days of the week and saving the image in a file called Plot2.png
plot2 <- function() {
        plot(df$timestamp,df$Global_active_power, type="l", xlab="", ylab="Global Active Power (kilowatts)")
        dev.copy(png, file="plot2.png", width=480, height=480)
        dev.off()
        cat("plot2.png has been saved in", getwd())
}
plot2()

#plotting the data and adjusting the various variables such as color, width and height. This plot
#maps the daata Energy sub metering against the day of the week and saves the resulting plot in a 
#file within the working directory called plot3.png
plot3 <- function() {
        plot(df$timestamp,df$Sub_metering_1, type="l", xlab="", ylab="Energy sub metering")
        lines(df$timestamp,df$Sub_metering_2,col="red")
        lines(df$timestamp,df$Sub_metering_3,col="blue")
        legend("topright", col=c("black","red","blue"), c("Sub_metering_1  ","Sub_metering_2  ", "Sub_metering_3  "),lty=c(1,1), lwd=c(1,1))
        dev.copy(png, file="plot3.png", width=480, height=480)
        dev.off()
        cat("plot3.png has been saved in", getwd())
}
plot3()

#creating four smaller plot graphics that inlude Global Acitve power against the day of the week, 
#Voltage against the day of the week, Energy sub metering against a specified date and time,
#and global reactive power against specified date and time periods. The function also
#sets certain characteristics of the plot, such as color and which data inputs to use.
plot4 <- function() {
        par(mfrow=c(2,2))
        
        #plot 1 which maps Global Active Power against specified date and time stamps
        plot(df$timestamp,df$Global_active_power, type="l", xlab="", ylab="Global Active Power")
        #Plot 2 maps voltage data against specified date and time stamp from the dataframe
        plot(df$timestamp,df$Voltage, type="l", xlab="datetime", ylab="Voltage")
        
        #plot3 maps Energy sub metering data against the date and time data frame
        plot(df$timestamp,df$Sub_metering_1, type="l", xlab="", ylab="Energy sub metering")
        lines(df$timestamp,df$Sub_metering_2,col="red")
        lines(df$timestamp,df$Sub_metering_3,col="blue")
        legend("topright", col=c("black","red","blue"), c("Sub_metering_1  ","Sub_metering_2  ", "Sub_metering_3  "),lty=c(1,1), bty="n", cex=.5) #bty removes the box, cex shrinks the text, spacing added after labels so it renders correctly
        
        #plot 4 maps Global Reactive Power against the specified date and time dataframe
        plot(df$timestamp,df$Global_reactive_power, type="l", xlab="datetime", ylab="Global_reactive_power")
        
        #saves the plots into a file called Plot4.png in the working directory folder
        dev.copy(png, file="plot4.png", width=480, height=480)
        dev.off()
        cat("plot4.png has been saved in", getwd())
}
plot4()



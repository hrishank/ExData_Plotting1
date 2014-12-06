############################################################################################
############################Module 1: Download and extract##################################

extractFunc <- function(){
temp <- tempdir()
tf = tempfile(tmpdir=temp, fileext=".zip")
download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", tf) 
fname = unzip(tf, list=TRUE)$Name[1] 
unzip(tf, files=fname, exdir=temp, overwrite=TRUE)
fpath = file.path(temp, fname)
d = read.table(fpath, sep = ";", header = TRUE, fill = FALSE, strip.white = TRUE)
unlink(temp)
unlink(tf)
return (d)
}

############################################################################################


############################################################################################
###########################Module 2: Make data plot friendly ###############################

dataForPlot <- function() {
#Extract the data
data <- extractFunc()
#Convert date and time to a datetime format
data$DateTime <- strptime(paste(data$Date, data$Time), "%d/%m/%Y %H:%M:%S")
#Create a lean data set with only the two dates in question
data_lean <- subset(data, data$DateTime >= "2007-02-01 00:00:00" & data$DateTime <= "2007-02-03 00:00:00")
#Get global active power in kilowatts and change voltage and sub_meter columns to be numeric 
data_lean$Global_active_power <- as.numeric(as.character(data_lean$Global_active_power))
data_lean$gap_kw <- data_lean$Global_active_power/1000.0
data_lean$Voltage <- as.numeric(as.character(data_lean$Voltage))
data_lean$Sub_metering_1 <- as.numeric(as.character(data_lean$Sub_metering_1))
data_lean$Sub_metering_2 <- as.numeric(as.character(data_lean$Sub_metering_2))
data_lean$Sub_metering_3 <- as.numeric(as.character(data_lean$Sub_metering_3))
data_lean$Global_reactive_power <- as.numeric(as.character(data_lean$Global_reactive_power))

return (data_lean)
}

############################################################################################


############################################################################################
#################################Module 3: Plot graphs #####################################

data_lean <- dataForPlot()

#PLOT 3: 

if (dev.cur() == 1) {dev.new()}
png("plot3.png", width = 480, height = 480, bg = "transparent")
par(mfrow = c(1,1))

with(data_lean, 
     plot(data_lean$DateTime, data_lean$Sub_metering_1, type="n", 
          xlab = "", ylab = "Energy sub metering"))

lines(data_lean$DateTime, data_lean$Sub_metering_1, type = "l", col = "black")
lines(data_lean$DateTime, data_lean$Sub_metering_2, type = "l", col = "red")
lines(data_lean$DateTime, data_lean$Sub_metering_3, type = "l", col = "blue")

legend("topright", 
       c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
       lty = c(1,1), lwd = c(1,1), 
       col = c("black", "red", "blue"))#, cex = 0.67))
dev.off()

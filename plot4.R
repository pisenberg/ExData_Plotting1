
#loads the subset of the data for Feb 1 and 2, 2007
loadData <- function() {
  energyData <- read.table("household_power_consumption.txt",sep=";",header=FALSE,na.strings="?",skip=66637,nrows=2880,colClasses=c("character","character","numeric","numeric","numeric","numeric","numeric","numeric"))
  
  colnames(energyData) <- c("Date","Time","Global_active_power","Global_reactive_power","Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3")
  
  #convert the data and time strings
  dateList <- apply(energyData[,c('Date','Time')],1,function(x) convertDate(x[1],x[2]))

  energyData$DateTime=dateList
  energyData
}


#converts the date and time to a datetime object
#not necessary for this plot
convertDate<-function(dateString,timeString){

  
  fullString <- paste(dateString,timeString,sep=" ")
 
  date = strptime(fullString,format="%d/%m/%Y %H:%M:%S")
 
  date
}

makePlot1 <- function(data){
  x_range <- range(0, length(data$Global_active_power))
  #print(x_range)
  plot(data$Global_active_power,type="l",col="black",main="",ylab="Global Active Power (kilowatts)",xlab="",axes=FALSE)
  
  # Make x axis using day labels
  xaxislabels <- c("Thu","Fri","Sat")
  short = c(0,x_range[2]/2,x_range[2])
  
  axis(1, at=short, lab=xaxislabels)
  
  # Calculate range from 0 to max value of cars and trucks
  g_range <- range(0, data$Global_active_power)
  
  # create a default y axis
  axis(2)
  
  # Create box around plot
  box(col="black")
}

makePlot2 <- function(data){
  x_range <- range(0, length(data$Voltage))
  #print(x_range)
  plot(data$Voltage,type="l",col="black",main="",ylab="Voltage",xlab="datetime",axes=FALSE)
  
  # Make x axis using day labels
  xaxislabels <- c("Thu","Fri","Sat")
  short = c(0,x_range[2]/2,x_range[2])
  axis(1, at=short, lab=xaxislabels)
  
  # create a default y axis
  axis(2)
  
  # Create box around plot
  box(col="black")
}

makePlot3 <- function(data){
  x_range <- range(0, length(data$Date))
  plot(data$Sub_metering_1,type="l",col="black",main="",ylab="Energy sub metering",xlab="",axes=FALSE)
  
  
  
  #add two more data vectors to the plot
  lines(data$Sub_metering_2,type="l",col="red")
  lines(data$Sub_metering_3,type="l",col="blue")
  
  
  # Make x axis using day labels
  xaxislabels <- c("Thu","Fri","Sat")
  short = c(0,x_range[2]/2,x_range[2])
  axis(1, at=short, lab=xaxislabels)
  
  
  # Make y axis with default labels and tickmarks
  axis(2)
  
  par(col="black")
  

  
  #add the legend
  legend("topright",box.lty=0,legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),col=c("black","red","blue"),lty=1)
  
  # Create box around plot
  box()
}

makePlot4 <- function(data){
  x_range <- range(0, length(data$Voltage))
  #print(x_range)
  plot(data$Global_reactive_power,type="l",col="black",main="",ylab="Global_reactive_power",xlab="datetime",axes=FALSE)
  
  # Make x axis using day labels
  xaxislabels <- c("Thu","Fri","Sat")
  short = c(0,x_range[2]/2,x_range[2])
  axis(1, at=short, lab=xaxislabels)
  
  # create a default y axis
  axis(2)
  
  # Create box around plot
  box(col="black")
}

#this function creates the plot
makePlot <- function(data){
  
  par(mfrow = c(2,2))
  
  makePlot1(data)
  makePlot2(data)
  makePlot3(data)
  makePlot4(data)
}

#produces the png with the plot
makePng <- function(data){
  png(file="plot4.png")
  makePlot(data)
  dev.off()
}

#call this function to run the script
main <- function(){
  data <- loadData()
  makePlot(data)
  makePng(data)
}
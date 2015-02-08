
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

#this function creates the histogram
makePlot <- function(data){
  hist(data$Global_active_power,col="red",main="Global Active Power",xlab="Global Active Power (kilowatts)")
  
}

#produces the png with the plot
makePng <- function(data){
  png(file="plot1.png")
  makePlot(data)
  dev.off()
}

#call this function to run the script
main <- function(){
  data <- loadData()
  makePlot(data)
  makePng(data)
}
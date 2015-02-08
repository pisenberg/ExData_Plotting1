
loadData <- function() {
  energyData <- read.table("household_power_consumption.txt",sep=";",header=FALSE,na.strings="?",skip=66637,nrows=2880,colClasses=c("character","character","numeric","numeric","numeric","numeric","numeric","numeric"))
  
  colnames(energyData) <- c("Date","Time","Global_active_power","Global_reactive_power","Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3")
  
  
  
  dateList <- apply(energyData[,c('Date','Time')],1,function(x) convertDate(x[1],x[2]))
  
  energyData$DateTime=dateList
  
  #print(head(energyData))
  
  energyData
}


convertDate<-function(dateString,timeString){
  
  #print(dateTimeVector)
  
  fullString <- paste(dateString,timeString,sep=" ")
  #print(fullString)
  
  date = strptime(fullString,format="%d/%m/%Y %H:%M:%S")
  #print(date)
  #class(date)
  
  date
}

makePlot <- function(data){
  hist(data$Global_active_power,col="red",main="Global Active Power",xlab="Global Active Power (kilowatts)")
  
}

makePng <- function(data){
  png(file="plot1.png")
  makePlot(data)
  dev.off()
}

main <- function(){
  data <- loadData()
  makePlot(data)
  makePng(data)
}
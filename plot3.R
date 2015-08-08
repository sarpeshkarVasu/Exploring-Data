plot3 <- function(){
  ## For plotting Number 3
  ## read the file
  power.consumption <- read.table("household_power_consumption.txt",header= TRUE,sep=";",na.strings = "?") 
  ## add Date and Time Vector
  dates <- power.consumption$Date
  times <- power.consumption$Time
  dt <- paste(dates,times)
  ## convert to date using striptime
  dated_dt <- strptime(dt,"%d/%m/%Y %H:%M:%S")
  ## add this new column to the data table
  plot2_data <- cbind(power.consumption,dated_dt)
  ## use dplyr functionality
  library(dplyr)
  plot2_start <- tbl_df(plot2_data)
  ## Select the required 2 dates
  plot2_final <- filter(plot2_start,dated_dt >"2007-02-01" ,dated_dt < "2007-02-03")
  ## Start plotting the 3 variables
  with(plot2_final,plot(dated_dt,Sub_metering_1,type="l",xlab="",ylab = "Energy sub metering",col="black"))
  par(new =TRUE)
  with(plot2_final,lines(dated_dt,Sub_metering_2,type="l",col="red"))
  par(new=TRUE)
  with(plot2_final,lines(dated_dt,Sub_metering_3,type="l",col="blue"))
  ## add legend. If you have time and patience play around with values
  legend("topright",c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),lty = c(1,1,1),col=c("black","red","blue")) 
  ## copy to png file
  dev.copy(png, file= "plot3.png",width = 480,height=480 )
  dev.off()
  
  
}
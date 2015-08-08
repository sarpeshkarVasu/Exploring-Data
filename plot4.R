plot4 <- function(){
  ## For plotting Number 4
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
  ## Start plotting the 4 charts in one file
  ## make 2 rows and 2 column grid
  par(mfrow = c(2,2))
  ## Tricky but try the 4 margins
  par(mar = c(4,4.4,1,1))
  par(cex =0.60)
  ## plot left top graph 
  with(plot2_final,plot(dated_dt,Global_active_power,type = "l",xlab="",ylab="Global Active Power"))
  ## plot right top graph
  with(plot2_final,plot(dated_dt,Voltage,type = "l",xlab="datetime"))
  ## plot bottom left graph
  with(plot2_final,plot(dated_dt,Sub_metering_1,type="l",xlab="",ylab = "Energy sub metering",col="black"))
  par(new =TRUE)
  with(plot2_final,lines(dated_dt,Sub_metering_2,type="l",col="red"))
  par(new=TRUE)
  with(plot2_final,lines(dated_dt,Sub_metering_3,type="l",col="blue"))
  legend("topright",c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),text.font = 0,lty = c(1,1,1),col=c("black","red","blue"),bty="n") 
  ## plot bottom right graph
  with(plot2_final,plot(dated_dt,Global_reactive_power,type = "l",xlab="datetime"))
  ## copy to png file.
  dev.copy(png, file = "plot4.png", width =480,height=480)
  dev.off()
  }
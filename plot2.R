plot2 <- function(){
  ## For plotting Number 2
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
  ## plot the graph on the screen and then copy it to the png file.
  with(plot2_final,plot(dated_dt,Global_active_power,xlab="",ylab="Global Active Power (kilowats)",type="l"))
  dev.copy(png, file= "plot2.png",width = 480,height=480 )
  dev.off()
}
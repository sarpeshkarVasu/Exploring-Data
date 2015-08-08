plot1 <- function(){
  ## For plotting Number 1
  ## read the file
  power.consumption <- read.table("household_power_consumption.txt",header= TRUE,sep=";",na.strings = "?") 
  ## add Date and Time Vector
  dates <- power.consumption$Datedw
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
  
  ## plot the histogram on the screen and then copy it to png
  with(final_2dates,hist(Global_active_power,col="red",main="Global Active Power",xlab = "Global active power(kilowatts)"))
  dev.copy(png,file="plot1.png",width =480,height=480) 
  dev.off()
}
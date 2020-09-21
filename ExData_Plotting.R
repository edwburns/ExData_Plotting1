
load_data<-function(infile = 'household_power_consumption.txt')
{
  print("executing load_data function")
  test<-read.table(infile, header=TRUE, sep=";", na.strings="?")
  testTib = as_tibble(test)
  testTib2<-mutate(testTib, dteTime =paste(Date, Time, sep =" "))
  tlow<-as.POSIXct(strptime('1/2/2007 00:00:00', format = "%d/%m/%Y %H:%M:%S"))
  thi<-as.POSIXct(strptime('2/2/2007 23:59:59', format = "%d/%m/%Y %H:%M:%S"))
  testTib3<-filter(testTib2, as.POSIXct(dteTime,format = "%d/%m/%Y %H:%M:%S") >=tlow & as.POSIXct(dteTime,format = "%d/%m/%Y %H:%M:%S")<=thi)
  con<-file('dataTibTbl.txt')
  open(con, 'w')
  write.table(testTib3,con)
  close(con)
  return(testTib3)
}

plot_1<-function(tibTbl)
{
    print("executing plot_1 function")
    png(filename="plot_1.png")
    hist(tibTbl$Global_active_power, main="Global Active Power", xlab="Global Active Power (kilowatts)", col="red")
    dev.off()
  
}
plot_2<-function(tibTbl)
{
  print("executing plot_2 function")
  png(filename="plot_2.png")
  plot(as.POSIXlt(tibTbl$dteTime,format = "%d/%m/%Y %H:%M:%S"),tibTbl$Global_active_power, type="l",xlab=" ", ylab="Global Active Power (kilowatts)")
  dev.off()
}
plot_3<-function(tibTbl)
{
  print("executing plot_3 function")
  png(filename="plot_3.png")
  plot(as.POSIXlt(tibTbl$dteTime,format = "%d/%m/%Y %H:%M:%S"),tibTbl$Sub_metering_1,type="n",xlab=" ", ylab="Energy sub metering")
  points(as.POSIXlt(tibTbl$dteTime,format = "%d/%m/%Y %H:%M:%S"),tibTbl$Sub_metering_2,type="l",col="red")
  points(as.POSIXlt(tibTbl$dteTime,format = "%d/%m/%Y %H:%M:%S"),tibTbl$Sub_metering_3,type="l",col="blue")
  points(as.POSIXlt(tibTbl$dteTime,format = "%d/%m/%Y %H:%M:%S"),tibTbl$Sub_metering_1,type="l",col="black")
  legend("topright", legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col=c("black","red","blue"), lty=1:1, cex=0.8)
  dev.off()
}

plot_4<-function(tibTbl)
{
  print("executing plot_4 function")
  png(filename="plot_4.png")
  par(mfcol=c(2,2))
  plot(as.POSIXlt(tibTbl$dteTime,format = "%d/%m/%Y %H:%M:%S"),tibTbl$Global_active_power, type="l",xlab=" ", ylab="Global Active Power")
  plot(as.POSIXlt(tibTbl$dteTime,format = "%d/%m/%Y %H:%M:%S"),tibTbl$Sub_metering_1,type="n",xlab=" ", ylab="Energy sub metering")
  points(as.POSIXlt(tibTbl$dteTime,format = "%d/%m/%Y %H:%M:%S"),tibTbl$Sub_metering_2,type="l",col="red")
  points(as.POSIXlt(tibTbl$dteTime,format = "%d/%m/%Y %H:%M:%S"),tibTbl$Sub_metering_3,type="l",col="blue")
  points(as.POSIXlt(tibTbl$dteTime,format = "%d/%m/%Y %H:%M:%S"),tibTbl$Sub_metering_1,type="l",col="black")
  legend("topright", legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col=c("black","red","blue"), lty=1:1, cex=0.8)
  plot(as.POSIXlt(tibTbl$dteTime,format = "%d/%m/%Y %H:%M:%S"),tibTbl$Voltage, type="l",xlab="datetime ", ylab="Voltage")
  plot(as.POSIXlt(tibTbl$dteTime,format = "%d/%m/%Y %H:%M:%S"),tibTbl$Global_reactive_power, type="l",xlab="datetime ", ylab="Global_reactive_power")
  
  dev.off()
}
make_Plots<-function()
{
  library(dplyr)
  library(tibble)
  dataTibTbl<-load_data()
  plot_1(dataTibTbl)
  plot_2(dataTibTbl)
  plot_3(dataTibTbl)
  plot_4(dataTibTbl)
}



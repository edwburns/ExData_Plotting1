
load_data<-function(infile = 'household_power_consumption.txt')
{
  library(dplyr)
  test<-read.table(infile, header=TRUE, sep=";", na.strings="?")
  testTib = as_tibble(test)
  testTib2<-mutate(testTib, dteTime =paste(Date, Time, sep =" "))
  tlow<-as.POSIXct(strptime('1/2/2007 00:00:00', format = "%d/%m/%Y %H:%M:%S"))
  thi<-as.POSIXct(strptime('2/2/2007 23:59:59', format = "%d/%m/%Y %H:%M:%S"))
  testTib3<-filter(testTib2, as.POSIXct(dteTime,format = "%d/%m/%Y %H:%M:%S") >=tlow & as.POSIXct(dteTime,format = "%d/%m/%Y %H:%M:%S")<=thi)
  con<-file('checkFilter.txt')
  open(con, 'w')
  write.table(testTib3,con)
  close(con)
  return(testTib3)
}
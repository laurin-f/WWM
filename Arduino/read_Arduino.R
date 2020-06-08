library(stringr)
library(ggplot2)
library(float)
datapfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Daten/Urdaten/Arduino/"

lines<- readLines(paste0(datapfad,"CO2_log.txt"))
starts <- grep("New Log",lines)
ends <- c(starts[-1]-1,length(lines))
ranges<-data.frame(starts+1,ends)
logs <- apply(ranges,1, function(x) lines[x[1]:x[2]])
logs2 <- logs
i <- 30
if(is.list(logs)){
  for(i in seq_along(logs)){
  serial.lines <- logs[[i]]
  serial <- str_split(serial.lines,";",simplify = T)
  colnames(serial)<- serial[1,]
  if(nrow(serial)> 2){
  serial <- as.data.frame(serial[-1,])
  serial$date<- lubridate::ymd_hms(paste(serial$Date,serial$Time))
  num.cols<- grep("(CO|co)2|temp",colnames(serial))
  serial[,num.cols] <- apply(serial[,num.cols],2,function(x) as.numeric(as.character(x)))
  serial$logID <- i
  }
  logs2[[i]] <- serial
  }
  cols <- sapply(logs2,ncol)
  serial <- do.call("rbind",logs2[cols == unique(cols)[1]])
#  serial2 <- do.call("rbind",logs2[cols == unique(cols)[2]])
}else{
  serial.lines <- logs
serial <- str_split(serial.lines,";",simplify = T)
colnames(serial)<- serial[1,]
serial <- as.data.frame(serial[-1,])
serial$date<- lubridate::ymd_hms(paste(serial$Date,serial$Time))
num.cols<- grep("CO2|temp",colnames(serial))
serial[,num.cols] <- apply(serial[,num.cols],2,function(x) as.numeric(as.character(x)))
}

serial$CO2_analog[serial$CO2_analog < 0] <- NA
serial$CO2_dig[serial$CO2_dig < -100] <- NA
ggplot(serial)+geom_line(aes(date,CO2_analog,col="analog"))+
  geom_line(aes(date,CO2_dig,col="digital"))#+facet_wrap(~logID)
ggplot(subset(serial,logID==32))+geom_line(aes(date,CO2_analog,col="analog"))+
  geom_line(aes(date,CO2_dig,col="digital"))#+facet_wrap(~logID)
colnames(serial2)
ggplot(serial2)+geom_line(aes(date,co2_con,col="analog"))#+facet_wrap(~logID)


ggplot(serial)+geom_point(aes(CO2_analog,CO2_dig))
ggplot(serial)+
  geom_line(aes(date,temp))

as.numeric(0x3FBD70A4)
as.numeric(c(0x3F,0xBD,0x70,0xA4))
as.double(0x3FBD70A4)
0x41280000 = 10.50
Rcpp::print_hex(10.4)
sprintf("%.60f",0x3FBD70A4)
binary
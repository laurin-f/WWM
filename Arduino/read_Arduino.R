library(stringr)
library(ggplot2)
datapfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Daten/Urdaten/Arduino/"

lines<- readLines(paste0(datapfad,"CO2_log.txt"))
starts <- grep("New Log",lines)
ends <- c(starts[-1]-1,length(lines))
ranges<-data.frame(starts+1,ends)
logs <- apply(ranges,1, function(x) lines[x[1]:x[2]])

serial.lines <- logs[[length(logs)]]
serial <- str_split(serial.lines,";",simplify = T)
colnames(serial)<- serial[1,]
serial <- as.data.frame(serial[-1,])
serial$date<- lubridate::ymd_hms(paste(serial$Date,serial$Time))
num.cols<- grep("CO2|temp",colnames(serial))
serial[,num.cols] <- apply(serial[,num.cols],2,function(x) as.numeric(as.character(x)))

ggplot(serial)+geom_line(aes(date,CO2_analog))+
  geom_line(aes(date,CO2_dig))
ggplot(serial)+
  geom_line(aes(date,temp))



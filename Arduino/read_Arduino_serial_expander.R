

library(pkg.WWM)
check.packages(c("ggplot2","lubridate","stringr","dplyr"))
datapfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Daten/Urdaten/Arduino/"
csv <- read.csv(paste0(datapfad,"CO2_LOG_25_01_2.TXT"),sep=";",stringsAsFactors = F,na.strings = c("NA","ovf","0.00"))
csv <- csv[!is.na(csv$date),]
csv$date <- ymd_hms(paste(csv$date))

cols <- apply(csv,2, function(x) any(!is.na(x)))

csv <- csv[,cols]
csv$temp_tiefe7<- NA
csv[,11:15] <- csv[10:14]
long <- tidyr::pivot_longer(csv,contains("tiefe"),names_pattern="(CO2|temp)_tiefe(\\d)",names_to = c(".value","tiefe"))

long$temp[long$temp > 100 | long$temp < -30 | long$temp == 0] <- NA
long$CO2 <- as.numeric(long$CO2)
long$CO2[long$CO2 <= 0] <- NA


ggplot(subset(long,date > ymd("2021-01-25")& !is.na(CO2)))+geom_line(aes(date,CO2,col=as.factor(tiefe)))
ggplot(subset(long,date > ymd("20210121")&date < ymd_hm("20210121 1720")))+geom_line(aes(date,CO2,col=as.factor(tiefe)))
ggplot(subset(long,date > ymd("20210122")&date < ymd_hm("20210122 1720")))+geom_line(aes(date,CO2,col=as.factor(tiefe)))
ggplot(long)+geom_line(aes(date,temp,col=as.factor(tiefe)))

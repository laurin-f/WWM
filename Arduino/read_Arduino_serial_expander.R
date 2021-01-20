library(pkg.WWM)
check.packages(c("ggplot2","lubridate","stringr","dplyr"))
datapfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Daten/Urdaten/Arduino/"
csv <- read.csv(paste0(datapfad,"CO2_LOG_20_01_1.TXT"),sep=";",stringsAsFactors = F,na.strings = c("NA","ovf","0.00"))

csv$date <- ymd_hms(paste(csv$date))

cols <- apply(csv,2, function(x) any(!is.na(x)))

csv <- csv[,cols]
long <- tidyr::pivot_longer(csv,contains("tiefe"),names_pattern="(CO2|temp)_tiefe(\\d)",names_to = c(".value","tiefe"))

long$temp[long$temp > 100 | long$temp < -30 | long$temp == 0] <- NA
long$CO2[long$CO2 <= 0] <- NA

ggplot(long)+geom_line(aes(date,CO2,col=as.factor(tiefe)))
ggplot(long)+geom_line(aes(date,temp,col=as.factor(tiefe)))

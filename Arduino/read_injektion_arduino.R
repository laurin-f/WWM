inj_pfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Daten/Urdaten/Injektionsrate_Arduino"

library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr")
check.packages(packages)

files <- list.files(inj_pfad,full.names = T)

data_ls <- lapply(files,read.table,sep=";",header=T,stringsAsFactors = F)
data <- do.call(rbind,data_ls)
data$date <- ymd_hms(data$date)
colnames(data) <- c("date","CO2")

data$CO2 <- as.numeric(data$CO2)
data$CO2[ data$CO2 < 300| data$CO2 > 9000] <- NA


range(data$date,na.rm=T)
daterange <- ymd_h(c("21/04/22 10", "21/04/22 16"))


data <- subset(data, date >= min(daterange) & date <= max(daterange))

ggplot(data)+geom_line(aes(date,CO2))
data$CO2_raw <- data$CO2
injectionrate(data=data)
split_chamber(data)

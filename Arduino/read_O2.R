O2_pfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Daten/Urdaten/O2test"

library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr")
check.packages(packages)

files <- list.files(O2_pfad,full.names = T)
files <- files[9]
data_ls <- lapply(files,read.table,sep=";",header=T,stringsAsFactors = F)
data <- do.call(rbind,data_ls)
data$date <- ymd_hms(data$date)
colnames(data) <- c("date","CO2","O2","temp")
data$O2[data$O2 > 30 | data$O2 < 15] <- NA
data$CO2 <- as.numeric(data$CO2)
data$CO2[ data$CO2 < 300| data$CO2 > 5000] <- NA

range(data$date,na.rm=T)

daterange <- ymd_h(c("21/04/22 11", "21/04/22 16"))
colnames(data)
data <- subset(data, date >= min(daterange) & date <= max(daterange))
data$temp <- as.numeric(data$temp)
colnames(data)
ggplot(data)+
  geom_line(aes(date,temp))
ggplot(data)+
  geom_line(aes(date,O2,col="O2"))

ggplot(data)+
  geom_line(aes(date,as.numeric(CO2),col="CO2"))


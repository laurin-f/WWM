O2_pfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Daten/Urdaten/O2test"

library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr")
check.packages(packages)

all_files <- list.files(O2_pfad,full.names = T)
#files <- all_files[9]
#files <- all_files[grep("211020",all_files)]
files <- all_files[grep("211028",all_files)]
data_ls <- lapply(files,read.table,sep=";",header=T,stringsAsFactors = F,na.strings=c("NA","","ovf"))
data <- do.call(rbind,data_ls)
data$date <- ymd_hms(data$date)
colnames(data) <- c("date","CO2","O2","temp")
#data$O2[data$O2 > 30 | data$O2 < 15] <- NA
data$CO2 <- as.numeric(data$CO2)
data$CO2[ data$CO2 < 300| data$CO2 > 5000] <- NA

range(data$date,na.rm=T)

#daterange <- ymd_h(c("21/04/22 11", "21/04/22 16"))
#colnames(data)
#data <- subset(data, date >= min(daterange) & date <= max(daterange))
data$temp <- as.numeric(data$temp)
data$temp[data$temp<1 | data$temp > 50] <- NA
#colnames(data)
ggplot(data)+
  geom_line(aes(date,temp))
ggplot(data)+
  geom_point(aes(date,O2,col="O2"))+
  geom_line(aes(date,O2,col="O2"))

ggplot(data)+
  geom_line(aes(date,as.numeric(CO2),col="CO2"))


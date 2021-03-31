O2_pfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Daten/Urdaten/O2test"

library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr")
check.packages(packages)

files <- list.files(O2_pfad,full.names = T)
data_ls <- lapply(files[1],read.table,sep=";",header=T)
data <- do.call(rbind,data_ls)
data$date <- ymd_hms(data$date)
colnames(data) <- c("date","CO2","O2","T")
data$O2[data$O2 > 30 | data$O2 < 15] <- NA
data$CO2[ data$CO2 < 300| data$CO2 > 5000] <- NA



daterange <- ymd_hms(c("21/03/23 15:38:12", "21/03/23 20:24:41"))
data$dup <- duplicated(data$date)
data <- subset(data, date >= min(daterange) & date <= max(daterange) & dup)

ggplot(data)+
  geom_line(aes(date,O2,col=dup))+
  geom_line(aes(date,CO2/70,col="CO2"))+
  scale_y_continuous(sec.axis = sec_axis(trans=~.*70,name="CO2"))


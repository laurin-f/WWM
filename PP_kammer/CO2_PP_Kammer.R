#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
datapfad_CO2_PP<- paste0(hauptpfad,"Daten/Urdaten/CO2_PP_Kammer/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr")
check.packages(packages)

files <- list.files(datapfad_CO2_PP,full.names = T)


data_ls <- lapply(files,read.table,sep=";",header=T,stringsAsFactors = F,na.strings=c("NA","","ovf"))
data <- do.call(rbind,data_ls)
colnames(data) <- c("date","CO2","O2","temp")
data$date <- ymd_hms(data$date)
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
  geom_line(aes(date,CO2))
ggplot(data)+
  geom_line(aes(date,O2))

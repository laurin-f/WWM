O2_pfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Daten/Urdaten/O2test"

library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr")
check.packages(packages)

files <- list.files(O2_pfad,full.names = T)
files <- files[grep("211019",files)]

data_ls <- lapply(files,read.table,sep=";",header=T,stringsAsFactors = F)

treats <- str_extract(files,"(?<=_)[A-Za-z1-9]+(?=.TXT$)")
for(i in seq_along(data_ls)){
  data_ls[[i]]$treat <- treats[i]
}
data <- do.call(rbind,data_ls)
data$date <- ymd_hms(data$date)
colnames(data) <- c("date","CO2","O2","temp","treat")
#data$O2[data$O2 > 30 | data$O2 < 15] <- NA
data$CO2 <- as.numeric(data$CO2)
data$CO2[ data$CO2 < 300| data$CO2 > 5000] <- NA


data$temp <- as.numeric(data$temp)
data$temp[data$temp<1] <- NA
ggplot(subset(data,treat == "normal"))+
  geom_point(aes(date,O2,col=treat))+
  geom_line(aes(date,O2,col=treat))
ggplot(subset(data,treat %in% c("normal","oversampling")))+
  geom_point(aes(date,O2,col=treat))+
  geom_line(aes(date,O2,col=treat))+labs(y="O2 [V]")

sort(unique(abs(diff(subset(data,treat == "normal")$O2))))[2]

5/2^10#10 bit resolution normal
sort(unique(abs(diff(subset(data,treat == "oversampling")$O2))))[2]
5/2^13#13 bit resolution with oversampling

unique(data$treat)
ggplot(data)+
  geom_point(aes(date,temp,col=treat))

ggplot(subset(data,treat == "KammerGarten"))+
  geom_line(aes(date,as.numeric(CO2),col=treat))

